.gpuinfo_memory_bytes <- function(x) {
    if (is.null(x) || !length(x)) return(0)
    if (is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 0)
        return(as.numeric(x) * 1024^3)
    if (!is.character(x) || length(x) != 1L || is.na(x))
        stop("Memory must be bytes or a value such as '4GB' or '8GiB'.", call. = FALSE)
    match <- regexec("^\\s*([0-9]+(?:\\.[0-9]+)?)\\s*(B|KB|MB|GB|TB|KIB|MIB|GIB|TIB)\\s*$", toupper(x), perl = TRUE)
    fields <- regmatches(toupper(x), match)[[1L]]
    if (length(fields) != 3L) stop("Could not parse memory requirement: ", x, ".", call. = FALSE)
    multiplier <- c(B = 1, KB = 1e3, MB = 1e6, GB = 1e9, TB = 1e12,
                    KIB = 1024, MIB = 1024^2, GIB = 1024^3, TIB = 1024^4)[[fields[[3L]]]]
    as.numeric(fields[[2L]]) * multiplier
}

.gpuinfo_cpu_supports <- function(require) {
    all(require %in% c("fp64", "fp32", "int8"))
}

.gpuinfo_device_matches <- function(caps, require, min_memory) {
    if (!nrow(caps)) return(logical())
    capability_ok <- rep(TRUE, nrow(caps))
    for (item in require) capability_ok <- capability_ok & !is.na(caps[[item]]) & caps[[item]]
    memory_ok <- if (min_memory <= 0) rep(TRUE, nrow(caps)) else
        !is.na(caps$memory_total_bytes) & caps$memory_total_bytes >= min_memory
    capability_ok & memory_ok
}

#' Select a compute backend
#'
#' @param require Character vector of required numerical capabilities.
#' @param prefer Ordered backend preference.
#' @param min_memory Minimum device memory. Numeric values are GiB; strings may
#'   include units such as `"4GB"` or `"4GiB"`.
#' @return A backend name, or `NA_character_` when no backend satisfies the
#'   requirements. With the default requirements CPU is always selected as a
#'   final fallback.
#' @export
select_backend <- function(require = character(),
                           prefer = c("cuda", "metal", "rocm", "opencl", "cpu"),
                           min_memory = 0) {
    allowed_caps <- c("fp64", "fp32", "fp16", "bf16", "int8", "unified_memory")
    require <- unique(tolower(require))
    if (anyNA(require) || any(!require %in% allowed_caps))
        stop("Unknown required capability: ", paste(setdiff(require, allowed_caps), collapse = ", "), ".", call. = FALSE)
    allowed_backends <- c("cuda", "metal", "rocm", "opencl", "cpu")
    if (!is.character(prefer) || !length(prefer) || anyNA(prefer))
        stop("`prefer` must be a non-empty character vector without missing values.", call. = FALSE)
    prefer <- unique(tolower(prefer))
    invalid <- setdiff(prefer, allowed_backends)
    if (length(invalid)) stop("Unknown backend(s): ", paste(invalid, collapse = ", "), ".", call. = FALSE)
    min_memory <- .gpuinfo_memory_bytes(min_memory)
    available <- available_backends()
    caps <- gpu_capabilities()
    for (candidate in prefer) {
        if (!candidate %in% available) next
        if (candidate == "cpu") {
            if (min_memory <= 0 && .gpuinfo_cpu_supports(require)) return("cpu")
        } else {
            subset <- caps[caps$backend == candidate, , drop = FALSE]
            if (any(.gpuinfo_device_matches(subset, require, min_memory))) return(candidate)
        }
    }
    NA_character_
}

#' Select a compatible accelerator device
#'
#' @param backend Optional backend restriction.
#' @param require Required capabilities.
#' @param min_memory Minimum memory in GiB or as a unit string.
#' @param strategy Either the first compatible device or the one with the most
#'   reported memory.
#' @return A one-row accelerator data frame, or an empty data frame.
#' @export
select_device <- function(backend = NULL, require = character(), min_memory = 0,
                          strategy = c("most_memory", "first")) {
    strategy <- match.arg(strategy)
    devices <- accelerator_info()
    caps <- gpu_capabilities()
    if (!is.null(backend)) {
        backend <- tolower(backend)
        devices <- devices[devices$backend %in% backend, , drop = FALSE]
        caps <- caps[caps$backend %in% backend, , drop = FALSE]
    }
    keep <- .gpuinfo_device_matches(caps, unique(tolower(require)), .gpuinfo_memory_bytes(min_memory))
    caps <- caps[keep, , drop = FALSE]
    devices <- devices[devices$id %in% caps$id & devices$backend %in% caps$backend, , drop = FALSE]
    if (!nrow(devices)) return(devices)
    if (strategy == "most_memory") {
        score <- devices$memory_total_bytes
        score[is.na(score)] <- -Inf
        devices <- devices[order(score, decreasing = TRUE), , drop = FALSE]
    }
    devices[1L, , drop = FALSE]
}

#' Check accelerator requirements
#'
#' @param backend Allowed accelerator backends, or `NULL` for any.
#' @param memory Minimum memory in GiB or as a unit string.
#' @param precision Optional precision requirement such as `"fp64"`.
#' @param require Additional capabilities.
#' @return A `gpuinfo_check` object with compatibility and diagnostic fields.
#' @export
check_accelerator <- function(backend = NULL, memory = 0, precision = NULL,
                              require = character()) {
    requirements <- unique(tolower(c(require, precision)))
    device <- select_device(backend, requirements, memory)
    compatible <- nrow(device) > 0L
    structure(list(
        compatible = compatible,
        device = device,
        requirements = list(backend = backend, memory_bytes = .gpuinfo_memory_bytes(memory),
                            capabilities = requirements),
        reason = if (compatible) "A compatible accelerator was found" else
            "No visible accelerator satisfies every requested capability"
    ), class = "gpuinfo_check")
}

#' @export
print.gpuinfo_check <- function(x, ...) {
    cat(if (x$compatible) "Compatible: YES\n" else "Compatible: NO\n")
    cat(x$reason, "\n")
    if (x$compatible) print(x$device, row.names = FALSE)
    invisible(x)
}

#' Require a compatible accelerator
#'
#' @inheritParams check_accelerator
#' @return The selected device, invisibly, or an error with a consistent
#'   diagnostic message.
#' @export
require_accelerator <- function(backend = NULL, memory = 0, precision = NULL,
                                require = character()) {
    check <- check_accelerator(backend, memory, precision, require)
    if (!check$compatible) {
        requested <- c(if (length(backend)) paste0("backend=", paste(backend, collapse = "/")),
                       if (.gpuinfo_memory_bytes(memory) > 0) paste0("memory>=", memory),
                       check$requirements$capabilities)
        stop("No compatible accelerator was found. Required: ",
             paste(requested, collapse = ", "), ". ", check$reason, ".",
             call. = FALSE)
    }
    invisible(check$device)
}

#' Require a GPU or vendor backend
#' @return `TRUE` invisibly, or an error.
#' @name require_backend
NULL

.gpuinfo_require_backend <- function(backend) {
    ok <- if (backend == "gpu") has_gpu() else supports(backend)
    if (!ok) stop("Required ", toupper(backend), " capability is not available.", call. = FALSE)
    invisible(TRUE)
}

#' @rdname require_backend
#' @export
require_gpu <- function() .gpuinfo_require_backend("gpu")
#' @rdname require_backend
#' @export
require_cuda <- function() .gpuinfo_require_backend("cuda")
#' @rdname require_backend
#' @export
require_metal <- function() .gpuinfo_require_backend("metal")
#' @rdname require_backend
#' @export
require_rocm <- function() .gpuinfo_require_backend("rocm")
