.gpuinfo_capability_row <- function(device) {
    backend <- device$backend[[1L]]
    cc <- suppressWarnings(as.numeric(device$compute_capability[[1L]]))
    known_cc <- !is.na(cc)
    opencl_fp64 <- NA
    if (identical(backend, "opencl")) {
        native_opencl <- .gpuinfo_native_opencl()
        hit <- which(native_opencl$devices == device$model[[1L]])
        if (length(hit)) opencl_fp64 <- native_opencl$fp64[[hit[[1L]]]]
    }
    values <- switch(backend,
        cuda = c(fp64 = if (known_cc) cc >= 1.3 else NA,
                 fp32 = TRUE, fp16 = if (known_cc) cc >= 5.3 else NA,
                 bf16 = if (known_cc) cc >= 8.0 else NA,
                 int8 = if (known_cc) cc >= 6.1 else NA,
                 unified_memory = if (known_cc) cc >= 3.0 else NA),
        metal = c(fp64 = FALSE, fp32 = TRUE, fp16 = TRUE, bf16 = NA,
                  int8 = NA, unified_memory = identical(device$vendor[[1L]], "Apple")),
        rocm = c(fp64 = TRUE, fp32 = TRUE, fp16 = TRUE, bf16 = NA,
                 int8 = NA, unified_memory = NA),
        opencl = c(fp64 = opencl_fp64, fp32 = TRUE, fp16 = NA, bf16 = NA,
                   int8 = NA, unified_memory = NA),
        c(fp64 = NA, fp32 = NA, fp16 = NA, bf16 = NA, int8 = NA,
          unified_memory = NA)
    )
    stats::setNames(as.list(as.logical(values)), names(values))
}

#' Report accelerator capabilities
#'
#' Capability values are logical and may be `NA` when the package cannot prove
#' support. Unknown is deliberately not treated as support during selection.
#'
#' @param device Optional integer device id. By default all devices are shown.
#' @return A data frame with one row per device.
#' @export
gpu_capabilities <- function(device = NULL) {
    gpu <- gpu_info()
    if (!is.null(device)) gpu <- gpu[gpu$id %in% as.integer(device), , drop = FALSE]
    columns <- c("id", "backend", "device", "memory_total_bytes", "fp64", "fp32",
                 "fp16", "bf16", "int8", "unified_memory")
    if (!nrow(gpu)) {
        out <- data.frame(id = integer(), backend = character(), device = character(),
            memory_total_bytes = numeric(), fp64 = logical(), fp32 = logical(),
            fp16 = logical(), bf16 = logical(), int8 = logical(),
            unified_memory = logical(), stringsAsFactors = FALSE)
        return(out[columns])
    }
    rows <- lapply(seq_len(nrow(gpu)), function(i) {
        caps <- .gpuinfo_capability_row(gpu[i, , drop = FALSE])
        data.frame(id = gpu$id[[i]], backend = gpu$backend[[i]], device = gpu$model[[i]],
            memory_total_bytes = gpu$memory_mb[[i]] * 1024^2,
            fp64 = caps$fp64, fp32 = caps$fp32, fp16 = caps$fp16,
            bf16 = caps$bf16, int8 = caps$int8,
            unified_memory = caps$unified_memory, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
}

#' Test a backend or numerical capability
#'
#' @param feature One of the backend names or `"gpu"`, `"fp64"`, `"fp32"`,
#'   `"fp16"`, `"bf16"`, `"int8"`, `"unified_memory"`, or `"multi_gpu"`.
#' @param backend Optional backend restriction.
#' @param device Optional integer device id.
#' @return A single non-missing logical value.
#' @export
supports <- function(feature, backend = NULL, device = NULL) {
    if (!is.character(feature) || length(feature) != 1L || is.na(feature))
        stop("`feature` must be one non-missing character value.", call. = FALSE)
    feature <- tolower(feature)
    backends <- c("cuda", "metal", "rocm", "opencl", "cpu")
    if (feature %in% backends) return(feature %in% available_backends())
    if (feature == "gpu") return(has_gpu())
    if (feature == "multi_gpu") return(gpu_count() > 1L)
    allowed <- c("fp64", "fp32", "fp16", "bf16", "int8", "unified_memory")
    if (!feature %in% allowed) stop("Unknown capability: ", feature, ".", call. = FALSE)
    caps <- gpu_capabilities(device)
    if (!is.null(backend)) caps <- caps[caps$backend %in% tolower(backend), , drop = FALSE]
    isTRUE(any(caps[[feature]] %in% TRUE))
}
