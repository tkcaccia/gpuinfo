.cuda_preflight_row <- function(component, ok, required, status, code, message,
                                remediation, evidence) {
    list(
        component = component, component_ok = isTRUE(ok),
        required = isTRUE(required), status = status,
        diagnostic_code = code, message = message,
        remediation = remediation, evidence = evidence
    )
}

#' Diagnose CUDA prerequisites
#'
#' Performs read-only CUDA checks and returns structured diagnostics. It does
#' not install software, alter environment variables, create contexts beyond
#' normal driver/runtime enumeration, or change device visibility.
#'
#' @param require Components that must pass. Supported values are 'driver',
#'   'runtime', 'toolkit', 'compiler', and 'architecture'.
#' @param architectures Architecture request passed to [cuda_build_info()].
#' @param strict If TRUE, fail with an error when a required component fails.
#' @return A gpuinfo_cuda_preflight object. The ok element is the overall
#'   required-check result; remaining fields contain one entry per component.
#' @export
cuda_preflight <- function(require = c("driver", "runtime"),
                           architectures = "auto", strict = FALSE) {
    allowed <- c("driver", "runtime", "toolkit", "compiler", "architecture")
    require <- unique(tolower(as.character(require)))
    invalid <- setdiff(require, allowed)
    if (length(invalid))
        stop("Unknown required CUDA component: ", paste(invalid, collapse = ", "),
             ".", call. = FALSE)
    if (!is.logical(strict) || length(strict) != 1L || is.na(strict))
        stop("strict must be TRUE or FALSE.", call. = FALSE)

    info <- cuda_info()
    build <- cuda_build_info(architectures = architectures)
    required <- function(component) component %in% require
    rows <- list()

    driver_ok <- isTRUE(info$driver_usable)
    driver_code <- if (driver_ok) "driver_usable" else info$diagnostic_code
    rows[[1L]] <- .cuda_preflight_row(
        "driver", driver_ok, required("driver"),
        if (driver_ok) "pass" else "fail", driver_code,
        if (driver_ok) {
            sprintf("CUDA driver initialized with %d visible device(s).",
                    info$visible_device_count)
        } else {
            "No process-visible CUDA device was confirmed through an initialized driver."
        },
        if (driver_ok) NA_character_ else
            "Install or repair the NVIDIA driver, check permissions and CUDA_VISIBLE_DEVICES, then retry.",
        list(library = info$driver_library, initialized = info$driver_initialized,
             initialization_status = info$driver_initialization_status,
             driver_version = info$driver_version,
             visible_device_count = info$visible_device_count)
    )

    runtime_ok <- isTRUE(info$runtime_usable)
    runtime_code <- if (runtime_ok && identical(info$runtime_version_status, "available")) {
        "runtime_usable"
    } else if (runtime_ok) {
        info$runtime_version_status
    } else if (!info$runtime_library) {
        "cuda_runtime_library_not_found"
    } else {
        "cuda_runtime_initialization_failed"
    }
    runtime_status <- if (!runtime_ok) "fail" else
        if (identical(info$runtime_version_status, "available")) "pass" else "warning"
    rows[[2L]] <- .cuda_preflight_row(
        "runtime", runtime_ok, required("runtime"), runtime_status, runtime_code,
        if (!runtime_ok) {
            "CUDA runtime could not be loaded and initialized."
        } else if (!identical(info$runtime_version_status, "available")) {
            "CUDA runtime initialized, but its version could not be established."
        } else {
            paste("CUDA runtime initialized; version", info$runtime_version)
        },
        if (runtime_ok && identical(info$runtime_version_status, "available")) {
            NA_character_
        } else {
            "Provide a compatible libcudart on the loader path or in the CUDA Toolkit directories."
        },
        list(library = info$runtime_library, library_path = info$runtime_library_path,
             initialized = info$runtime_initialized,
             initialization_status = info$runtime_initialization_status,
             version = info$runtime_version,
             version_status = info$runtime_version_status)
    )

    toolkit_ok <- isTRUE(info$toolkit_available)
    rows[[3L]] <- .cuda_preflight_row(
        "toolkit", toolkit_ok, required("toolkit"),
        if (toolkit_ok) "pass" else "fail",
        if (toolkit_ok) "toolkit_available" else "cuda_toolkit_not_found",
        if (toolkit_ok) "CUDA Toolkit headers and supporting compiler/runtime files were found."
        else "A usable CUDA Toolkit layout was not found.",
        if (toolkit_ok) NA_character_ else
            "Set CUDA_HOME or CUDA_PATH to an existing Toolkit containing headers and compiler/runtime files.",
        list(root = info$toolkit_root, version = info$toolkit_version)
    )

    compiler_ok <- isTRUE(info$compiler_available)
    rows[[4L]] <- .cuda_preflight_row(
        "compiler", compiler_ok, required("compiler"),
        if (compiler_ok) "pass" else "fail",
        if (compiler_ok) "nvcc_available" else "nvcc_not_found",
        if (compiler_ok) paste("nvcc found at", info$nvcc_path) else
            "The nvcc compiler was not found.",
        if (compiler_ok) NA_character_ else
            "Install or select a CUDA Toolkit with nvcc, or remove compiler from the required components.",
        list(path = info$nvcc_path, version = info$compiler_version)
    )

    architecture_ok <- isTRUE(build$ok)
    rows[[5L]] <- .cuda_preflight_row(
        "architecture", architecture_ok, required("architecture"),
        if (architecture_ok) "pass" else "fail", build$reason,
        if (architecture_ok) {
            paste("CUDA build architectures:", paste(build$native_architectures,
                                                       collapse = ", "))
        } else if (build$requires_explicit_architecture) {
            "No visible CUDA device supplied an automatic build architecture."
        } else {
            "The requested CUDA architecture set could not be validated."
        },
        if (architecture_ok) NA_character_ else if (build$requires_explicit_architecture)
            "Pass an explicit architecture only when cross-compiling for known target hardware."
        else "Use architectures supported by the selected nvcc or select a compatible Toolkit.",
        list(architectures = build$architectures,
             unsupported_native = build$unsupported_native_architectures,
             unsupported_ptx = build$unsupported_ptx_architectures,
             nvcc_version = build$nvcc_version)
    )

    overall <- all(vapply(rows, function(row) {
        !row$required || row$component_ok
    }, logical(1L)))
    result <- structure(list(
        ok = overall,
        component = vapply(rows, function(row) row[["component"]], character(1L)),
        component_ok = vapply(rows, function(row) row[["component_ok"]], logical(1L)),
        required = vapply(rows, function(row) row[["required"]], logical(1L)),
        status = vapply(rows, function(row) row[["status"]], character(1L)),
        diagnostic_code = vapply(rows, function(row) row[["diagnostic_code"]], character(1L)),
        message = vapply(rows, function(row) row[["message"]], character(1L)),
        remediation = vapply(rows, function(row) row[["remediation"]], character(1L)),
        evidence = lapply(rows, function(row) row[["evidence"]])
    ), class = "gpuinfo_cuda_preflight")
    if (isTRUE(strict) && !overall) {
        failed <- result$component[result$required & !result$component_ok]
        stop("CUDA preflight failed for required component(s): ",
             paste(failed, collapse = ", "), ".", call. = FALSE)
    }
    result
}

#' @export
print.gpuinfo_cuda_preflight <- function(x, ...) {
    cat("CUDA preflight: ", if (isTRUE(x$ok)) "OK" else "FAILED", "\n", sep = "")
    for (i in seq_along(x$component)) {
        marker <- if (x$required[[i]]) "required" else "optional"
        cat("  [", x$status[[i]], "/", marker, "] ", x$component[[i]],
            ": ", x$message[[i]], "\n", sep = "")
    }
    invisible(x)
}
