.cuda_library_specs <- function(os = Sys.info()[["sysname"]]) {
    data.frame(
        component = c("driver", "runtime", "cublas", "cufft", "cusolver",
                      "curand", "nccl"),
        library = c("CUDA driver", "CUDA runtime", "cuBLAS", "cuFFT",
                    "cuSOLVER", "cuRAND", "NCCL"),
        purpose = c(
            "CUDA device discovery and driver API",
            "CUDA runtime API and kernel execution",
            "dense linear algebra",
            "fast Fourier transforms",
            "dense and sparse direct solvers",
            "random-number generation",
            "multi-GPU collective communication"
        ),
        required_for_has_cuda = c(TRUE, rep(FALSE, 6L)),
        supported_platform = c(rep(os %in% c("Linux", "Windows"), 6L),
                               identical(os, "Linux")),
        stringsAsFactors = FALSE
    )
}

.cuda_library_defaults <- function(component, os) {
    if (identical(os, "Windows")) {
        return(switch(component,
            driver = "nvcuda.dll",
            runtime = paste0("cudart64_", c(130:120, 118:110), ".dll"),
            cublas = paste0("cublas64_", 13:11, ".dll"),
            cufft = paste0("cufft64_", 12:10, ".dll"),
            cusolver = paste0("cusolver64_", 12:10, ".dll"),
            curand = paste0("curand64_", 11:10, ".dll"),
            character()
        ))
    }
    if (identical(os, "Darwin")) {
        stem <- switch(component, driver = "cuda", runtime = "cudart",
                       cublas = "cublas", cufft = "cufft",
                       cusolver = "cusolver", curand = "curand", nccl = "nccl")
        return(if (length(stem)) paste0("lib", stem, ".dylib") else character())
    }
    stem <- switch(component, driver = "cuda", runtime = "cudart",
                   cublas = "cublas", cufft = "cufft",
                   cusolver = "cusolver", curand = "curand", nccl = "nccl")
    if (!length(stem)) return(character())
    versions <- switch(component,
        driver = c(".so.1", ".so"),
        runtime = c(".so", ".so.13", ".so.12", ".so.11.0"),
        nccl = c(".so.2", ".so"),
        c(".so", paste0(".so.", 13:10))
    )
    paste0("lib", stem, versions)
}

.cuda_library_candidates <- function(component, static,
                                      os = Sys.info()[["sysname"]]) {
    if (identical(component, "runtime"))
        return(static$runtime_candidates)
    if (identical(component, "nccl"))
        return(.nccl_candidates())
    if (identical(component, "driver")) {
        defaults <- .cuda_library_defaults(component, os)
        if (identical(os, "Linux"))
            defaults <- c(defaults, "/usr/lib/wsl/lib/libcuda.so.1")
        return(unique(defaults))
    }
    stem <- component
    pattern <- if (identical(os, "Windows")) {
        paste0("^", stem, "64_.*\\.dll$")
    } else if (identical(os, "Darwin")) {
        paste0("^lib", stem, ".*\\.dylib$")
    } else {
        paste0("^lib", stem, "\\.so(\\..*)?$")
    }
    files <- unlist(lapply(static$library_dirs, function(dir) {
        list.files(dir, pattern, full.names = TRUE, ignore.case = FALSE)
    }), use.names = FALSE)
    unique(c(files, .cuda_library_defaults(component, os)))
}

.cuda_empty_library_table <- function() {
    data.frame(
        component = character(), library = character(), purpose = character(),
        loadable = logical(), library_path = character(), version = character(),
        required_for_has_cuda = logical(), status = character(),
        reason_code = character(), stringsAsFactors = FALSE
    )
}

#' Inspect NVIDIA CUDA shared libraries
#'
#' Returns one consolidated table describing whether the CUDA driver, CUDA
#' runtime, cuBLAS, cuFFT, cuSOLVER, cuRAND, and NCCL shared libraries can be
#' loaded by the current R process. Loading is immediately reversed and no
#' library handle, CUDA context, communicator, solver, FFT plan, or random
#' generator is created.
#'
#' A loadable shared library is stronger evidence than a matching file name,
#' because dependent-library and process-architecture problems also prevent a
#' successful load. It does not prove that headers are installed, that every
#' symbol required by another package exists, or that an operation will
#' succeed. cuVS and RAFT are deliberately outside this generic CUDA component
#' inventory and should be checked by the package that uses them.
#'
#' Only the CUDA driver contributes to [has_cuda()], together with successful
#' driver initialization and at least one process-visible device. All other
#' rows are informational and optional for ordinary CUDA detection.
#'
#' @param refresh Re-run cached CUDA path discovery.
#' @return A data frame with one row per library component. `loadable` reports
#'   dynamic-loader success, `library_path` gives the loaded candidate,
#'   `version` is populated only when a safe version-only query is available,
#'   and status/reason fields explain unavailable results.
#' @seealso [cuda_info()], [nccl_info()], [has_cuda()]
#' @export
cuda_libraries <- function(refresh = FALSE) {
    static <- .cuda_static_info(refresh = refresh)
    os <- Sys.info()[["sysname"]]
    specs <- .cuda_library_specs(os)
    rows <- lapply(seq_len(nrow(specs)), function(i) {
        component <- specs$component[[i]]
        supported <- specs$supported_platform[[i]]
        candidates <- if (supported) {
            .cuda_library_candidates(component, static, os)
        } else character()
        probe <- if (supported) .gpuinfo_native_library(candidates) else list(
            loadable = FALSE, library_path = NA_character_
        )
        loadable <- isTRUE(probe$loadable)
        version <- NA_character_
        if (identical(component, "nccl") && loadable) {
            nccl <- .gpuinfo_native_nccl(candidates)
            version <- .nccl_version_string(nccl$version_integer)
        }
        reason <- if (!supported) {
            "unsupported_operating_system"
        } else if (!loadable) {
            paste0(component, "_library_not_loadable")
        } else {
            "ok"
        }
        data.frame(
            component = component,
            library = specs$library[[i]],
            purpose = specs$purpose[[i]],
            loadable = loadable,
            library_path = if (loadable) probe$library_path else NA_character_,
            version = version,
            required_for_has_cuda = specs$required_for_has_cuda[[i]],
            status = if (loadable) "available" else "unavailable",
            reason_code = reason,
            stringsAsFactors = FALSE
        )
    })
    answer <- if (length(rows)) do.call(rbind, rows) else .cuda_empty_library_table()
    rownames(answer) <- NULL
    answer
}
