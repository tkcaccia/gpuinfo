#' Collect hardware information
#'
#' @param as Return a nested R list or a JSON string. JSON output requires the
#'   suggested `jsonlite` package.
#' @return A nested list containing R, system, CPU, GPU, and backend details.
#' @export
hardware_info <- function(as = c("list", "json")) {
    as <- match.arg(as)
    answer <- list(
        r = list(version = paste(R.version$major, R.version$minor, sep = "."),
                 platform = R.version$platform),
        system = as.list(Sys.info()[c("sysname", "release", "version", "machine")]),
        cpu = cpu_info(),
        gpu = gpu_info(),
        accelerators = accelerator_info(),
        capabilities = gpu_capabilities(),
        cuda = cuda_info(),
        metal = metal_info(),
        rocm = rocm_info(),
        opencl = opencl_info(),
        available_backends = available_backends(),
        best_backend = best_backend(),
        environment = environment_info()
    )
    if (as == "json") {
        if (!requireNamespace("jsonlite", quietly = TRUE))
            stop("Install the suggested package 'jsonlite' for JSON output.", call. = FALSE)
        return(jsonlite::toJSON(answer, auto_unbox = TRUE, pretty = TRUE, na = "null"))
    }
    answer
}

#' Print a GPU diagnostic report
#'
#' Prints a compact, dependency-free report suitable for pasting into an issue.
#'
#' @param format Print text or JSON. JSON output requires the suggested
#'   `jsonlite` package.
#' @return The hardware information list, invisibly.
#' @export
gpu_sitrep <- function(format = c("text", "json")) {
    format <- match.arg(format)
    if (format == "json") {
        value <- hardware_info("json")
        cat(value, "\n")
        return(invisible(value))
    }
    x <- hardware_info()
    gpu <- x$gpu
    cat("gpuinfo diagnostic report\n", "-------------------------\n\n", sep = "")
    cat("R:\n  version: ", x$r$version, "\n  platform: ", x$r$platform, "\n\n", sep = "")
    cat("System:\n  OS: ", x$system$sysname, " ", x$system$release,
        "\n  architecture: ", x$system$machine, "\n", sep = "")
    cat("  CPU: ", ifelse(is.na(x$cpu$model), "unknown", x$cpu$model),
        "\n  logical cores: ", x$cpu$logical_cores, "\n\n", sep = "")
    cat("GPU:\n")
    if (!nrow(gpu)) {
        cat("  none detected\n\n")
    } else {
        for (i in seq_len(nrow(gpu))) {
            memory <- if (is.na(gpu$memory_mb[[i]])) "unknown" else paste0(gpu$memory_mb[[i]], " MiB")
            cat("  [", gpu$id[[i]], "] ", gpu$vendor[[i]], " ", gpu$model[[i]],
                " (memory: ", memory, ")\n", sep = "")
        }
        cat("\n")
    }
    cat("CUDA:\n",
        "  GPU detected: ", .gpuinfo_yesno(x$cuda$gpu), "\n",
        "  driver: ", .gpuinfo_yesno(x$cuda$driver), "\n",
        "  driver version: ", ifelse(is.na(x$cuda$driver_version), "unknown", x$cuda$driver_version), "\n",
        "  local runtime: ", .gpuinfo_yesno(x$cuda$runtime), "\n",
        "  toolkit: ", .gpuinfo_yesno(x$cuda$toolkit), "\n",
        "  nvcc: ", .gpuinfo_yesno(x$cuda$nvcc), "\n",
        "  usable: ", .gpuinfo_yesno(x$cuda$usable), "\n\n", sep = "")
    cat("Metal: ", .gpuinfo_yesno(x$metal$usable), "\n", sep = "")
    cat("ROCm: ", .gpuinfo_yesno(x$rocm$usable), "\n", sep = "")
    cat("OpenCL: ", .gpuinfo_yesno(x$opencl$available), "\n", sep = "")
    cat("Available backends: ", paste(x$available_backends, collapse = ", "), "\n", sep = "")
    cat("Best backend: ", x$best_backend, "\n", sep = "")
    invisible(x)
}
