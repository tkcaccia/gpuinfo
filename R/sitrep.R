#' Collect hardware information
#'
#' Creates a single structured snapshot of the CPU, visible GPUs, compute
#' backends, execution environment, and validation evidence. This is the main
#' entry point when more detail than the `has_*()` predicates is required.
#'
#' Detection is read-only and framework-independent. It describes the hardware
#' and system software visible to the current R process; it does not determine
#' whether torch, TensorFlow, or another package was compiled with GPU support.
#' Containers, schedulers, permissions, and device-visibility environment
#' variables can affect the result.
#'
#' @param as Output representation. `"list"` returns native R objects.
#'   `"json"` returns a pretty-printed JSON character scalar and requires the
#'   suggested `jsonlite` package. Missing values become JSON `null` values.
#' @return A nested list containing R, system, CPU, GPU, backend, environment,
#'   and package-validation details when `as = "list"`; otherwise a JSON
#'   character scalar containing the same information.
#'
#' @section Returned components:
#' The report contains the following named components:
#' \describe{
#'   \item{r}{R version and platform.}
#'   \item{system}{Operating-system name, release, version, and architecture.}
#'   \item{cpu}{CPU model, vendor, architecture, logical-core count, and OS.}
#'   \item{gpu}{One row per detected visible GPU, including device id, vendor,
#'     model, reported memory, backend, and NVIDIA compute capability when
#'     known. An empty data frame means that no visible GPU was detected.}
#'   \item{accelerators}{A vendor-neutral accelerator table with memory and
#'     backend-status information.}
#'   \item{capabilities}{Per-device precision, memory, and backend capability
#'     information. Unknown capabilities are represented by `NA`.}
#'   \item{cuda}{NVIDIA GPU, driver, runtime, Toolkit, `nvcc`, versions, and
#'     apparent CUDA usability. The CUDA version advertised by `nvidia-smi` is
#'     a driver-supported maximum, not necessarily an installed Toolkit.}
#'   \item{metal}{macOS platform, Apple silicon, device, framework, support,
#'     and apparent Metal usability.}
#'   \item{rocm}{AMD GPU, kernel driver, runtime, utilities, version, and
#'     apparent ROCm usability.}
#'   \item{opencl}{Loader, platforms, devices, vendors, memory, FP64 support,
#'     and apparent OpenCL availability.}
#'   \item{available_backends}{Detected usable backends, always including the
#'     CPU fallback.}
#'   \item{best_backend}{The first usable backend in the package priority order
#'     CUDA, Metal, ROCm, OpenCL, then CPU.}
#'   \item{environment}{Container, CI, WSL, scheduler, and device-visibility
#'     information. Values are reported without changing the environment.}
#'   \item{validation}{Real-hardware validation evidence shipped with this
#'     package version. This describes maintainer testing, not the current host.}
#' }
#'
#' @examples
#' info <- hardware_info()
#' names(info)
#' info$cpu
#' info$gpu
#' info$cuda$usable
#'
#' if (requireNamespace("jsonlite", quietly = TRUE)) {
#'     json <- hardware_info("json")
#' }
#'
#' @seealso [gpu_sitrep()], [has_gpu()], [has_cuda()], [has_metal()],
#'   [has_rocm()], [has_opencl()]
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
        environment = environment_info(),
        validation = validation_info()
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
#' Prints the most useful fields from [hardware_info()] in a compact diagnostic
#' report suitable for an issue, bug report, or hardware-validation record.
#' Text output includes the R and operating-system versions, CPU, visible GPUs,
#' CUDA components, other backend results, and the selected fallback backend.
#'
#' The function performs the same read-only detection as [hardware_info()]. It
#' does not install software, initialize a framework, change device visibility,
#' or transmit the report. Review the output before sharing it because system
#' and environment details may identify the computer or execution environment.
#'
#' @param format Output format. `"text"` prints a human-readable report.
#'   `"json"` prints the complete JSON representation and requires the
#'   suggested `jsonlite` package.
#' @return For `format = "text"`, the underlying hardware-information list is
#'   returned invisibly. For `format = "json"`, the printed JSON character
#'   scalar is returned invisibly.
#'
#' @examples
#' gpu_sitrep()
#'
#' if (requireNamespace("jsonlite", quietly = TRUE)) {
#'     report_json <- gpu_sitrep("json")
#' }
#'
#' @seealso [hardware_info()]
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
