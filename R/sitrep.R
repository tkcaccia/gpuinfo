#' Collect hardware information
#'
#' Creates a single structured snapshot of the CPU, system memory, visible
#' GPUs, compute backends, execution environment, and validation evidence. This is the main
#' entry point when more detail than the `has_*()` predicates is required.
#'
#' Detection is read-only and framework-independent. It describes the hardware
#' and system software visible to the current R process; it does not determine
#' whether torch, TensorFlow, or another package was compiled with GPU support.
#' Containers, schedulers, permissions, and device-visibility environment
#' variables can affect the result.
#' Repeated commands and native probes are reused within this call to avoid
#' scanning the same hardware several times. The observations are discarded
#' when the call finishes; the next call collects fresh dynamic values.
#'
#' @param as Output representation. `"list"` returns native R objects.
#'   `"json"` returns a pretty-printed JSON character scalar and requires the
#'   suggested `jsonlite` package. Missing values become JSON `null` values.
#' @return A nested list containing R, system, CPU, memory, GPU, backend, environment,
#'   and package-validation details when `as = "list"`; otherwise a JSON
#'   character scalar containing the same information.
#'
#' @section Returned components:
#' The report contains the following named components:
#' \describe{
#'   \item{r}{R version and platform.}
#'   \item{system}{Operating-system name, release, version, and architecture.}
#'   \item{cpu}{CPU identity, topology, process-available cores, features,
#'     detection source, status, and reason codes.}
#'   \item{memory}{Host, effective container, swap, and selected-process memory,
#'     in bytes. Dynamic values are sampled on each call.}
#'   \item{gpu}{One row per detected visible GPU, including device id, vendor,
#'     model, reported memory, backend, and NVIDIA compute capability when
#'     known. This compact table is retained for compatibility.}
#'   \item{gpu_devices}{Detailed process-visible and physical device rows with
#'     UUIDs, visibility, byte-based memory, and CUDA architecture.}
#'   \item{gpu_summary}{Visible and physical device counts plus a multi-GPU
#'     visibility flag. It does not claim package-specific multi-GPU execution.}
#'   \item{accelerators}{A vendor-neutral accelerator table with memory and
#'     backend-status information.}
#'   \item{capabilities}{Per-device precision, memory, and backend capability
#'     information. Unknown capabilities are represented by `NA`.}
#'   \item{cuda}{NVIDIA GPU, driver, runtime, Toolkit, `nvcc`, versions, and
#'     shared-library inventory, plus apparent CUDA usability. The CUDA version
#'     advertised by `nvidia-smi` is a driver-supported maximum, not necessarily
#'     an installed Toolkit.}
#'   \item{nccl}{Optional NCCL shared-library loading and version information,
#'     plus the process-visible CUDA device count. No communicator is created.}
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
#' @seealso [gpu_sitrep()], [has_gpu()], [has_cuda()], [has_metal()],
#'   [has_rocm()], [has_opencl()]
#' @export
hardware_info <- function(as = c("list", "json")) {
    as <- match.arg(as)
    .gpuinfo_with_observation(function() .gpuinfo_hardware_info(as))
}

.gpuinfo_hardware_info <- function(as) {
    devices <- gpu_devices()
    answer <- list(
        r = list(version = paste(R.version$major, R.version$minor, sep = "."),
                 platform = R.version$platform),
        system = as.list(Sys.info()[c("sysname", "release", "version", "machine")]),
        cpu = cpu_info(),
        memory = system_memory_info(),
        gpu = gpu_info(),
        gpu_devices = devices,
        gpu_summary = .gpuinfo_gpu_summary(devices),
        accelerators = accelerator_info(),
        capabilities = gpu_capabilities(),
        cuda = cuda_info(),
        nccl = nccl_info(),
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

.gpuinfo_gpu_summary <- function(devices = gpu_devices()) {
    visible <- if (nrow(devices)) sum(devices$process_visible %in% TRUE) else 0L
    physical <- if (nrow(devices)) {
        keys <- ifelse(!is.na(devices$uuid) & nzchar(devices$uuid), devices$uuid,
                       paste(devices$vendor, devices$model, devices$physical_id, sep = ":"))
        length(unique(keys))
    } else 0L
    list(visible_device_count = as.integer(visible),
         physical_device_count = as.integer(physical),
         multiple_visible_devices = visible > 1L)
}

#' Create a GPU diagnostic report
#'
#' Creates a structured diagnostic report suitable for an issue, bug report,
#' or hardware-validation record. When printed, the report includes the R and
#' operating-system versions, CPU, visible GPUs, CUDA components, other backend
#' results, and the selected fallback backend.
#'
#' The function performs the same read-only detection as [hardware_info()]. It
#' does not install software, initialize a framework, change device visibility,
#' or transmit the report. Review the output before sharing it because system
#' and environment details may identify the computer or execution environment.
#'
#' @param format Format used by the object's [print()] method. `"text"` displays
#'   a human-readable report. `"json"` displays the complete JSON representation
#'   and requires the suggested `jsonlite` package.
#' @return A visible `gpuinfo_sitrep` object containing the structured output of
#'   [hardware_info()]. The object can be inspected with normal list operations;
#'   assigning it to a variable produces no console output.
#'
#' @examples
#' report <- gpu_sitrep()
#' report$cpu$vendor
#' print(report)
#'
#' @seealso [hardware_info()]
#' @export
gpu_sitrep <- function(format = c("text", "json")) {
    format <- match.arg(format)
    x <- hardware_info()
    if (format == "json" && !requireNamespace("jsonlite", quietly = TRUE))
        stop("Install the suggested package 'jsonlite' for JSON output.", call. = FALSE)
    attr(x, "format") <- format
    class(x) <- c("gpuinfo_sitrep", class(x))
    x
}

#' @export
print.gpuinfo_sitrep <- function(x, ...) {
    format <- attr(x, "format", exact = TRUE)
    if (identical(format, "json")) {
        value <- unclass(x)
        attr(value, "format") <- NULL
        cat(jsonlite::toJSON(value, auto_unbox = TRUE, pretty = TRUE, na = "null"), "\n")
        return(invisible(x))
    }
    gpu <- x$gpu
    cat("gpuinfo diagnostic report\n", "-------------------------\n\n", sep = "")
    cat("R:\n  version: ", x$r$version, "\n  platform: ", x$r$platform, "\n\n", sep = "")
    cat("System:\n  OS: ", x$system$sysname, " ", x$system$release,
        "\n  architecture: ", x$system$machine, "\n", sep = "")
    cat("  CPU: ", ifelse(is.na(x$cpu$model), "unknown", x$cpu$model),
        "\n  logical/physical/available cores: ", x$cpu$logical_cores, "/",
        x$cpu$physical_cores, "/", x$cpu$available_cores, "\n", sep = "")
    memory_gib <- x$memory$total_memory_bytes / 1024^3
    cat("  RAM: ", ifelse(is.finite(memory_gib),
                           paste0(format(round(memory_gib, 2), trim = TRUE), " GiB"),
                           "unknown"), "\n\n", sep = "")
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
    cat("NCCL:\n",
        "  library: ", .gpuinfo_yesno(x$nccl$library), "\n",
        "  version: ", ifelse(is.na(x$nccl$version), "unknown", x$nccl$version), "\n",
        "  visible CUDA devices: ", x$nccl$visible_cuda_device_count,
        "\n  communication tested: no\n\n", sep = "")
    cat("CUDA libraries: ", sum(x$cuda$libraries$loadable), "/",
        nrow(x$cuda$libraries), " loadable\n", sep = "")
    cat("Metal: ", .gpuinfo_yesno(x$metal$usable), "\n", sep = "")
    cat("ROCm: ", .gpuinfo_yesno(x$rocm$usable), "\n", sep = "")
    cat("OpenCL: ", .gpuinfo_yesno(x$opencl$available), "\n", sep = "")
    cat("Available backends: ", paste(x$available_backends, collapse = ", "), "\n", sep = "")
    cat("Best backend: ", x$best_backend, "\n", sep = "")
    invisible(x)
}
