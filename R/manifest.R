#' Create a reusable hardware manifest
#'
#' Creates a stable, serializable record of the host, process-visible GPU
#' devices, generic compute components, and an optional memory snapshot. The
#' manifest is intended to accompany benchmark results. It contains no FAISS,
#' cuVS, machine-learning-framework, or package-specific capability checks.
#'
#' @param memory Memory observation to include. `"process"` uses
#'   [gpu_memory_sample()], `"device"` uses [gpu_memory_info()], and `"none"`
#'   omits memory values.
#' @param pid Process identifier used when `memory = "process"`.
#' @param refresh Re-run cached static discovery. Dynamic memory is always
#'   sampled again.
#' @param as Output representation. `"list"` returns a `gpuinfo_manifest`
#'   object; `"json"` returns JSON and requires the suggested `jsonlite` package.
#' @return A versioned list or JSON character scalar. Memory units are bytes.
#' @examples
#' manifest <- hardware_manifest(memory = "none")
#' manifest$schema_version
#' manifest$components$cuda$driver_version
#' @seealso [hardware_info()], [gpu_memory_sample()]
#' @export
hardware_manifest <- function(memory = c("process", "device", "none"),
                              pid = Sys.getpid(), refresh = FALSE,
                              as = c("list", "json")) {
    memory <- match.arg(memory)
    as <- match.arg(as)
    if (length(pid) != 1L || is.na(pid) || !is.finite(pid) || pid < 1 ||
        pid > .Machine$integer.max || pid != floor(pid))
        stop("'pid' must be one positive integer.", call. = FALSE)
    pid <- as.integer(pid)
    .gpuinfo_with_observation(function() {
        .gpuinfo_hardware_manifest(memory, pid, refresh, as)
    })
}

.gpuinfo_hardware_manifest <- function(memory, pid, refresh, as) {
    cuda <- cuda_info(refresh = refresh)
    devices <- .gpuinfo_devices_from_cuda(
        cuda$devices, driver_initialized = isTRUE(cuda$driver_initialized)
    )
    cuda_fields <- c(
        "hardware_visible", "driver_library", "driver_initialized",
        "driver_usable", "driver_initialization_status",
        "driver_device_count_status", "driver_version",
        "driver_supported_cuda_version",
        "runtime_library", "runtime_initialized", "runtime_usable",
        "runtime_version", "runtime_version_status", "toolkit_available",
        "toolkit_root", "toolkit_version", "compiler_available", "nvcc_path",
        "compiler_version", "libraries", "status", "diagnostic_code",
        "reason_codes"
    )
    memory_value <- switch(memory,
        process = .gpuinfo_memory_sample_from_devices(
            pid,
            devices[devices$process_visible %in% TRUE, , drop = FALSE]
        ),
        device = .gpuinfo_memory_info_from_devices(devices),
        none = NULL
    )
    answer <- list(
        schema_version = "1.0",
        generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
        package = list(name = "gpuinfo", version = as.character(
            utils::packageVersion("gpuinfo")
        )),
        process = list(pid = as.integer(pid), r_version = R.version.string,
                       r_platform = R.version$platform),
        host = list(system = as.list(Sys.info()), cpu = cpu_info(refresh = refresh),
                    memory = system_memory_info(pid = pid), environment = environment_info()),
        devices = devices,
        device_summary = .gpuinfo_gpu_summary(devices),
        components = list(
            cuda = cuda[cuda_fields],
            nccl = .nccl_info_from_cuda(cuda, refresh = refresh),
            metal = metal_info(),
            rocm = rocm_info(), opencl = opencl_info()
        ),
        memory = list(scope = memory, units = "bytes", sample = memory_value),
        measurement_rules = list(
            cuda_process_memory = paste(
                "nvidia-smi compute-process memory attributed to PID and",
                "reconciled with visible devices by UUID"
            ),
            metal_process_memory = paste(
                "MTLDevice.currentAllocatedSize for the current R process;",
                "not peak, resident, total unified, or system-wide memory"
            )
        )
    )
    if (as == "json") {
        if (!requireNamespace("jsonlite", quietly = TRUE))
            stop("Install the suggested package 'jsonlite' for JSON output.", call. = FALSE)
        return(jsonlite::toJSON(answer, auto_unbox = TRUE, pretty = TRUE, na = "null"))
    }
    class(answer) <- c("gpuinfo_manifest", class(answer))
    answer
}

#' @export
print.gpuinfo_manifest <- function(x, ...) {
    cat("gpuinfo hardware manifest\n", sep = "")
    cat("  schema: ", x$schema_version, "\n", sep = "")
    cat("  generated: ", x$generated_at, "\n", sep = "")
    cat("  devices: ", nrow(x$devices), "\n", sep = "")
    cat("  memory scope: ", x$memory$scope, "\n", sep = "")
    invisible(x)
}
