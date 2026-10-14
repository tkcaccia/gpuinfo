#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(gpuinfo))

cat("gpuinfo optional real-CUDA validation\n")
cat("timestamp_utc:", format(Sys.time(), tz = "UTC", usetz = TRUE), "\n")
cat("CUDA_VISIBLE_DEVICES:",
    Sys.getenv("CUDA_VISIBLE_DEVICES", unset = "<unset>"), "\n\n")

info <- cuda_info(refresh = TRUE)
devices <- gpu_devices(refresh = TRUE)
memory <- gpu_memory_info(refresh = TRUE)
process_memory <- gpu_memory_sample(refresh = TRUE)
peak_memory <- gpu_memory_peak(list(process_memory),
    devices[devices$process_visible %in% TRUE, , drop = FALSE])
preflight <- cuda_preflight(require = "driver")
nccl <- nccl_info(refresh = TRUE)
libraries <- cuda_libraries(refresh = TRUE)

print(gpu_sitrep())
cat("\nCUDA preflight:\n")
print(preflight)
cat("\nOptional NCCL information:\n")
print(nccl)
cat("\nCUDA shared libraries:\n")
print(libraries, row.names = FALSE)
cat("\nCUDA devices:\n")
print(devices, row.names = FALSE)
cat("\nGPU memory (bytes):\n")
print(memory, row.names = FALSE)
cat("\nCurrent-process GPU memory (bytes):\n")
print(process_memory, row.names = FALSE)
cat("\nObserved process GPU memory peak (bytes):\n")
print(peak_memory, row.names = FALSE)

if (!isTRUE(info$driver_library))
    stop("CUDA driver library was not loaded.", call. = FALSE)
if (!isTRUE(info$driver_initialized))
    stop("CUDA driver initialization failed.", call. = FALSE)
if (!isTRUE(info$driver_usable) || info$visible_device_count < 1L)
    stop("No CUDA device is visible to the current process.", call. = FALSE)

visible <- info$devices[info$devices$process_visible %in% TRUE, , drop = FALSE]
if (anyDuplicated(visible$logical_id))
    stop("Process-visible CUDA logical ids are not unique.", call. = FALSE)
if (any(is.na(visible$compute_capability)))
    warning("At least one visible device has unknown compute capability.",
            call. = FALSE)
if (isTRUE(info$runtime_library) && is.na(info$runtime_version))
    warning("CUDA runtime loaded but cudaRuntimeGetVersion() was unavailable.",
            call. = FALSE)
if (!identical(nccl$visible_cuda_device_count, info$visible_device_count))
    stop("NCCL report and CUDA report disagree on visible device count.",
         call. = FALSE)
if (isTRUE(nccl$library) && !identical(nccl$status, "available"))
    warning("NCCL loaded but its version could not be established.",
            call. = FALSE)

if (isTRUE(info$compiler_available)) {
    build <- cuda_build_info(refresh = TRUE)
    cat("\nCUDA build information:\n")
    print(build)
    if (!isTRUE(build$ok))
        stop("Visible CUDA architecture is not supported by the selected nvcc: ",
             build$reason, call. = FALSE)
} else {
    cat("\nnvcc is unavailable; build-architecture validation skipped.\n")
}

cat("\nresult: VALIDATION PASSED\n")
