args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !args[[1L]] %in% c("cuda", "metal", "rocm", "opencl")) {
    stop("Usage: Rscript tools/validate-hardware.R <cuda|metal|rocm|opencl>")
}

backend <- args[[1L]]
library(gpuinfo)

cat("gpuinfo real-hardware validation\n")
cat("expected backend:", backend, "\n\n")
print(gpu_sitrep())

report <- hardware_info()
process_memory <- gpu_memory_sample()
manifest <- hardware_manifest(memory = "process", refresh = TRUE)
if (is.na(report$cpu$logical_cores) || report$cpu$logical_cores < 1L)
    stop("CPU logical core detection failed")
if (is.na(report$cpu$available_cores) || report$cpu$available_cores < 1L)
    stop("CPU process-available core detection failed")
if (!is.finite(report$memory$total_memory_bytes) || report$memory$total_memory_bytes <= 0)
    stop("System RAM detection failed")
if (!identical(report$gpu_summary$visible_device_count,
               as.integer(sum(report$gpu_devices$process_visible %in% TRUE))))
    stop("GPU visibility summary does not match the device table")
usable <- switch(backend,
    cuda = has_cuda(), metal = has_metal(),
    rocm = has_rocm(), opencl = has_opencl()
)
if (!usable) stop("Expected backend '", backend, "' was not available")
if (!has_gpu()) stop("No GPU was detected")
if (!nrow(report$accelerators)) stop("No standardized accelerator was returned")

cat("\nCapabilities:\n")
print(report$capabilities, row.names = FALSE)
cat("\nProcess-level memory sample (bytes):\n")
print(process_memory, row.names = FALSE)
cat("\nManifest summary:\n")
print(manifest)
cat("\nVALIDATION PASSED\n")
