args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !args[[1L]] %in% c("cuda", "metal", "rocm", "opencl")) {
    stop("Usage: Rscript tools/validate-hardware.R <cuda|metal|rocm|opencl>")
}

backend <- args[[1L]]
library(gpuinfo)

cat("gpuinfo real-hardware validation\n")
cat("expected backend:", backend, "\n\n")
gpu_sitrep()

report <- hardware_info()
usable <- switch(backend,
    cuda = has_cuda(), metal = has_metal(),
    rocm = has_rocm(), opencl = has_opencl()
)
if (!usable) stop("Expected backend '", backend, "' was not available")
if (!has_gpu()) stop("No GPU was detected")
if (!nrow(report$accelerators)) stop("No standardized accelerator was returned")

cat("\nCapabilities:\n")
print(report$capabilities, row.names = FALSE)
cat("\nVALIDATION PASSED\n")
