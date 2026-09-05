args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !args[[1L]] %in% c("cuda", "metal", "rocm", "opencl")) {
    stop("Usage: Rscript tools/validate-hardware.R <cuda|metal|rocm|opencl>")
}

backend <- args[[1L]]
library(gpuinfo)

cat("gpuinfo real-hardware validation\n")
cat("expected backend:", backend, "\n\n")
gpu_sitrep()

info <- backend_info()
row <- info[info$backend == backend, , drop = FALSE]
if (!nrow(row) || row$status[[1L]] != "available") {
    stop("Expected backend '", backend, "' was not available: ",
         if (nrow(row)) row$reason[[1L]] else "missing backend row")
}
if (!has_gpu()) stop("No GPU was detected")
if (!nrow(accelerator_info())) stop("No standardized accelerator was returned")

caps <- gpu_capabilities()
cat("\nCapabilities:\n")
print(caps, row.names = FALSE)
cat("\nVALIDATION PASSED\n")
