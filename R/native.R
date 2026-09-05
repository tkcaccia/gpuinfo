.gpuinfo_native_cuda <- function() {
    tryCatch(.Call(C_gpuinfo_cuda_probe), error = function(e) list(
        library = FALSE, initialized = FALSE, count = 0L,
        driver_version = NA_integer_, names = character(),
        memory_mb = numeric(), compute_capability = character()
    ))
}

.gpuinfo_native_opencl <- function() {
    tryCatch(.Call(C_gpuinfo_opencl_probe), error = function(e) list(
        library = FALSE, platform_count = 0L, device_count = 0L,
        devices = character(), gpu = logical(), memory_bytes = numeric(),
        fp64 = logical()
    ))
}

.gpuinfo_native_metal <- function() {
    tryCatch(.Call(C_gpuinfo_metal_probe), error = function(e) list(
        framework = FALSE, device = FALSE
    ))
}

.gpuinfo_native_rocm <- function() {
    tryCatch(.Call(C_gpuinfo_rocm_probe), error = function(e) list(
        library = FALSE, initialized = FALSE, count = 0L,
        runtime_version = NA_integer_
    ))
}

.gpuinfo_cuda_version <- function(value) {
    if (!length(value) || is.na(value) || value <= 0L) return(NA_character_)
    paste0(value %/% 1000L, ".", (value %% 1000L) %/% 10L)
}

.gpuinfo_hip_version <- function(value) {
    if (!length(value) || is.na(value) || value <= 0L) return(NA_character_)
    if (value < 10000000L) return(.gpuinfo_cuda_version(value))
    major <- value %/% 10000000L
    minor <- (value %% 10000000L) %/% 100000L
    patch <- value %% 100000L
    paste(major, minor, patch, sep = ".")
}
