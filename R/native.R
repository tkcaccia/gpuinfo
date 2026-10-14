.gpuinfo_native_cuda <- function() {
    .gpuinfo_observe_once("native_cuda", compute = function() {
        tryCatch(.Call(C_gpuinfo_cuda_probe), error = function(e) list(
            library = FALSE, initialized = FALSE, init_status = NA_integer_,
            count_status = NA_integer_, count = 0L, driver_version = NA_integer_,
            names = character(), memory_bytes = numeric(), memory_mb = numeric(),
            compute_capability_major = integer(),
            compute_capability_minor = integer(), compute_capability = character(),
            uuids = character()
        ))
    })
}

.gpuinfo_native_cudart <- function(candidates) {
    .gpuinfo_observe_once("native_cudart", as.character(candidates), function() {
        tryCatch(.Call(C_gpuinfo_cudart_probe, as.character(candidates)),
            error = function(e) list(
                library = FALSE, library_path = NA_character_,
                version_query_status = NA_integer_, runtime_version = NA_integer_,
                initialized = FALSE, init_status = NA_integer_, device_count = 0L
            ))
    })
}

.gpuinfo_native_library <- function(candidates) {
    .gpuinfo_observe_once("native_library", as.character(candidates), function() {
        tryCatch(.Call(C_gpuinfo_library_probe, as.character(candidates)),
            error = function(e) list(
                loadable = FALSE, library_path = NA_character_
            ))
    })
}

.gpuinfo_native_nccl <- function(candidates) {
    .gpuinfo_observe_once("native_nccl", as.character(candidates), function() {
        tryCatch(.Call(C_gpuinfo_nccl_probe, as.character(candidates)),
            error = function(e) list(
                library = FALSE, library_path = NA_character_,
                version_status = NA_integer_, version_integer = NA_integer_
            ))
    })
}

.gpuinfo_native_opencl <- function() {
    .gpuinfo_observe_once("native_opencl", compute = function() {
        tryCatch(.Call(C_gpuinfo_opencl_probe), error = function(e) list(
            library = FALSE, platform_count = 0L, device_count = 0L,
            devices = character(), vendors = character(), gpu = logical(),
            memory_bytes = numeric(), fp64 = logical()
        ))
    })
}

.gpuinfo_native_metal <- function() {
    .gpuinfo_observe_once("native_metal", compute = function() {
        tryCatch(.Call(C_gpuinfo_metal_probe), error = function(e) list(
            framework = FALSE, device = FALSE,
            allocation_query_supported = FALSE,
            current_allocated_size = NA_real_
        ))
    })
}

.gpuinfo_native_rocm <- function() {
    .gpuinfo_observe_once("native_rocm", compute = function() {
        tryCatch(.Call(C_gpuinfo_rocm_probe), error = function(e) list(
            library = FALSE, initialized = FALSE, count = 0L,
            runtime_version = NA_integer_
        ))
    })
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
