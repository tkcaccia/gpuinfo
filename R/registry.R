.gpuinfo_backend_registry <- function() {
    list(
        cuda = list(info = cuda_info, available = has_cuda),
        metal = list(info = metal_info, available = has_metal),
        rocm = list(info = rocm_info, available = has_rocm),
        opencl = list(info = opencl_info, available = has_opencl),
        cpu = list(info = function() list(usable = TRUE), available = has_cpu)
    )
}
