test_that("backend information is structured", {
    cuda <- cuda_info()
    expect_named(cuda, c(
        "hardware_visible", "driver_library", "driver_initialized",
        "driver_usable", "driver_initialization_status",
        "driver_device_count_status",
        "gpu", "driver", "driver_version", "driver_supported_cuda_version",
        "runtime_library", "runtime_library_path", "runtime_initialized",
        "runtime_usable", "runtime_initialization_status", "runtime",
        "runtime_version", "runtime_version_status", "toolkit_available",
        "toolkit", "toolkit_root", "toolkit_version", "compiler_available",
        "compiler_version", "nvcc", "nvcc_path", "libraries", "devices",
        "physical_devices", "visible_device_count", "compute_capability",
        "cuda_architectures", "cuda_visible_devices", "status",
        "diagnostic_code", "reason_codes", "usable"
    ), ignore.order = TRUE)
    expect_type(metal_info(), "list")
    expect_type(rocm_info(), "list")
    expect_type(opencl_info(), "list")
})

test_that("hardware_info and sitrep are safe", {
    info <- hardware_info()
    expect_named(info, c(
        "r", "system", "cpu", "memory", "gpu", "gpu_devices", "gpu_summary",
        "accelerators", "capabilities", "cuda", "nccl",
        "metal", "rocm", "opencl", "available_backends", "best_backend",
        "environment", "validation"
    ))
    expect_silent(returned <- gpu_sitrep())
    expect_s3_class(returned, "gpuinfo_sitrep")
    expect_output(print(returned), "gpuinfo diagnostic report")
    expect_type(returned, "list")
})
