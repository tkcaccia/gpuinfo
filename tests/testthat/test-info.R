test_that("backend information is structured", {
    cuda <- cuda_info()
    expect_named(cuda, c(
        "gpu", "driver", "driver_version", "driver_supported_cuda_version",
        "runtime", "runtime_version", "toolkit", "toolkit_version", "nvcc",
        "nvcc_path", "compute_capability", "usable"
    ))
    expect_type(metal_info(), "list")
    expect_type(rocm_info(), "list")
    expect_type(opencl_info(), "list")
})

test_that("hardware_info and sitrep are safe", {
    info <- hardware_info()
    expect_named(info, c(
        "r", "system", "cpu", "gpu", "accelerators", "capabilities", "cuda",
        "metal", "rocm", "opencl", "available_backends", "best_backend",
        "environment"
    ))
    expect_output(returned <- gpu_sitrep(), "gpuinfo diagnostic report")
    expect_type(returned, "list")
})
