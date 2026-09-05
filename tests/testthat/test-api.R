test_that("predicates always return scalar logical values", {
    predicates <- list(
        has_gpu, has_cuda, has_nvidia_gpu, has_cuda_driver,
        has_cuda_runtime, has_cuda_toolkit, has_nvcc,
        has_metal, has_rocm, has_opencl
    )
    for (predicate in predicates) {
        value <- predicate()
        expect_type(value, "logical")
        expect_length(value, 1L)
        expect_false(is.na(value))
    }
})

test_that("gpu_info has a stable schema", {
    info <- gpu_info()
    expect_s3_class(info, "data.frame")
    expect_identical(names(info), c(
        "id", "vendor", "model", "memory_mb", "backend", "compute_capability"
    ))
    expect_type(info$id, "integer")
    expect_type(info$memory_mb, "double")
})

test_that("summary helpers are stable", {
    expect_type(gpu_count(), "integer")
    expect_length(gpu_count(), 1L)
    expect_gte(gpu_count(), 0L)
    expect_type(gpu_memory(), "double")
    expect_type(cpu_info(), "list")
})
