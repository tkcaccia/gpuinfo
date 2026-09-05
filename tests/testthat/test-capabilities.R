test_that("accelerator and backend schemas are stable", {
    accelerators <- accelerator_info()
    expect_named(accelerators, c(
        "id", "type", "vendor", "model", "backend", "memory_total_bytes",
        "memory_free_bytes", "status", "reason"
    ))
    backends <- backend_info()
    expect_named(backends, c("backend", "status", "reason", "version", "device_count"))
    expect_setequal(backends$backend, c("cuda", "metal", "rocm", "opencl", "cpu"))
    expect_true(all(backends$status %in% c("available", "unavailable", "unknown")))
})

test_that("capability queries never promote unknown to supported", {
    caps <- gpu_capabilities()
    expect_named(caps, c(
        "id", "backend", "device", "memory_total_bytes", "fp64", "fp32",
        "fp16", "bf16", "int8", "unified_memory"
    ))
    expect_type(supports("gpu"), "logical")
    expect_length(supports("gpu"), 1L)
    expect_error(supports("telepathy"), "Unknown capability")
})

test_that("selection and requirement matching are consistent", {
    expect_identical(select_backend(prefer = "cpu"), "cpu")
    expect_identical(select_backend(require = "fp64", prefer = "cpu"), "cpu")
    expect_true(is.na(select_backend(require = "fp16", prefer = "cpu")))
    check <- check_accelerator(memory = "999TiB")
    expect_s3_class(check, "gpuinfo_check")
    expect_false(check$compatible)
    expect_error(require_accelerator(memory = "999TiB"), "No compatible")
})

test_that("environment reporting is machine readable", {
    expect_type(environment_info(), "list")
    expect_length(visible_devices(), 6L)
    expect_type(gpu_devices(), "integer")
    expect_type(available_gpu_count(), "integer")
    expect_type(physical_gpu_count(), "integer")
})
