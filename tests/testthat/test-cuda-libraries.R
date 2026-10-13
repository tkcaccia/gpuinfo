test_that("generic library probing safely reports a missing library", {
    probe <- gpuinfo:::.gpuinfo_native_library(
        "/gpuinfo/nonexistent/libcuda_component.so")
    expect_false(probe$loadable)
    expect_true(is.na(probe$library_path))
})

test_that("CUDA libraries are returned in one stable table", {
    specs <- gpuinfo:::.cuda_library_specs("Linux")
    local_mocked_bindings(
        .cuda_static_info = function(refresh = FALSE) list(
            runtime_candidates = "runtime", library_dirs = character()
        ),
        .cuda_library_specs = function(os) specs,
        .cuda_library_candidates = function(component, static, os) component,
        .gpuinfo_native_library = function(candidates) list(
            loadable = candidates %in% c("runtime", "cublas", "nccl"),
            library_path = if (candidates %in% c("runtime", "cublas", "nccl"))
                paste0("/mock/", candidates) else NA_character_
        ),
        .gpuinfo_native_nccl = function(candidates) list(
            library = TRUE, library_path = candidates,
            version_status = 0L, version_integer = 22705L
        ),
        .package = "gpuinfo"
    )
    libraries <- cuda_libraries()
    expect_identical(libraries$component, c(
        "driver", "runtime", "cublas", "cufft", "cusolver", "curand", "nccl"
    ))
    expect_named(libraries, c(
        "component", "library", "purpose", "loadable", "library_path",
        "version", "required_for_has_cuda", "status", "reason_code"
    ))
    expect_true(libraries$loadable[libraries$component == "cublas"])
    expect_identical(libraries$version[libraries$component == "nccl"], "2.27.5")
    expect_true(libraries$required_for_has_cuda[libraries$component == "driver"])
    expect_false(any(libraries$component %in% c("cuvs", "raft")))
})

test_that("platform-specific CUDA library names are recognized", {
    linux <- gpuinfo:::.cuda_library_defaults("cublas", "Linux")
    windows <- gpuinfo:::.cuda_library_defaults("curand", "Windows")
    expect_true("libcublas.so" %in% linux)
    expect_true("curand64_10.dll" %in% windows)
})
