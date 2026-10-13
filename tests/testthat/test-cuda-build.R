mock_build_cuda_info <- function(architectures = integer()) {
    devices <- gpuinfo:::.cuda_empty_devices()
    if (length(architectures)) {
        major <- architectures %/% 10L
        minor <- architectures %% 10L
        devices <- data.frame(
            logical_id = seq_along(architectures) - 1L,
            physical_id = rep(NA_integer_, length(architectures)),
            uuid = paste0("GPU-", seq_along(architectures)),
            vendor = "NVIDIA", model = paste0("GPU ", architectures),
            total_memory_bytes = NA_real_, used_memory_bytes = NA_real_,
            free_memory_bytes = NA_real_,
            compute_capability_major = major,
            compute_capability_minor = minor,
            compute_capability = paste0(major, ".", minor),
            cuda_arch = as.integer(architectures), backend = "cuda",
            detection_source = "cuda-driver",
            visibility_status = "process_visible", process_visible = TRUE,
            stringsAsFactors = FALSE
        )
    }
    list(devices = devices)
}

mock_build_static <- function() {
    list(
        compiler_available = TRUE, nvcc_path = "/mock/nvcc",
        nvcc_version = "13.0", toolkit_available = TRUE,
        toolkit_root = "/mock/cuda", toolkit_version = "13.0",
        include_dirs = "/mock/cuda/include", library_dirs = "/mock/cuda/lib64",
        target_dirs = "/mock/cuda/targets/x86_64-linux",
        runtime_library_files = "/mock/cuda/lib64/libcudart.so"
    )
}

test_that("no visible GPU does not invent an architecture", {
    testthat::local_mocked_bindings(
        cuda_info = function(refresh = FALSE) mock_build_cuda_info(),
        .cuda_static_info = function(refresh = FALSE) mock_build_static(),
        .cuda_nvcc_supported = function(path) list(
            native = c(80L, 90L, 120L), ptx = c(80L, 90L, 120L),
            query_supported = TRUE
        ),
        .package = "gpuinfo"
    )
    build <- cuda_build_info()
    expect_false(build$detected)
    expect_identical(build$reason, "no_visible_cuda_device")
    expect_true(build$requires_explicit_architecture)
    expect_length(build$architectures, 0L)
})

test_that("Blackwell produces sm_120 and compute_120", {
    testthat::local_mocked_bindings(
        cuda_info = function(refresh = FALSE) mock_build_cuda_info(120L),
        .cuda_static_info = function(refresh = FALSE) mock_build_static(),
        .cuda_nvcc_supported = function(path) list(
            native = c(80L, 90L, 100L, 120L),
            ptx = c(80L, 90L, 100L, 120L), query_supported = TRUE
        ),
        .package = "gpuinfo"
    )
    build <- cuda_build_info()
    expect_true(build$ok)
    expect_identical(build$architectures, 120L)
    expect_identical(build$native_architectures, "sm_120")
    expect_identical(build$ptx_architecture, "compute_120")
    expect_true(any(grepl("code=sm_120", build$nvcc_gencode_flags, fixed = TRUE)))
    expect_true(any(grepl("code=compute_120", build$nvcc_gencode_flags, fixed = TRUE)))
})

test_that("nvcc rejects an unsupported detected architecture", {
    testthat::local_mocked_bindings(
        cuda_info = function(refresh = FALSE) mock_build_cuda_info(120L),
        .cuda_static_info = function(refresh = FALSE) mock_build_static(),
        .cuda_nvcc_supported = function(path) list(
            native = c(80L, 90L), ptx = c(80L, 90L), query_supported = TRUE
        ),
        .package = "gpuinfo"
    )
    build <- cuda_build_info()
    expect_false(build$ok)
    expect_identical(build$reason, "architecture_not_supported_by_nvcc")
    expect_identical(build$unsupported_native_architectures, 120L)
})

test_that("explicit architecture overrides normalize consistently", {
    testthat::local_mocked_bindings(
        cuda_info = function(refresh = FALSE) mock_build_cuda_info(),
        .cuda_static_info = function(refresh = FALSE) mock_build_static(),
        .cuda_nvcc_supported = function(path) list(
            native = c(80L, 90L), ptx = c(80L, 90L), query_supported = TRUE
        ),
        .package = "gpuinfo"
    )
    build <- cuda_build_info(c("8.0", "sm_90"), ptx = "compute_90")
    expect_true(build$ok)
    expect_identical(build$architectures, c(80L, 90L))
    expect_identical(build$native_architectures, c("sm_80", "sm_90"))
    expect_identical(build$ptx_architecture, "compute_90")
    expect_false(build$requires_explicit_architecture)
})

test_that("Windows CUDA roots and runtime DLLs are discovered", {
    root <- file.path(tempdir(), paste0("cuda-win-", Sys.getpid()))
    dirs <- c(file.path(root, "include"), file.path(root, "bin"))
    vapply(dirs, dir.create, logical(1L), recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(root, "include", "cuda.h"))
    file.create(file.path(root, "include", "cuda_runtime_api.h"))
    file.create(file.path(root, "bin", "cudart64_130.dll"))
    file.create(file.path(root, "bin", "nvcc.exe"))
    env <- c(CUDA_HOME = root, CUDA_PATH = "", CUDAToolkit_ROOT = "",
             ProgramFiles = "", ProgramW6432 = "")
    static <- gpuinfo:::.cuda_discover_static("Windows", env)
    expect_true(static$toolkit_available)
    expect_true(any(grepl("cudart64_130.dll", static$runtime_candidates,
                         fixed = TRUE)))
    expect_true(grepl("nvcc.exe", static$nvcc_path, fixed = TRUE))
})

test_that("Toolkit-root nvcc is preferred over PATH", {
    root <- file.path(tempdir(), paste0("cuda-priority-", Sys.getpid()))
    dir.create(file.path(root, "bin"), recursive = TRUE, showWarnings = FALSE)
    compiler <- file.path(root, "bin", if (.Platform$OS.type == "windows")
        "nvcc.exe" else "nvcc")
    file.create(compiler)
    expect_identical(
        gpuinfo:::.cuda_nvcc_path(root, Sys.info()[["sysname"]]),
        normalizePath(compiler, winslash = "/", mustWork = FALSE)
    )
})

test_that("Linux target-directory layouts are discovered", {
    root <- file.path(tempdir(), paste0("cuda-linux-", Sys.getpid()))
    target <- file.path(root, "targets", "x86_64-linux")
    dirs <- c(file.path(target, "include"), file.path(target, "lib"))
    vapply(dirs, dir.create, logical(1L), recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(target, "include", "cuda.h"))
    file.create(file.path(target, "include", "cuda_runtime_api.h"))
    file.create(file.path(target, "lib", "libcudart.so.13"))
    env <- c(CUDA_HOME = root, CUDA_PATH = "", CUDAToolkit_ROOT = "")
    static <- gpuinfo:::.cuda_discover_static("Linux", env)
    expect_true(static$toolkit_available)
    expect_true(target %in% static$target_dirs)
    expect_true(any(grepl("libcudart.so.13", static$runtime_library_files,
                         fixed = TRUE)))
})

test_that("a CUDA directory alone is not a Toolkit", {
    root <- file.path(tempdir(), paste0("empty-cuda-", Sys.getpid()))
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    env <- c(CUDA_HOME = root, CUDA_PATH = "", CUDAToolkit_ROOT = "",
             ProgramFiles = "", ProgramW6432 = "")
    # Simulating Windows avoids discovering unrelated /usr/local CUDA roots on
    # real Linux GPU hosts while testing the same completeness rule.
    static <- gpuinfo:::.cuda_discover_static("Windows", env)
    expect_false(static$toolkit_available)
})

test_that("macOS static discovery is safe without CUDA", {
    env <- c(CUDA_HOME = "", CUDA_PATH = "", CUDAToolkit_ROOT = "")
    expect_no_error(static <- gpuinfo:::.cuda_discover_static("Darwin", env))
    expect_false(static$toolkit_available)
})

test_that("preflight returns diagnostics and strict mode can fail", {
    info <- list(
        driver_usable = FALSE, diagnostic_code = "cuda_driver_library_not_found",
        driver_library = FALSE, driver_initialized = FALSE,
        driver_initialization_status = NA_integer_, driver_version = NA_character_,
        visible_device_count = 0L, runtime_usable = FALSE,
        runtime_version_status = "runtime_library_not_found",
        runtime_library = FALSE, runtime_library_path = NA_character_,
        runtime_initialized = FALSE, runtime_initialization_status = NA_integer_,
        runtime_version = NA_character_, toolkit_available = FALSE,
        toolkit_root = NA_character_, toolkit_version = NA_character_,
        compiler_available = FALSE, nvcc_path = NA_character_,
        compiler_version = NA_character_
    )
    build <- list(
        ok = FALSE, reason = "no_visible_cuda_device",
        requires_explicit_architecture = TRUE, native_architectures = character(),
        architectures = integer(), unsupported_native_architectures = integer(),
        unsupported_ptx_architectures = integer(), nvcc_version = NA_character_
    )
    testthat::local_mocked_bindings(
        cuda_info = function(...) info,
        cuda_build_info = function(...) build,
        .package = "gpuinfo"
    )
    preflight <- cuda_preflight()
    expect_s3_class(preflight, "gpuinfo_cuda_preflight")
    expect_false(preflight$ok)
    expect_output(print(preflight), "CUDA preflight: FAILED")
    expect_error(cuda_preflight(strict = TRUE), "driver.*runtime")
})
