mock_native_cuda <- function(library = FALSE, initialized = FALSE,
                             names = character(), uuids = character(),
                             capabilities = character(),
                             memory_bytes = numeric(), init_status = NA_integer_) {
    n <- length(names)
    parts <- gpuinfo:::.cuda_capability_parts(capabilities)
    list(
        library = library, initialized = initialized,
        init_status = init_status, count_status = if (initialized) 0L else NA_integer_,
        count = as.integer(n), driver_version = if (library) 12060L else NA_integer_,
        names = names, memory_bytes = memory_bytes,
        memory_mb = memory_bytes / 1024^2,
        compute_capability_major = parts$major,
        compute_capability_minor = parts$minor,
        compute_capability = capabilities, uuids = uuids
    )
}

mock_runtime_cuda <- function(library = FALSE, initialized = FALSE,
                              version = NA_integer_) {
    list(
        library = library,
        library_path = if (library) "/mock/libcudart.so" else NA_character_,
        version_query_status = if (library && !is.na(version)) 0L else NA_integer_,
        runtime_version = version, initialized = initialized,
        init_status = if (initialized) 0L else NA_integer_,
        device_count = if (initialized) 1L else 0L
    )
}

mock_static_cuda <- function(toolkit = FALSE, compiler = FALSE) {
    list(
        roots = character(), target_dirs = character(), include_dirs = character(),
        library_dirs = character(), header_available = toolkit,
        runtime_header_available = toolkit,
        runtime_candidates = "libcudart.so", runtime_library_files = character(),
        toolkit_available = toolkit,
        toolkit_root = if (toolkit) "/mock/cuda" else NA_character_,
        toolkit_version = if (toolkit) "13.0" else NA_character_,
        compiler_available = compiler,
        nvcc_path = if (compiler) "/mock/cuda/bin/nvcc" else NA_character_,
        nvcc_version = if (compiler) "13.0" else NA_character_, os = "Linux"
    )
}

mock_physical_cuda <- function(ids = integer(), uuids = character(),
                               models = character(), capabilities = character()) {
    if (!length(ids)) return(gpuinfo:::.cuda_empty_devices())
    parts <- gpuinfo:::.cuda_capability_parts(capabilities)
    data.frame(
        logical_id = rep(NA_integer_, length(ids)), physical_id = as.integer(ids),
        uuid = toupper(uuids), vendor = "NVIDIA", model = models,
        total_memory_bytes = rep(16 * 1024^3, length(ids)),
        used_memory_bytes = rep(2 * 1024^3, length(ids)),
        free_memory_bytes = rep(14 * 1024^3, length(ids)),
        compute_capability_major = parts$major,
        compute_capability_minor = parts$minor,
        compute_capability = parts$text, cuda_arch = parts$arch,
        backend = "cuda", detection_source = "nvidia-smi",
        visibility_status = "physical_visibility_unknown",
        process_visible = FALSE, stringsAsFactors = FALSE
    )
}

test_that("no GPU and no CUDA libraries are structured and safe", {
    testthat::local_mocked_bindings(
        .gpuinfo_native_cuda = function() mock_native_cuda(),
        .cuda_smi_devices = function() gpuinfo:::.cuda_empty_devices(),
        .gpuinfo_native_cudart = function(candidates) mock_runtime_cuda(),
        .cuda_static_info = function(refresh = FALSE) mock_static_cuda(),
        .cuda_driver_version = function() NA_character_,
        .cuda_supported_version = function() NA_character_,
        .package = "gpuinfo"
    )
    info <- cuda_info()
    expect_false(info$hardware_visible)
    expect_false(info$driver_library)
    expect_false(info$runtime_library)
    expect_identical(info$diagnostic_code, "no_nvidia_hardware")
    expect_false(info$usable)
    expect_equal(nrow(info$devices), 0L)
})

test_that("driver presence is separate from visible devices", {
    native <- mock_native_cuda(library = TRUE, initialized = TRUE, init_status = 0L)
    testthat::local_mocked_bindings(
        .gpuinfo_native_cuda = function() native,
        .cuda_smi_devices = function() gpuinfo:::.cuda_empty_devices(),
        .gpuinfo_native_cudart = function(candidates) mock_runtime_cuda(),
        .cuda_static_info = function(refresh = FALSE) mock_static_cuda(),
        .cuda_driver_version = function() "580.1",
        .cuda_supported_version = function() "13.0",
        .package = "gpuinfo"
    )
    info <- cuda_info()
    expect_true(info$driver_library)
    expect_true(info$driver_initialized)
    expect_false(info$driver_usable)
    expect_identical(info$diagnostic_code, "no_nvidia_hardware")
    expect_true("no_visible_cuda_device" %in% info$reason_codes)
})

test_that("driver initialization and device enumeration stay independent", {
    native <- mock_native_cuda(library = TRUE, initialized = TRUE,
                               init_status = 0L)
    native$count_status <- 999L
    testthat::local_mocked_bindings(
        .gpuinfo_native_cuda = function() native,
        .cuda_smi_devices = function() gpuinfo:::.cuda_empty_devices(),
        .gpuinfo_native_cudart = function(candidates) mock_runtime_cuda(),
        .cuda_static_info = function(refresh = FALSE) mock_static_cuda(),
        .cuda_driver_version = function() "580.1",
        .cuda_supported_version = function() "13.0",
        .package = "gpuinfo"
    )
    info <- cuda_info()
    expect_true(info$driver_initialized)
    expect_identical(info$driver_initialization_status, 0L)
    expect_identical(info$driver_device_count_status, 999L)
    expect_false(info$driver_usable)
})

test_that("driver can be usable without runtime Toolkit or nvcc", {
    native <- mock_native_cuda(
        TRUE, TRUE, "NVIDIA T4", "GPU-A", "7.5", 16 * 1024^3, 0L
    )
    testthat::local_mocked_bindings(
        .gpuinfo_native_cuda = function() native,
        .cuda_smi_devices = function() mock_physical_cuda(0L, "GPU-A", "NVIDIA T4", "7.5"),
        .gpuinfo_native_cudart = function(candidates) mock_runtime_cuda(),
        .cuda_static_info = function(refresh = FALSE) mock_static_cuda(),
        .cuda_driver_version = function() "580.1",
        .cuda_supported_version = function() "13.0",
        .package = "gpuinfo"
    )
    info <- cuda_info()
    expect_true(info$driver_usable)
    expect_false(info$runtime_library)
    expect_false(info$toolkit_available)
    expect_false(info$compiler_available)
    expect_true(info$usable)
})

test_that("runtime version comes from libcudart and not nvcc", {
    native <- mock_native_cuda(
        TRUE, TRUE, "NVIDIA T4", "GPU-A", "7.5", 16 * 1024^3, 0L
    )
    static <- mock_static_cuda(toolkit = TRUE, compiler = FALSE)
    static$toolkit_version <- "13.0"
    testthat::local_mocked_bindings(
        .gpuinfo_native_cuda = function() native,
        .cuda_smi_devices = function() mock_physical_cuda(0L, "GPU-A", "NVIDIA T4", "7.5"),
        .gpuinfo_native_cudart = function(candidates) mock_runtime_cuda(TRUE, TRUE, 12060L),
        .cuda_static_info = function(refresh = FALSE) static,
        .cuda_driver_version = function() "580.1",
        .cuda_supported_version = function() "13.0",
        .package = "gpuinfo"
    )
    info <- cuda_info()
    expect_identical(info$runtime_version, "12.6")
    expect_identical(info$toolkit_version, "13.0")
    expect_false(info$compiler_available)
})

test_that("malformed nvidia-smi output is ignored safely", {
    rows <- gpuinfo:::.cuda_parse_smi_devices(c(
        "garbage", "x, GPU-A, missing", "0,GPU-A,Model,not-memory,0,0,8.0"
    ))
    expect_equal(nrow(rows), 0L)
})

test_that("missing compute capability support stays unknown", {
    native <- mock_native_cuda(
        TRUE, TRUE, "NVIDIA GPU", "GPU-A", NA_character_, 8 * 1024^3, 0L
    )
    rows <- gpuinfo:::.cuda_native_devices(native)
    expect_true(is.na(rows$compute_capability))
    expect_true(is.na(rows$cuda_arch))
})

test_that("multiple CUDA capabilities and Blackwell 12.0 are retained", {
    native <- mock_native_cuda(
        TRUE, TRUE, c("A100", "Blackwell"), c("GPU-A", "GPU-B"),
        c("8.0", "12.0"), c(80, 192) * 1024^3, 0L
    )
    rows <- gpuinfo:::.cuda_native_devices(native)
    expect_identical(rows$compute_capability, c("8.0", "12.0"))
    expect_identical(rows$cuda_arch, c(80L, 120L))
})

test_that("CUDA visibility order is taken from driver ordinals", {
    physical <- mock_physical_cuda(
        c(0L, 1L), c("GPU-A", "GPU-B"), c("same model", "same model"),
        c("8.0", "9.0")
    )
    native <- mock_native_cuda(
        TRUE, TRUE, "same model", "GPU-B", "9.0", 16 * 1024^3, 0L
    )
    old_visibility <- Sys.getenv("CUDA_VISIBLE_DEVICES", unset = NA_character_)
    Sys.setenv(CUDA_VISIBLE_DEVICES = "GPU-B")
    on.exit(if (is.na(old_visibility)) Sys.unsetenv("CUDA_VISIBLE_DEVICES") else
        Sys.setenv(CUDA_VISIBLE_DEVICES = old_visibility), add = TRUE)
    rows <- gpuinfo:::.cuda_reconcile_devices(
        gpuinfo:::.cuda_native_devices(native), physical, TRUE
    )
    expect_identical(rows$uuid, c("GPU-B", "GPU-A"))
    expect_identical(rows$logical_id, c(0L, NA_integer_))
    expect_identical(rows$physical_id, c(1L, 0L))
    expect_identical(rows$visibility_status,
                     c("process_visible", "physical_only_not_process_visible"))
})

test_that("UUID reconciliation does not use numeric id or model", {
    physical <- mock_physical_cuda(
        c(0L, 1L), c("GPU-A", "GPU-B"), c("same", "same"), c("8.0", "9.0")
    )
    native <- mock_native_cuda(
        TRUE, TRUE, c("same", "same"), c("GPU-B", "GPU-A"),
        c("9.0", "8.0"), rep(16 * 1024^3, 2L), 0L
    )
    rows <- gpuinfo:::.cuda_reconcile_devices(
        gpuinfo:::.cuda_native_devices(native), physical, TRUE
    )
    expect_identical(rows$physical_id, c(1L, 0L))
    expect_true(all(rows$detection_source == "cuda-driver+nvidia-smi-uuid"))
})

test_that("has_gpu excludes physical-only CUDA devices", {
    physical <- mock_physical_cuda(7L, "GPU-A", "NVIDIA GPU", "8.0")
    visible <- physical
    visible$logical_id <- 0L
    visible$process_visible <- TRUE
    visible$visibility_status <- "process_visible"
    testthat::local_mocked_bindings(
        gpu_devices = function(refresh = FALSE) physical,
        .package = "gpuinfo"
    )
    expect_false(has_gpu())
    testthat::local_mocked_bindings(
        gpu_devices = function(refresh = FALSE) visible,
        .package = "gpuinfo"
    )
    expect_true(has_gpu())
})

test_that("OS NVIDIA fallback is retained only without CUDA observations", {
    generic <- data.frame(
        id = 0L, vendor = "NVIDIA", model = "NVIDIA GPU",
        memory_mb = 8192, backend = NA_character_,
        compute_capability = NA_character_, stringsAsFactors = FALSE
    )
    physical <- mock_physical_cuda(7L, "GPU-A", "NVIDIA GPU", "8.0")
    testthat::local_mocked_bindings(
        gpu_info = function() generic,
        .package = "gpuinfo"
    )
    fallback <- gpuinfo:::.gpuinfo_devices_from_cuda(
        gpuinfo:::.cuda_empty_devices()
    )
    expect_equal(nrow(fallback), 1L)
    expect_true(fallback$process_visible)
    reconciled <- gpuinfo:::.gpuinfo_devices_from_cuda(physical)
    expect_equal(nrow(reconciled), 1L)
    expect_false(reconciled$process_visible)
    hidden <- gpuinfo:::.gpuinfo_devices_from_cuda(
        gpuinfo:::.cuda_empty_devices(), driver_initialized = TRUE
    )
    expect_equal(nrow(hidden), 0L)
})

test_that("memory information uses explicit byte columns", {
    devices <- mock_physical_cuda(0L, "GPU-A", "A100", "8.0")
    devices$logical_id <- 0L
    devices$process_visible <- TRUE
    devices$visibility_status <- "process_visible"
    testthat::local_mocked_bindings(
        gpu_devices = function(refresh = FALSE) devices,
        .package = "gpuinfo"
    )
    memory <- gpu_memory_info()
    expect_named(memory, c(
        "logical_id", "physical_id", "uuid", "vendor", "model",
        "total_memory_bytes", "used_memory_bytes", "free_memory_bytes",
        "backend", "detection_source", "visibility_status", "process_visible"
    ))
    expect_equal(memory$free_memory_bytes, 14 * 1024^3)
})
