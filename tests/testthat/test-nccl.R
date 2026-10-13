test_that("missing NCCL is reported without creating a communicator", {
    raw <- gpuinfo:::.gpuinfo_native_nccl(
        "/gpuinfo/nonexistent/libnccl.so")
    expect_false(raw$library)
    expect_true(is.na(raw$library_path))
    expect_true(is.na(raw$version_integer))
})

test_that("NCCL version integers are decoded", {
    expect_identical(gpuinfo:::.nccl_version_string(22705L), "2.27.5")
    expect_identical(gpuinfo:::.nccl_version_string(2708L), "2.7.8")
    expect_true(is.na(gpuinfo:::.nccl_version_string(NA_integer_)))
})

test_that("NCCL inspection separates library presence from communication", {
    local_mocked_bindings(
        .nccl_supported_platform = function() TRUE,
        .nccl_candidates = function(...) "libnccl.so.2",
        .gpuinfo_native_nccl = function(...) list(
            library = TRUE, library_path = "libnccl.so.2",
            version_status = 0L, version_integer = 22705L
        ),
        cuda_info = function(...) list(visible_device_count = 2L),
        .package = "gpuinfo"
    )
    info <- nccl_info()
    expect_true(info$library)
    expect_identical(info$version, "2.27.5")
    expect_identical(info$version_integer, 22705L)
    expect_identical(info$visible_cuda_device_count, 2L)
    expect_identical(info$visible_device_count, 2L)
    expect_true(info$multiple_visible_cuda_devices)
    expect_identical(info$status, "available")
    expect_identical(info$diagnostic_code, "ok")
    expect_false(info$communication_tested)
})

test_that("missing NCCL returns structured diagnostics", {
    local_mocked_bindings(
        .nccl_supported_platform = function() TRUE,
        .nccl_candidates = function(...) character(),
        .gpuinfo_native_nccl = function(...) list(
            library = FALSE, library_path = NA_character_,
            version_status = NA_integer_, version_integer = NA_integer_
        ),
        cuda_info = function(...) list(visible_device_count = 0L),
        .package = "gpuinfo"
    )
    info <- nccl_info()
    expect_false(info$library)
    expect_identical(info$status, "unavailable")
    expect_match(info$diagnostic_code, "^nccl_")
    expect_false(info$communication_tested)
})
