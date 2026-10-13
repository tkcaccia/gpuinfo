memory_sample_devices <- function(backends = "cuda", uuids = "GPU-A") {
    n <- length(backends)
    data.frame(
        logical_id = seq_len(n) - 1L, physical_id = seq_len(n) - 1L,
        uuid = rep_len(uuids, n), vendor = ifelse(backends == "metal", "Apple", "NVIDIA"),
        model = ifelse(backends == "metal", "Apple M3", "NVIDIA GPU"),
        total_memory_bytes = rep(8 * 1024^3, n),
        used_memory_bytes = rep(NA_real_, n), free_memory_bytes = rep(NA_real_, n),
        compute_capability_major = rep(NA_integer_, n),
        compute_capability_minor = rep(NA_integer_, n),
        compute_capability = rep(NA_character_, n), cuda_arch = rep(NA_integer_, n),
        backend = backends, detection_source = rep("mock", n),
        visibility_status = rep("process_visible", n),
        process_visible = rep(TRUE, n), stringsAsFactors = FALSE
    )
}

test_that("CUDA process rows retain unavailable WDDM memory", {
    rows <- gpuinfo:::.cuda_parse_process_memory(c(
        "42, GPU-A, 128", "42, GPU-B, N/A", "malformed"
    ))
    expect_equal(nrow(rows), 2L)
    expect_equal(rows$used_memory_bytes[[1L]], 128 * 1024^2)
    expect_true(is.na(rows$used_memory_bytes[[2L]]))
})

test_that("CUDA process memory is reconciled by UUID and PID", {
    devices <- memory_sample_devices(c("cuda", "cuda"), c("GPU-A", "GPU-B"))
    query <- list(ok = TRUE, status = 0L, rows = data.frame(
        pid = c(99L, 99L, 100L), uuid = c("GPU-B", "GPU-B", "GPU-A"),
        used_memory_bytes = c(64, 32, 512) * 1024^2
    ))
    testthat::local_mocked_bindings(
        gpu_devices = function(refresh = FALSE) devices,
        .cuda_process_memory = function() query,
        .package = "gpuinfo"
    )
    sample <- gpu_memory_sample(pid = 99L)
    expect_equal(sample$used_memory_bytes, c(0, 96 * 1024^2))
    expect_identical(sample$measurement_status, c("not_active", "measured"))
    expect_identical(sample$reason_code, c("process_not_using_device", "ok"))
})

test_that("CUDA worker PIDs share one process-memory observation", {
    devices <- memory_sample_devices(c("cuda", "cuda"), c("GPU-A", "GPU-B"))
    calls <- 0L
    query <- list(ok = TRUE, status = 0L, rows = data.frame(
        pid = c(99L, 100L, 100L), uuid = c("GPU-A", "GPU-B", "GPU-B"),
        used_memory_bytes = c(64, 32, 16) * 1024^2
    ))
    testthat::local_mocked_bindings(
        gpu_devices = function(refresh = FALSE) devices,
        .cuda_process_memory = function() {
            calls <<- calls + 1L
            query
        },
        .package = "gpuinfo"
    )
    sample <- gpu_memory_sample(pid = c(99L, 100L))
    expect_identical(calls, 1L)
    expect_identical(sample$pid, c(99L, 99L, 100L, 100L))
    expect_identical(sample$uuid, c("GPU-A", "GPU-B", "GPU-A", "GPU-B"))
    expect_equal(sample$used_memory_bytes, c(64, 0, 0, 48) * 1024^2)
    expect_length(unique(sample$sample_time), 1L)
    expect_error(gpu_memory_sample(pid = c(99L, 99L)), "distinct")
})

test_that("CUDA unavailable process accounting returns a reason code", {
    devices <- memory_sample_devices("cuda", "GPU-A")
    query <- list(ok = TRUE, status = 0L, rows = data.frame(
        pid = 99L, uuid = "GPU-A", used_memory_bytes = NA_real_
    ))
    testthat::local_mocked_bindings(
        gpu_devices = function(refresh = FALSE) devices,
        .cuda_process_memory = function() query,
        .package = "gpuinfo"
    )
    sample <- gpu_memory_sample(pid = 99L)
    expect_true(is.na(sample$used_memory_bytes))
    expect_identical(sample$measurement_status, "unavailable")
    expect_identical(sample$reason_code, "cuda_process_memory_unavailable")
})

test_that("Metal samples only the current process", {
    devices <- memory_sample_devices("metal", NA_character_)
    testthat::local_mocked_bindings(
        gpu_devices = function(refresh = FALSE) devices,
        .gpuinfo_native_metal = function() list(
            framework = TRUE, device = TRUE, allocation_query_supported = TRUE,
            current_allocated_size = 123456
        ),
        .package = "gpuinfo"
    )
    current <- gpu_memory_sample()
    expect_equal(current$used_memory_bytes, 123456)
    expect_identical(current$measurement_source, "MTLDevice.currentAllocatedSize")
    other <- gpu_memory_sample(pid = Sys.getpid() + 1L)
    expect_true(is.na(other$used_memory_bytes))
    expect_identical(other$reason_code, "metal_current_process_only")
    both <- gpu_memory_sample(pid = c(Sys.getpid() + 1L, Sys.getpid()))
    expect_true(is.na(both$used_memory_bytes[[1L]]))
    expect_equal(both$used_memory_bytes[[2L]], 123456)
})

test_that("memory sampler has a stable empty schema", {
    testthat::local_mocked_bindings(
        gpu_devices = function(refresh = FALSE) gpuinfo:::.cuda_empty_devices(),
        .package = "gpuinfo"
    )
    sample <- gpu_memory_sample()
    expect_named(sample, names(gpuinfo:::.gpuinfo_empty_memory_sample()))
    expect_equal(nrow(sample), 0L)
    expect_error(gpu_memory_sample(pid = 0L), "positive integer")
})

test_that("hardware manifest is versioned and serializable", {
    manifest <- hardware_manifest(memory = "none")
    expect_s3_class(manifest, "gpuinfo_manifest")
    expect_identical(manifest$schema_version, "1.0")
    expect_named(manifest, c(
        "schema_version", "generated_at", "package", "process", "host",
        "devices", "device_summary", "components", "memory", "measurement_rules"
    ))
    expect_identical(manifest$memory$scope, "none")
    expect_null(manifest$memory$sample)
    if (requireNamespace("jsonlite", quietly = TRUE)) {
        expect_type(hardware_manifest(memory = "none", as = "json"), "character")
    }
})
