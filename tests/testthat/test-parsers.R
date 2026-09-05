test_that("NVIDIA query fixtures retain multiple devices", {
    path <- testthat::test_path("..", "fixtures", "nvidia", "representative-query.csv")
    rows <- gpuinfo:::.cuda_parse_smi_rows(readLines(path), TRUE)
    expect_equal(nrow(rows), 2L)
    expect_identical(rows$model[[1L]], "NVIDIA GeForce RTX 4090")
    expect_equal(rows$memory_mb[[2L]], 81920)
    expect_identical(rows$compute_capability, c("8.9", "8.0"))
})

test_that("NVIDIA memory fixture distinguishes total, used, and free", {
    path <- testthat::test_path("..", "fixtures", "nvidia", "representative-memory.csv")
    rows <- gpuinfo:::.cuda_parse_memory_rows(readLines(path))
    expect_equal(rows$used, c(3100, 1024))
    expect_equal(rows$free, c(21464, 80896))
})

test_that("Apple profiler fixture is parsed without live hardware", {
    path <- testthat::test_path("..", "fixtures", "apple", "apple-m3-system-profiler.txt")
    lines <- readLines(path)
    testthat::local_mocked_bindings(.metal_profiler = function() lines, .package = "gpuinfo")
    rows <- gpuinfo:::.metal_gpus()
    expect_equal(nrow(rows), 1L)
    expect_identical(rows$vendor, "Apple")
    expect_identical(rows$model, "Apple M3")
})

test_that("OpenCL vendor metadata prevents a CUDA device duplicate", {
    native <- list(
        devices = "Tesla T4", vendors = "NVIDIA Corporation", gpu = TRUE,
        memory_bytes = 15828320256, fp64 = TRUE
    )
    rows <- gpuinfo:::.opencl_gpu_rows(native)
    expect_identical(rows$vendor, "NVIDIA")
    expect_identical(rows$model, "Tesla T4")
})

test_that("real NVIDIA validation fixture records a passing T4 run", {
    path <- testthat::test_path("..", "fixtures", "nvidia", "hf-t4-validation.txt")
    evidence <- readLines(path)
    expect_true(any(evidence == "GPU: NVIDIA Tesla T4"))
    expect_true(any(evidence == "compute_capability: 7.5"))
    expect_true(any(evidence == "result: VALIDATION PASSED"))
    expect_true(any(evidence == "deduplication: PASSED"))
})
