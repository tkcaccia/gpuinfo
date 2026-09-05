test_that("CPU is always available", {
    expect_true("cpu" %in% available_backends())
    expect_identical(best_backend("cpu"), "cpu")
})

test_that("backend preference is honored", {
    available <- available_backends()
    expect_identical(best_backend(rev(available)), rev(available)[[1L]])
})

test_that("backend preference is validated", {
    expect_error(best_backend(character()), "non-empty")
    expect_error(best_backend(c("cuda", NA_character_)), "missing")
    expect_error(best_backend("quantum"), "Unknown backend")
})
