test_that("shipped validation evidence is structured", {
    evidence <- validation_info()
    expect_s3_class(evidence, "data.frame")
    expect_named(evidence, c(
        "package_version", "date", "os", "architecture", "hardware",
        "backend", "result", "scope"
    ))
    expect_true(all(evidence$result == "pass"))
})

test_that("JSON diagnostics are optional and explicit", {
    skip_if_not_installed("jsonlite")
    value <- hardware_info("json")
    expect_s3_class(value, "json")
    expect_match(as.character(value), '"accelerators"')
    expect_output(gpu_sitrep("json"), '"best_backend"')
})
