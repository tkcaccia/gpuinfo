test_that("backend information is structured", {
    cuda <- cuda_info()
    expect_named(cuda, c(
        "hardware_visible", "driver_library", "driver_initialized",
        "driver_usable", "driver_initialization_status",
        "driver_device_count_status",
        "gpu", "driver", "driver_version", "driver_supported_cuda_version",
        "runtime_library", "runtime_library_path", "runtime_initialized",
        "runtime_usable", "runtime_initialization_status", "runtime",
        "runtime_version", "runtime_version_status", "toolkit_available",
        "toolkit", "toolkit_root", "toolkit_version", "compiler_available",
        "compiler_version", "nvcc", "nvcc_path", "libraries", "devices",
        "physical_devices", "visible_device_count", "compute_capability",
        "cuda_architectures", "cuda_visible_devices", "status",
        "diagnostic_code", "reason_codes", "usable"
    ), ignore.order = TRUE)
    expect_type(metal_info(), "list")
    expect_type(rocm_info(), "list")
    expect_type(opencl_info(), "list")
})

test_that("hardware_info and sitrep are safe", {
    info <- hardware_info()
    expect_named(info, c(
        "r", "system", "cpu", "memory", "gpu", "gpu_devices", "gpu_summary",
        "accelerators", "capabilities", "cuda", "nccl",
        "metal", "rocm", "opencl", "available_backends", "best_backend",
        "environment", "validation"
    ))
    expect_silent(returned <- gpu_sitrep())
    expect_s3_class(returned, "gpuinfo_sitrep")
    expect_output(print(returned), "gpuinfo diagnostic report")
    expect_type(returned, "list")
})

test_that("reports run identical commands once and refresh between calls", {
    commands <- character()
    testthat::local_mocked_bindings(
        .gpuinfo_run_command = function(command, args = character(), timeout = 5L) {
            commands <<- c(commands, paste(command, paste(args, collapse = " "), timeout))
            list(ok = FALSE, output = character(), status = NA_integer_,
                 path = NA_character_)
        },
        .package = "gpuinfo"
    )
    first <- hardware_info()
    first_commands <- commands
    expect_gt(length(first_commands), 0L)
    expect_identical(anyDuplicated(first_commands), 0L)
    commands <- character()
    second <- hardware_info()
    expect_gt(length(commands), 0L)
    expect_identical(anyDuplicated(commands), 0L)
    expect_named(second, names(first))
    expect_null(gpuinfo:::.gpuinfo_observation$cache)
})

test_that("observation reuse is nested and discarded after errors", {
    observe <- gpuinfo:::.gpuinfo_with_observation
    once <- gpuinfo:::.gpuinfo_observe_once
    calls <- 0L
    compute <- function() {
        calls <<- calls + 1L
        calls
    }
    values <- observe(function() {
        c(once("memory", compute = compute),
          observe(function() once("memory", compute = compute)))
    })
    expect_identical(values, c(1L, 1L))
    expect_identical(once("memory", compute = compute), 2L)
    expect_identical(observe(function() once("memory", compute = compute)), 3L)
    expect_error(observe(function() stop("interrupted report")), "interrupted report")
    expect_null(gpuinfo:::.gpuinfo_observation$cache)
})
