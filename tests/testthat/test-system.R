test_that("Linux CPU topology and features are parsed", {
    lines <- c(
        "processor : 0", "vendor_id : GenuineIntel", "model name : Example CPU",
        "physical id : 0", "core id : 0", "flags : sse avx2",
        "processor : 1", "physical id : 0", "core id : 1"
    )
    value <- gpuinfo:::.gpuinfo_parse_linux_cpuinfo(lines)
    expect_equal(value$model, "Example CPU")
    expect_equal(value$vendor, "GenuineIntel")
    expect_equal(value$physical_cores, 2L)
    expect_equal(value$packages, 1L)
    expect_setequal(value$features, c("sse", "avx2"))
})

test_that("Linux memory values have explicit byte units", {
    value <- gpuinfo:::.gpuinfo_parse_linux_meminfo(c(
        "MemTotal:       1000 kB", "MemFree:         100 kB",
        "MemAvailable:    400 kB", "SwapTotal:       50 kB",
        "SwapFree:        20 kB"
    ))
    expect_equal(value$total, 1000 * 1024)
    expect_equal(value$available, 400 * 1024)
    expect_equal(value$free, 100 * 1024)
    expect_equal(value$swap_total, 50 * 1024)
    expect_equal(value$swap_free, 20 * 1024)
})

test_that("macOS vm_stat and size values are parsed", {
    value <- gpuinfo:::.gpuinfo_parse_vm_stat(c(
        "Mach Virtual Memory Statistics: (page size of 16384 bytes)",
        "Pages free: 10.", "Pages inactive: 20.", "Pages speculative: 5."
    ))
    expect_equal(value$free, 15 * 16384)
    expect_equal(value$available, 35 * 16384)
    expect_equal(gpuinfo:::.gpuinfo_size_to_bytes("1.5G"), 1.5 * 1024^3)
})

test_that("CPU list ranges are counted without double meaning", {
    expect_equal(gpuinfo:::.gpuinfo_count_cpu_list("0-3,8,10-11"), 7)
    expect_true(is.na(gpuinfo:::.gpuinfo_count_cpu_list("bad")))
})

test_that("GPU vendor matching does not confuse Corporation with ATI", {
    expect_identical(
        gpuinfo:::.gpuinfo_gpu_vendor("Intel Corporation Device a780"), "Intel")
    expect_identical(
        gpuinfo:::.gpuinfo_gpu_vendor("Advanced Micro Devices Radeon RX 7900"), "AMD")
    expect_identical(
        gpuinfo:::.gpuinfo_gpu_vendor("NVIDIA Corporation RTX 5060 Ti"), "NVIDIA")
})

test_that("modern and legacy Windows GPU output are parsed", {
    modern <- gpuinfo:::.gpuinfo_parse_windows_gpus(
        c("4294967296|NVIDIA RTX", "1073741824|Intel Graphics"), "|")
    expect_identical(modern$model, c("NVIDIA RTX", "Intel Graphics"))
    expect_equal(modern$bytes, c(4294967296, 1073741824))

    legacy <- gpuinfo:::.gpuinfo_parse_windows_gpus(
        c("Node,AdapterRAM,Name", "PC,8589934592,AMD Radeon"), ",")
    expect_identical(legacy$model, "AMD Radeon")
    expect_equal(legacy$bytes, 8589934592)
})

test_that("public CPU and system memory schemas are stable and safe", {
    cpu <- cpu_info(refresh = TRUE)
    expect_named(cpu, c(
        "model", "vendor", "architecture", "logical_cores", "physical_cores",
        "available_cores", "packages", "threads_per_core", "features", "os",
        "os_release", "detection_source", "status", "reason_codes"
    ))
    expect_type(cpu$logical_cores, "integer")
    expect_true(is.na(cpu$available_cores) || cpu$available_cores >= 1L)

    memory <- system_memory_info()
    expect_named(memory, c(
        "total_memory_bytes", "available_memory_bytes", "free_memory_bytes",
        "used_memory_bytes", "effective_total_memory_bytes",
        "effective_available_memory_bytes", "memory_limit_bytes",
        "memory_current_bytes", "swap_total_bytes", "swap_free_bytes",
        "swap_used_bytes", "process_resident_memory_bytes",
        "process_peak_resident_memory_bytes", "detection_source", "status",
        "reason_codes"
    ))
    numeric_fields <- names(memory)[vapply(memory, is.numeric, logical(1L))]
    expect_true(all(vapply(memory[numeric_fields], function(x) length(x) == 1L,
                           logical(1L))))
    expect_true(memory$status %in% c("available", "partial", "unavailable"))
    expect_error(system_memory_info(pid = 0L), "positive integer")
})

test_that("hardware summary distinguishes visibility from execution", {
    devices <- data.frame(
        physical_id = c(1L, 0L), uuid = c("GPU-B", "GPU-A"),
        vendor = c("NVIDIA", "NVIDIA"), model = c("B", "A"),
        process_visible = c(TRUE, TRUE), stringsAsFactors = FALSE
    )
    value <- gpuinfo:::.gpuinfo_gpu_summary(devices)
    expect_equal(value$visible_device_count, 2L)
    expect_equal(value$physical_device_count, 2L)
    expect_true(value$multiple_visible_devices)
})
