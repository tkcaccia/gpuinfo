#' Inspect CPU resources visible to the current process
#'
#' Reports CPU identity and topology using base R and read-only operating-system
#' interfaces. `available_cores` estimates the processors available after
#' affinity, container, and common scheduler limits. Unknown values are `NA`.
#'
#' @param refresh Re-run cached static discovery.
#' @return A named list containing identity, topology, process-available cores,
#'   instruction-set flags when available, source, status, and reason codes.
#' @examples
#' cpu <- cpu_info()
#' cpu$logical_cores
#' cpu$available_cores
#' @seealso [system_memory_info()], [hardware_info()]
#' @export
cpu_info <- function(refresh = FALSE) {
    key <- "cpu_info_v2"
    if (isTRUE(refresh) && exists(key, envir = .gpuinfo_cache, inherits = FALSE))
        rm(list = key, envir = .gpuinfo_cache)
    .gpuinfo_cached(key, .gpuinfo_cpu_info_uncached)
}

.gpuinfo_cpu_info_uncached <- function() {
    sys <- Sys.info()
    os <- unname(sys[["sysname"]])
    model <- vendor <- NA_character_
    logical <- physical <- packages <- threads <- NA_integer_
    features <- sources <- character()
    if (identical(os, "Darwin")) {
        get_sysctl <- function(name) {
            value <- .gpuinfo_command("sysctl", c("-n", name))
            if (value$ok) .gpuinfo_na(value$output) else NA_character_
        }
        model <- get_sysctl("machdep.cpu.brand_string")
        logical <- suppressWarnings(as.integer(get_sysctl("hw.logicalcpu")))
        physical <- suppressWarnings(as.integer(get_sysctl("hw.physicalcpu")))
        packages <- suppressWarnings(as.integer(get_sysctl("hw.packages")))
        features <- .gpuinfo_cpu_features(c(get_sysctl("machdep.cpu.features"),
                                             get_sysctl("machdep.cpu.leaf7_features")))
        hardware <- .gpuinfo_cached("hardware_profiler", function()
            .gpuinfo_command("system_profiler", c("SPHardwareDataType", "-detailLevel", "mini"), timeout = 15L)$output)
        if (is.na(model)) model <- .gpuinfo_match(hardware, "^\\s*Chip:\\s*(.+)$")
        if (is.na(model)) model <- .gpuinfo_match(hardware, "^\\s*Processor Name:\\s*(.+)$")
        vendor <- if (!is.na(model) && grepl("Apple", model, fixed = TRUE)) "Apple" else NA_character_
        sources <- c("sysctl", "system_profiler")
    } else if (identical(os, "Linux") && file.exists("/proc/cpuinfo")) {
        lines <- tryCatch(readLines("/proc/cpuinfo", warn = FALSE), error = function(e) character())
        parsed <- .gpuinfo_parse_linux_cpuinfo(lines)
        model <- parsed$model
        vendor <- parsed$vendor
        physical <- parsed$physical_cores
        packages <- parsed$packages
        features <- parsed$features
        sources <- "/proc/cpuinfo"
    } else if (identical(os, "Windows")) {
        parsed <- .gpuinfo_windows_cpu()
        model <- parsed$model
        vendor <- parsed$vendor
        logical <- parsed$logical_cores
        physical <- parsed$physical_cores
        packages <- parsed$packages
        sources <- parsed$source
    }
    if (is.na(logical)) {
        logical <- suppressWarnings(tryCatch(parallel::detectCores(logical = TRUE),
                                             error = function(e) NA_integer_))
        sources <- c(sources, "parallel::detectCores")
    }
    if (is.na(physical))
        physical <- suppressWarnings(tryCatch(parallel::detectCores(logical = FALSE),
                                              error = function(e) NA_integer_))
    if (!is.na(logical) && !is.na(physical) && physical > 0L &&
        logical %% physical == 0L)
        threads <- as.integer(logical / physical)
    available <- .gpuinfo_available_cores(os, logical)
    reasons <- character()
    if (is.na(model)) reasons <- c(reasons, "cpu_model_unavailable")
    if (is.na(physical)) reasons <- c(reasons, "physical_core_count_unavailable")
    if (is.na(available)) reasons <- c(reasons, "available_core_count_unavailable")
    if (!is.na(logical) && !is.na(physical) && is.na(threads))
        reasons <- c(reasons, "nonuniform_threads_per_core")
    list(
        model = model, vendor = vendor, architecture = unname(sys[["machine"]]),
        logical_cores = as.integer(logical), physical_cores = as.integer(physical),
        available_cores = as.integer(available), packages = as.integer(packages),
        threads_per_core = as.integer(threads), features = features, os = os,
        os_release = unname(sys[["release"]]),
        detection_source = paste(unique(sources[nzchar(sources)]), collapse = ","),
        status = if (is.na(logical) || length(reasons)) "partial" else "available",
        reason_codes = unique(reasons)
    )
}

.gpuinfo_parse_linux_cpuinfo <- function(lines) {
    model <- .gpuinfo_match(lines, "^(?:model name|Hardware)\\s*:\\s*(.+)$")
    if (is.na(model)) {
        candidate <- .gpuinfo_match(lines, "^Processor\\s*:\\s*(.+)$")
        if (!is.na(candidate) && !grepl("^[0-9]+$", candidate)) model <- candidate
    }
    vendor <- .gpuinfo_match(lines, "^(?:vendor_id|CPU implementer)\\s*:\\s*(.+)$")
    flags <- .gpuinfo_match(lines, "^(?:flags|Features)\\s*:\\s*(.+)$")
    physical_ids <- sub("^physical id\\s*:\\s*", "", lines[grepl("^physical id\\s*:", lines, ignore.case = TRUE)])
    core_ids <- sub("^core id\\s*:\\s*", "", lines[grepl("^core id\\s*:", lines, ignore.case = TRUE)])
    physical <- packages <- NA_integer_
    if (length(physical_ids)) packages <- length(unique(trimws(physical_ids)))
    if (length(physical_ids) && length(physical_ids) == length(core_ids))
        physical <- length(unique(paste(trimws(physical_ids), trimws(core_ids), sep = ":")))
    list(model = model, vendor = vendor, physical_cores = as.integer(physical),
         packages = as.integer(packages), features = .gpuinfo_cpu_features(flags))
}

.gpuinfo_cpu_features <- function(x) {
    if (!length(x) || all(is.na(x))) return(character())
    values <- unlist(strsplit(tolower(paste(x[!is.na(x)], collapse = " ")),
                              "[[:space:],]+"), use.names = FALSE)
    sort(unique(values[nzchar(values)]))
}

.gpuinfo_available_cores <- function(os, logical) {
    candidates <- as.numeric(logical)
    if (identical(os, "Linux")) {
        nproc <- .gpuinfo_command("nproc")
        if (nproc$ok) candidates <- c(candidates, suppressWarnings(as.numeric(nproc$output[[1L]])))
        cpuset_paths <- c("/sys/fs/cgroup/cpuset.cpus.effective", "/sys/fs/cgroup/cpuset/cpuset.cpus")
        for (path in cpuset_paths[file.exists(cpuset_paths)]) {
            value <- tryCatch(readLines(path, warn = FALSE, n = 1L), error = function(e) character())
            candidates <- c(candidates, .gpuinfo_count_cpu_list(value))
        }
        if (file.exists("/sys/fs/cgroup/cpu.max")) {
            line <- tryCatch(readLines("/sys/fs/cgroup/cpu.max", warn = FALSE, n = 1L),
                             error = function(e) character())
            value <- strsplit(paste(line, collapse = " "), "[[:space:]]+")[[1L]]
            if (length(value) >= 2L && !identical(value[[1L]], "max")) {
                quota <- suppressWarnings(as.numeric(value[[1L]]))
                period <- suppressWarnings(as.numeric(value[[2L]]))
                if (is.finite(quota) && is.finite(period) && period > 0)
                    candidates <- c(candidates, max(1, ceiling(quota / period)))
            }
        }
        quota_path <- "/sys/fs/cgroup/cpu/cpu.cfs_quota_us"
        period_path <- "/sys/fs/cgroup/cpu/cpu.cfs_period_us"
        if (file.exists(quota_path) && file.exists(period_path)) {
            quota <- suppressWarnings(as.numeric(tryCatch(
                readLines(quota_path, warn = FALSE, n = 1L), error = function(e) NA_character_)))
            period <- suppressWarnings(as.numeric(tryCatch(
                readLines(period_path, warn = FALSE, n = 1L), error = function(e) NA_character_)))
            if (is.finite(quota) && quota > 0 && is.finite(period) && period > 0)
                candidates <- c(candidates, max(1, ceiling(quota / period)))
        }
    }
    scheduler <- suppressWarnings(as.numeric(c(
        Sys.getenv("SLURM_CPUS_PER_TASK", unset = NA_character_),
        Sys.getenv("PBS_NP", unset = NA_character_),
        Sys.getenv("NSLOTS", unset = NA_character_))))
    candidates <- c(candidates, scheduler)
    candidates <- candidates[is.finite(candidates) & candidates > 0]
    if (!length(candidates)) NA_integer_ else as.integer(max(1, floor(min(candidates))))
}

.gpuinfo_count_cpu_list <- function(x) {
    if (!length(x) || is.na(x[[1L]]) || !nzchar(trimws(x[[1L]]))) return(NA_real_)
    parts <- strsplit(trimws(x[[1L]]), ",", fixed = TRUE)[[1L]]
    counts <- vapply(parts, function(part) {
        bounds <- suppressWarnings(as.numeric(strsplit(part, "-", fixed = TRUE)[[1L]]))
        if (!length(bounds) || any(!is.finite(bounds))) return(NA_real_)
        if (length(bounds) == 1L) 1 else max(0, bounds[[2L]] - bounds[[1L]] + 1)
    }, numeric(1L))
    if (anyNA(counts)) NA_real_ else sum(counts)
}

.gpuinfo_windows_cpu <- function() {
    empty <- list(model = NA_character_, vendor = NA_character_, logical_cores = NA_integer_,
                  physical_cores = NA_integer_, packages = NA_integer_, source = "unavailable")
    command <- paste(
        "$x=Get-CimInstance Win32_Processor;",
        "'MODEL='+($x.Name -join '|');'VENDOR='+($x.Manufacturer -join '|');",
        "'LOGICAL='+(($x|Measure-Object NumberOfLogicalProcessors -Sum).Sum);",
        "'PHYSICAL='+(($x|Measure-Object NumberOfCores -Sum).Sum);'PACKAGES='+$x.Count")
    out <- .gpuinfo_command("powershell", c("-NoProfile", "-NonInteractive", "-Command", command), timeout = 10L)
    if (!out$ok) return(empty)
    value <- function(name) .gpuinfo_match(out$output, paste0("^", name, "=(.*)$"))
    list(model = value("MODEL"), vendor = value("VENDOR"),
         logical_cores = suppressWarnings(as.integer(value("LOGICAL"))),
         physical_cores = suppressWarnings(as.integer(value("PHYSICAL"))),
         packages = suppressWarnings(as.integer(value("PACKAGES"))), source = "PowerShell CIM")
}

#' Inspect host and process memory
#'
#' Returns a point-in-time snapshot. Host totals are kept separate from the
#' effective memory visible inside a container or control group. All sizes are
#' numeric byte counts, and dynamic values are never cached.
#'
#' @param pid Process identifier whose resident memory should be sampled.
#' @return A named list containing host, effective, swap, and selected-process
#'   memory values plus source, status, and reason codes.
#' @examples
#' memory <- system_memory_info()
#' memory$total_memory_bytes
#' memory$effective_available_memory_bytes
#' @seealso [cpu_info()], [gpu_memory_info()], [gpu_memory_sample()]
#' @export
system_memory_info <- function(pid = Sys.getpid()) {
    if (length(pid) != 1L || is.na(pid) || !is.finite(pid) || pid < 1 ||
        pid > .Machine$integer.max || pid != floor(pid))
        stop("'pid' must be one positive integer.", call. = FALSE)
    pid <- as.integer(pid)
    os <- unname(Sys.info()[["sysname"]])
    value <- switch(os, Linux = .gpuinfo_linux_memory(pid), Darwin = .gpuinfo_macos_memory(pid),
                    Windows = .gpuinfo_windows_memory(pid),
                    .gpuinfo_empty_system_memory("unsupported_operating_system"))
    value$used_memory_bytes <- .gpuinfo_nonnegative_difference(
        value$total_memory_bytes, value$available_memory_bytes)
    value$swap_used_bytes <- .gpuinfo_nonnegative_difference(
        value$swap_total_bytes, value$swap_free_bytes)
    value
}

.gpuinfo_empty_system_memory <- function(reason = "memory_information_unavailable") {
    list(total_memory_bytes = NA_real_, available_memory_bytes = NA_real_,
         free_memory_bytes = NA_real_, used_memory_bytes = NA_real_,
         effective_total_memory_bytes = NA_real_, effective_available_memory_bytes = NA_real_,
         memory_limit_bytes = NA_real_, memory_current_bytes = NA_real_,
         swap_total_bytes = NA_real_, swap_free_bytes = NA_real_, swap_used_bytes = NA_real_,
         process_resident_memory_bytes = NA_real_, process_peak_resident_memory_bytes = NA_real_,
         detection_source = NA_character_, status = "unavailable", reason_codes = reason)
}

.gpuinfo_linux_memory <- function(pid = Sys.getpid()) {
    empty <- .gpuinfo_empty_system_memory()
    lines <- tryCatch(readLines("/proc/meminfo", warn = FALSE), error = function(e) character())
    values <- .gpuinfo_parse_linux_meminfo(lines)
    status_path <- file.path("/proc", as.character(pid), "status")
    status_lines <- tryCatch(readLines(status_path, warn = FALSE), error = function(e) character())
    field <- function(name) .gpuinfo_meminfo_value(status_lines, name)
    limit <- .gpuinfo_read_cgroup_number(c("/sys/fs/cgroup/memory.max",
                                           "/sys/fs/cgroup/memory/memory.limit_in_bytes"))
    current <- .gpuinfo_read_cgroup_number(c("/sys/fs/cgroup/memory.current",
                                             "/sys/fs/cgroup/memory/memory.usage_in_bytes"))
    if (is.finite(limit) && is.finite(values$total) && limit > values$total * 1000) limit <- NA_real_
    effective_total <- if (is.finite(limit)) min(values$total, limit, na.rm = TRUE) else values$total
    limit_available <- if (is.finite(limit) && is.finite(current)) max(0, limit - current) else NA_real_
    effective_available <- values$available
    if (is.finite(limit_available)) effective_available <- min(effective_available, limit_available, na.rm = TRUE)
    reasons <- character()
    if (!is.finite(values$total)) reasons <- c(reasons, "host_total_memory_unavailable")
    if (!is.finite(values$available)) reasons <- c(reasons, "host_available_memory_unavailable")
    empty[names(empty)] <- list(
        values$total, values$available, values$free, NA_real_, effective_total,
        effective_available, limit, current, values$swap_total, values$swap_free,
        NA_real_, field("VmRSS"), field("VmHWM"), "/proc/meminfo,/proc/<pid>/status,cgroup",
        if (length(reasons)) "partial" else "available", reasons)
    empty
}

.gpuinfo_parse_linux_meminfo <- function(lines) {
    get <- function(name) .gpuinfo_meminfo_value(lines, name)
    total <- get("MemTotal")
    available <- get("MemAvailable")
    free <- get("MemFree")
    if (!is.finite(available))
        available <- .gpuinfo_sum_or_na(c(free, get("Buffers"), get("Cached")))
    list(total = total, available = available, free = free,
         swap_total = get("SwapTotal"), swap_free = get("SwapFree"))
}

.gpuinfo_meminfo_value <- function(lines, name) {
    text <- .gpuinfo_match(lines, paste0("^", name, ":\\s*([0-9]+)\\s*(kB)?$"))
    value <- suppressWarnings(as.numeric(text))
    if (is.finite(value)) value * 1024 else NA_real_
}

.gpuinfo_read_cgroup_number <- function(paths) {
    for (path in paths[file.exists(paths)]) {
        text <- tryCatch(readLines(path, warn = FALSE, n = 1L), error = function(e) character())
        if (length(text) && !identical(trimws(text[[1L]]), "max")) {
            value <- suppressWarnings(as.numeric(trimws(text[[1L]])))
            if (is.finite(value) && value >= 0) return(value)
        }
    }
    NA_real_
}

.gpuinfo_macos_memory <- function(pid = Sys.getpid()) {
    empty <- .gpuinfo_empty_system_memory()
    total_cmd <- .gpuinfo_command("sysctl", c("-n", "hw.memsize"))
    total <- if (total_cmd$ok) suppressWarnings(as.numeric(total_cmd$output[[1L]])) else NA_real_
    vm <- .gpuinfo_command("vm_stat")
    parsed <- .gpuinfo_parse_vm_stat(vm$output)
    swap <- .gpuinfo_command("sysctl", c("-n", "vm.swapusage"))
    swap_total <- .gpuinfo_size_to_bytes(.gpuinfo_match(swap$output, "total = ([0-9.]+[KMGTP]?)"))
    swap_free <- .gpuinfo_size_to_bytes(.gpuinfo_match(swap$output, "free = ([0-9.]+[KMGTP]?)"))
    ps <- .gpuinfo_command("ps", c("-o", "rss=", "-p", as.character(pid)))
    rss <- if (ps$ok) suppressWarnings(as.numeric(trimws(ps$output[[1L]]))) * 1024 else NA_real_
    reasons <- character()
    if (!is.finite(total)) reasons <- c(reasons, "host_total_memory_unavailable")
    if (!is.finite(parsed$available)) reasons <- c(reasons, "host_available_memory_unavailable")
    empty[names(empty)] <- list(
        total, parsed$available, parsed$free, NA_real_, total, parsed$available,
        NA_real_, NA_real_, swap_total, swap_free, NA_real_, rss, NA_real_,
        "sysctl,vm_stat,ps", if (length(reasons)) "partial" else "available", reasons)
    empty
}

.gpuinfo_parse_vm_stat <- function(lines) {
    page_size <- suppressWarnings(as.numeric(.gpuinfo_match(lines, "page size of ([0-9]+) bytes")))
    pages <- function(name) suppressWarnings(as.numeric(
        .gpuinfo_match(lines, paste0("^Pages ", name, ":\\s*([0-9]+)\\.?$"))))
    free_pages <- .gpuinfo_sum_or_na(c(pages("free"), pages("speculative")))
    available_pages <- .gpuinfo_sum_or_na(c(free_pages, pages("inactive")))
    list(free = if (is.finite(page_size)) free_pages * page_size else NA_real_,
         available = if (is.finite(page_size)) available_pages * page_size else NA_real_)
}

.gpuinfo_windows_memory <- function(pid = Sys.getpid()) {
    empty <- .gpuinfo_empty_system_memory()
    command <- paste(
        paste0("$TargetPid=", as.integer(pid), ";$o=Get-CimInstance Win32_OperatingSystem;$p=Get-Process -Id $TargetPid;"),
        "'TOTAL='+$o.TotalVisibleMemorySize;'FREE='+$o.FreePhysicalMemory;",
        "'VIRTUALTOTAL='+$o.TotalVirtualMemorySize;'VIRTUALFREE='+$o.FreeVirtualMemory;",
        "'RSS='+$p.WorkingSet64;'PEAKRSS='+$p.PeakWorkingSet64")
    out <- .gpuinfo_command("powershell", c("-NoProfile", "-NonInteractive", "-Command", command), timeout = 10L)
    if (!out$ok) return(empty)
    number <- function(name, scale = 1) {
        value <- suppressWarnings(as.numeric(.gpuinfo_match(out$output, paste0("^", name, "=([0-9]+)$"))))
        if (is.finite(value)) value * scale else NA_real_
    }
    total <- number("TOTAL", 1024)
    free <- number("FREE", 1024)
    swap_total <- .gpuinfo_nonnegative_difference(number("VIRTUALTOTAL", 1024), total)
    swap_free <- .gpuinfo_nonnegative_difference(number("VIRTUALFREE", 1024), free)
    reasons <- if (!is.finite(total)) "host_total_memory_unavailable" else character()
    empty[names(empty)] <- list(
        total, free, free, NA_real_, total, free, NA_real_, NA_real_,
        swap_total, swap_free, NA_real_,
        number("RSS"), number("PEAKRSS"), "PowerShell CIM",
        if (length(reasons)) "partial" else "available", reasons)
    empty
}

.gpuinfo_size_to_bytes <- function(x) {
    if (!length(x) || is.na(x[[1L]])) return(NA_real_)
    match_value <- regexec("^([0-9.]+)([KMGTP]?)$", toupper(trimws(x[[1L]])))
    parts <- regmatches(toupper(trimws(x[[1L]])), match_value)[[1L]]
    if (length(parts) < 3L) return(NA_real_)
    exponent <- match(parts[[3L]], c("", "K", "M", "G", "T", "P")) - 1L
    suppressWarnings(as.numeric(parts[[2L]])) * 1024^exponent
}

.gpuinfo_nonnegative_difference <- function(x, y) {
    if (!is.finite(x) || !is.finite(y)) NA_real_ else max(0, x - y)
}

.gpuinfo_sum_or_na <- function(x) {
    if (!any(is.finite(x))) NA_real_ else sum(x[is.finite(x)])
}
