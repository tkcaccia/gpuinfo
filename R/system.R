#' CPU information
#'
#' Returns lightweight information about the host CPU using base R and
#' operating-system utilities when available.
#'
#' @return A named list containing CPU model, architecture, logical core count,
#'   vendor, and operating system.
#' @export
cpu_info <- function() {
    sys <- Sys.info()
    model <- vendor <- NA_character_
    os <- unname(sys[["sysname"]])

    if (identical(os, "Darwin")) {
        model <- .gpuinfo_na(.gpuinfo_command("sysctl", c("-n", "machdep.cpu.brand_string"))$output)
        if (!is.na(model) && grepl("Operation not permitted|^sysctl:", model)) model <- NA_character_
        hardware <- .gpuinfo_cached("hardware_profiler", function()
            .gpuinfo_command("system_profiler", c("SPHardwareDataType", "-detailLevel", "mini"), timeout = 15L)$output)
        if (is.na(model)) {
            model <- .gpuinfo_match(hardware, "^\\s*Chip:\\s*(.+)$")
        }
        if (is.na(model)) {
            model <- .gpuinfo_match(hardware, "^\\s*Processor Name:\\s*(.+)$")
        }
        vendor <- if (!is.na(model) && grepl("Apple", model, fixed = TRUE)) "Apple" else NA_character_
    } else if (identical(os, "Linux") && file.exists("/proc/cpuinfo")) {
        lines <- tryCatch(readLines("/proc/cpuinfo", warn = FALSE), error = function(e) character())
        model <- .gpuinfo_match(lines, "^(?:model name|Hardware)\\s*:\\s*(.+)$")
        vendor <- .gpuinfo_match(lines, "^vendor_id\\s*:\\s*(.+)$")
    } else if (identical(os, "Windows")) {
        out <- .gpuinfo_command("wmic", c("cpu", "get", "Name,Manufacturer", "/format:csv"))$output
        row <- out[grepl(",", out, fixed = TRUE)]
        if (length(row)) {
            fields <- trimws(strsplit(utils::tail(row, 1L), ",", fixed = TRUE)[[1L]])
            if (length(fields) >= 3L) {
                vendor <- .gpuinfo_na(fields[[2L]])
                model <- .gpuinfo_na(fields[[3L]])
            }
        }
    }

    cores <- suppressWarnings(tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_))
    if (is.na(cores) && identical(os, "Darwin")) {
        cores <- suppressWarnings(as.integer(.gpuinfo_match(hardware, "^\\s*Total Number of Cores:\\s*([0-9]+)")))
    }
    list(
        model = model,
        vendor = vendor,
        architecture = unname(sys[["machine"]]),
        logical_cores = as.integer(cores),
        os = os,
        os_release = unname(sys[["release"]])
    )
}
