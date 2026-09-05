#' Inspect OpenCL availability
#'
#' This probe uses `clinfo` when available and otherwise checks for the
#' operating-system OpenCL loader. It does not require an OpenCL R package.
#'
#' @return A named list containing loader, utility, platform, device, and
#'   availability information.
#' @export
opencl_info <- function() {
    native <- .gpuinfo_native_opencl()
    result <- .gpuinfo_command("clinfo", "--raw")
    if (!result$ok) result <- .gpuinfo_command("clinfo")
    loader <- native$library || .gpuinfo_library_exists("opencl")
    platforms <- if (native$platform_count > 0L) native$platform_count else if (result$ok) sum(grepl("Platform Name", result$output, fixed = TRUE)) else 0L
    device_lines <- if (result$ok) grep("Device Name", result$output, value = TRUE, fixed = TRUE) else character()
    devices <- unique(c(native$devices, trimws(sub("^.*Device Name\\s+", "", device_lines))))
    devices <- devices[nzchar(devices)]
    list(
        loader = .gpuinfo_bool(loader),
        clinfo = .gpuinfo_bool(result$ok),
        platform_count = as.integer(platforms),
        device_count = as.integer(length(devices)),
        devices = devices,
        gpu_devices = native$devices[native$gpu %in% TRUE],
        memory_bytes = native$memory_bytes,
        fp64 = native$fp64,
        available = .gpuinfo_bool(loader && platforms > 0L && length(devices) > 0L)
    )
}

.opencl_gpu_rows <- function(native = .gpuinfo_native_opencl()) {
    keep <- native$gpu %in% TRUE
    if (!any(keep)) return(.gpuinfo_empty_gpus())
    models <- native$devices[keep]
    memory <- native$memory_bytes[keep] / 1024^2
    vendor <- ifelse(grepl("NVIDIA", models, ignore.case = TRUE), "NVIDIA",
        ifelse(grepl("AMD|Radeon", models, ignore.case = TRUE), "AMD",
            ifelse(grepl("Intel", models, ignore.case = TRUE), "Intel",
                ifelse(grepl("Apple", models, ignore.case = TRUE), "Apple", NA_character_))))
    data.frame(id = seq_along(models) - 1L, vendor = vendor, model = models,
        memory_mb = memory, backend = "opencl", compute_capability = NA_character_,
        stringsAsFactors = FALSE)
}

#' Test whether OpenCL is available
#' @return A single `TRUE` or `FALSE`.
#' @export
has_opencl <- function() opencl_info()$available
