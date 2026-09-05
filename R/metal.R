.metal_profiler <- function() {
    if (!identical(Sys.info()[["sysname"]], "Darwin")) return(character())
    .gpuinfo_cached("metal_profiler", function() {
        result <- .gpuinfo_command("system_profiler", c("SPDisplaysDataType", "-detailLevel", "mini"), timeout = 15L)
        if (result$ok) result$output else character()
    })
}

.metal_gpus <- function() {
    lines <- .metal_profiler()
    chip_lines <- grep("^\\s*(Chipset Model|Chip):", lines, value = TRUE)
    if (!length(chip_lines)) return(.gpuinfo_empty_gpus())
    models <- trimws(sub("^[^:]+:", "", chip_lines))
    memory <- rep(NA_real_, length(models))
    vram <- grep("^\\s*(VRAM|Total Number of Cores):", lines, value = TRUE)
    vram <- grep("VRAM", vram, value = TRUE)
    if (length(vram)) {
        nums <- suppressWarnings(as.numeric(.gpuinfo_match(vram, "([0-9.]+)\\s*(?:MB|GB)", 1L)))
        is_gb <- grepl("GB", vram, ignore.case = TRUE)
        nums[is_gb] <- nums[is_gb] * 1024
        memory[seq_len(min(length(memory), length(nums)))] <- nums[seq_len(min(length(memory), length(nums)))]
    }
    vendor <- ifelse(grepl("Apple", models, ignore.case = TRUE), "Apple",
        ifelse(grepl("AMD|Radeon", models, ignore.case = TRUE), "AMD",
            ifelse(grepl("Intel", models, ignore.case = TRUE), "Intel", NA_character_)))
    data.frame(
        id = seq_along(models) - 1L,
        vendor = vendor,
        model = models,
        memory_mb = memory,
        backend = "metal",
        compute_capability = NA_character_,
        stringsAsFactors = FALSE
    )
}

#' Inspect Apple Metal availability
#'
#' Metal detection is based on macOS hardware and system information, not on
#' framework-specific MPS support.
#'
#' @return A named list describing the platform, GPUs, Metal support, and
#'   apparent usability.
#' @export
metal_info <- function() {
    native <- .gpuinfo_native_metal()
    macos <- identical(Sys.info()[["sysname"]], "Darwin")
    gpus <- .metal_gpus()
    profiler <- .metal_profiler()
    apple_silicon <- macos && identical(Sys.info()[["machine"]], "arm64")
    reported <- any(grepl("Metal.*Supported|Metal Support", profiler, ignore.case = TRUE))
    supported <- native$device || apple_silicon || reported
    list(
        platform = macos,
        apple_silicon = .gpuinfo_bool(apple_silicon),
        gpu = nrow(gpus) > 0L,
        devices = gpus,
        framework = .gpuinfo_bool(native$framework),
        supported = .gpuinfo_bool(supported),
        usable = .gpuinfo_bool(macos && supported && (native$device || nrow(gpus) > 0L || apple_silicon))
    )
}

#' Test whether Metal appears usable
#' @return A single `TRUE` or `FALSE`.
#' @export
has_metal <- function() metal_info()$usable
