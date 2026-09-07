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
#' @noRd
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
#'
#' Returns `TRUE` on macOS when a Metal-capable graphics device is visible.
#' Detection uses the Metal framework when it can be loaded and supplements it
#' with `system_profiler` and Apple-silicon platform information.
#'
#' This reports operating-system and hardware capability, not framework-specific
#' support such as torch MPS. A positive result therefore does not guarantee
#' that another R package can execute work through Metal. Detailed fields are
#' available in `hardware_info()$metal`.
#'
#' @return One non-missing logical value: `TRUE` when Metal appears usable,
#'   otherwise `FALSE`. It is `FALSE` on non-macOS platforms.
#'
#' @examples
#' has_metal()
#' hardware_info()$metal
#'
#' @seealso [hardware_info()], [has_gpu()]
#' @export
has_metal <- function() metal_info()$usable
