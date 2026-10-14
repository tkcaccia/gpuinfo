.gpu_generic_linux <- function() {
    if (!identical(Sys.info()[["sysname"]], "Linux")) return(.gpuinfo_empty_gpus())
    result <- .gpuinfo_command("lspci")
    if (!result$ok) return(.gpuinfo_empty_gpus())
    lines <- result$output[grepl("VGA compatible controller|3D controller|Display controller", result$output, ignore.case = TRUE)]
    if (!length(lines)) return(.gpuinfo_empty_gpus())
    model <- trimws(sub("^.*(?:VGA compatible controller|3D controller|Display controller):\\s*", "", lines, perl = TRUE, ignore.case = TRUE))
    vendor <- .gpuinfo_gpu_vendor(model)
    backend <- ifelse(vendor == "NVIDIA" & has_cuda(), "cuda",
        ifelse(vendor == "AMD" & has_rocm(), "rocm", NA_character_))
    backend[is.na(backend) & !is.na(vendor) & has_opencl()] <- "opencl"
    data.frame(id = seq_along(lines) - 1L, vendor = vendor, model = model,
        memory_mb = NA_real_, backend = backend, compute_capability = NA_character_,
        stringsAsFactors = FALSE)
}

.gpu_generic_windows <- function() {
    if (!identical(Sys.info()[["sysname"]], "Windows")) return(.gpuinfo_empty_gpus())
    command <- paste(
        "$x=Get-CimInstance Win32_VideoController;",
        "$x|ForEach-Object {([string]$_.AdapterRAM)+'|'+([string]$_.Name)}")
    result <- .gpuinfo_command("powershell", c("-NoProfile", "-NonInteractive", "-Command", command), timeout = 10L)
    parsed <- if (result$ok) .gpuinfo_parse_windows_gpus(result$output, "|") else NULL
    if (is.null(parsed) || !length(parsed$model)) {
        result <- .gpuinfo_command("wmic", c("path", "win32_VideoController", "get",
                                              "Name,AdapterRAM", "/format:csv"))
        parsed <- if (result$ok) .gpuinfo_parse_windows_gpus(result$output, ",") else NULL
    }
    if (is.null(parsed) || !length(parsed$model)) return(.gpuinfo_empty_gpus())
    models <- parsed$model
    bytes <- parsed$bytes
    vendor <- .gpuinfo_gpu_vendor(models)
    backend <- ifelse(vendor == "NVIDIA" & has_cuda(), "cuda",
        ifelse(vendor == "AMD" & has_rocm(), "rocm",
            ifelse(!is.na(vendor) & has_opencl(), "opencl", NA_character_)))
    data.frame(id = seq_along(models) - 1L, vendor = vendor, model = models,
        memory_mb = bytes / 1024^2, backend = backend, compute_capability = NA_character_,
        stringsAsFactors = FALSE)
}

.gpuinfo_parse_windows_gpus <- function(lines, separator = "|") {
    lines <- lines[grepl(separator, lines, fixed = TRUE)]
    lines <- lines[!grepl("AdapterRAM", lines, fixed = TRUE)]
    parsed <- lapply(lines, function(line) trimws(strsplit(line, separator, fixed = TRUE)[[1L]]))
    parsed <- parsed[lengths(parsed) >= 2L]
    if (!length(parsed)) return(list(model = character(), bytes = numeric()))
    list(
        model = vapply(parsed, function(x) utils::tail(x, 1L), character(1L)),
        bytes = suppressWarnings(as.numeric(vapply(
            parsed, function(x) x[[length(x) - 1L]], character(1L))))
    )
}

.gpuinfo_gpu_vendor <- function(model) {
    ifelse(grepl("\\bNVIDIA\\b", model, perl = TRUE, ignore.case = TRUE), "NVIDIA",
        ifelse(grepl("\\bAMD\\b|\\bATI\\b|Advanced Micro Devices|\\bRadeon\\b",
                     model, perl = TRUE, ignore.case = TRUE), "AMD",
            ifelse(grepl("\\bIntel\\b", model, perl = TRUE, ignore.case = TRUE),
                   "Intel", NA_character_)))
}

#' Detect graphics processors
#'
#' @return A data frame with one row per detected GPU and stable columns `id`,
#'   `vendor`, `model`, `memory_mb`, `backend`, and `compute_capability`.
#'   Unknown values are represented by `NA`.
#' @noRd
gpu_info <- function() {
    os <- Sys.info()[["sysname"]]
    gpus <- if (identical(os, "Darwin")) {
        .metal_gpus()
    } else {
        nvidia <- .cuda_rows()
        generic <- if (identical(os, "Linux")) .gpu_generic_linux() else .gpu_generic_windows()
        rocm <- .rocm_native_rows()
        opencl <- .opencl_gpu_rows()
        if (nrow(nvidia)) {
            generic <- generic[is.na(generic$vendor) | generic$vendor != "NVIDIA", , drop = FALSE]
        }
        if (nrow(rocm)) {
            if (any(generic$vendor == "AMD", na.rm = TRUE)) rocm <- .gpuinfo_empty_gpus()
        }
        if (nrow(opencl)) {
            opencl_vendors <- unique(opencl$vendor[!is.na(opencl$vendor)])
            generic <- generic[is.na(generic$vendor) | !(
                generic$vendor %in% opencl_vendors & generic$backend == "opencl"
            ), , drop = FALSE]
        }
        represented <- unique(c(nvidia$vendor, generic$vendor, rocm$vendor))
        opencl <- opencl[is.na(opencl$vendor) | !opencl$vendor %in% represented, , drop = FALSE]
        pieces <- list(nvidia, generic, rocm, opencl)
        pieces <- pieces[vapply(pieces, nrow, integer(1L)) > 0L]
        if (length(pieces)) do.call(rbind, pieces) else .gpuinfo_empty_gpus()
    }
    if (!nrow(gpus)) return(.gpuinfo_empty_gpus())
    gpus$id <- seq_len(nrow(gpus)) - 1L
    rownames(gpus) <- NULL
    gpus
}

#' List detected GPU devices
#'
#' Returns process-visible CUDA devices and, when `nvidia-smi` is available,
#' physical NVIDIA devices that are not visible to the current process. CUDA
#' driver and `nvidia-smi` records are reconciled only by UUID. Other vendors
#' are reported through the available native or operating-system probes.
#'
#' @param refresh Re-run cached static CUDA discovery used while constructing
#'   the device report. Dynamic NVIDIA memory values are always queried again.
#' @return A data frame with one row per detected device. Memory values are
#'   bytes. `logical_id` is the CUDA process-visible ordinal; `physical_id` is
#'   the `nvidia-smi` index and may differ after visibility filtering or
#'   reordering. `visibility_status` distinguishes process-visible devices from
#'   physical-only observations.
#' @export
gpu_devices <- function(refresh = FALSE) {
    cuda <- cuda_info(refresh = refresh)
    .gpuinfo_devices_from_cuda(cuda$devices,
                              driver_initialized = isTRUE(cuda$driver_initialized))
}

.gpuinfo_devices_from_cuda <- function(cuda, driver_initialized = FALSE) {
    legacy <- gpu_info()
    if (nrow(cuda) || isTRUE(driver_initialized)) {
        legacy <- legacy[
            is.na(legacy$vendor) | legacy$vendor != "NVIDIA", , drop = FALSE
        ]
    }
    if (nrow(legacy)) {
        parts <- .cuda_capability_parts(legacy$compute_capability)
        other <- data.frame(
            logical_id = legacy$id, physical_id = rep(NA_integer_, nrow(legacy)),
            uuid = rep(NA_character_, nrow(legacy)), vendor = legacy$vendor,
            model = legacy$model,
            total_memory_bytes = legacy$memory_mb * 1024^2,
            used_memory_bytes = rep(NA_real_, nrow(legacy)),
            free_memory_bytes = rep(NA_real_, nrow(legacy)),
            compute_capability_major = parts$major,
            compute_capability_minor = parts$minor,
            compute_capability = parts$text, cuda_arch = parts$arch,
            backend = legacy$backend,
            detection_source = ifelse(
                legacy$backend == "metal", "metal-system-profiler",
                ifelse(legacy$backend == "rocm", "rocm", "os-or-opencl")
            ),
            visibility_status = rep("process_visible", nrow(legacy)),
            process_visible = rep(TRUE, nrow(legacy)), stringsAsFactors = FALSE
        )
    } else {
        other <- .cuda_empty_devices()
    }
    pieces <- list(cuda, other)
    pieces <- pieces[vapply(pieces, nrow, integer(1L)) > 0L]
    if (!length(pieces)) return(.cuda_empty_devices())
    out <- do.call(rbind, pieces)
    rownames(out) <- NULL
    out
}

#' Test whether a GPU is detected
#'
#' Returns `TRUE` when at least one graphics processor is visible to the current
#' R process. Detection combines native vendor-library queries with portable
#' operating-system and command-line fallbacks when available.
#'
#' This tests hardware visibility, not whether any particular compute backend
#' or R framework can use the device. A GPU with a missing driver can therefore
#' be detected while all backend predicates remain `FALSE`. Conversely, a
#' container or scheduler can hide a physical GPU from the process.
#'
#' @return One non-missing logical value: `TRUE` when a visible GPU is detected,
#'   otherwise `FALSE`.
#'
#' @examples
#' has_gpu()
#'
#' @seealso [hardware_info()], [has_cuda()], [has_metal()], [has_rocm()],
#'   [has_opencl()]
#' @export
has_gpu <- function() {
    devices <- gpu_devices()
    nrow(devices) > 0L && any(devices$process_visible %in% TRUE)
}

gpu_count <- function() as.integer(nrow(gpu_info()))

gpu_memory <- function(unit = c("bytes", "GB", "GiB", "MB", "MiB")) {
    unit <- match.arg(unit)
    info <- gpu_info()
    values <- info$memory_mb
    values <- switch(unit, bytes = values * 1024^2, GB = values * 1024^2 / 1e9,
                     GiB = values / 1024, MB = values * 1024^2 / 1e6, MiB = values)
    names(values) <- info$model
    values
}

#' Report total, used, and free GPU memory
#'
#' Total memory is a static device property. Used and free memory are dynamic
#' snapshots queried from `nvidia-smi` when available and are not cached.
#' Other backends retain known total memory and return `NA` for dynamic values.
#' These are device-wide values, not memory attributed to the current process;
#' use [gpu_memory_sample()] for process-level benchmark observations.
#'
#' @param refresh Re-run cached static CUDA discovery. Dynamic memory is always
#'   refreshed regardless of this argument.
#' @return A data frame whose memory columns are explicitly measured in bytes.
#' @export
gpu_memory_info <- function(refresh = FALSE) {
    devices <- gpu_devices(refresh = refresh)
    .gpuinfo_memory_info_from_devices(devices)
}

.gpuinfo_memory_info_from_devices <- function(devices) {
    devices[c(
        "logical_id", "physical_id", "uuid", "vendor", "model",
        "total_memory_bytes", "used_memory_bytes", "free_memory_bytes",
        "backend", "detection_source", "visibility_status", "process_visible"
    )]
}
