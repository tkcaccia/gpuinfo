.gpu_generic_linux <- function() {
    if (!identical(Sys.info()[["sysname"]], "Linux")) return(.gpuinfo_empty_gpus())
    result <- .gpuinfo_command("lspci")
    if (!result$ok) return(.gpuinfo_empty_gpus())
    lines <- result$output[grepl("VGA compatible controller|3D controller|Display controller", result$output, ignore.case = TRUE)]
    if (!length(lines)) return(.gpuinfo_empty_gpus())
    model <- trimws(sub("^.*(?:VGA compatible controller|3D controller|Display controller):\\s*", "", lines, perl = TRUE, ignore.case = TRUE))
    vendor <- ifelse(grepl("NVIDIA", model, ignore.case = TRUE), "NVIDIA",
        ifelse(grepl("AMD|ATI|Advanced Micro Devices", model, ignore.case = TRUE), "AMD",
            ifelse(grepl("Intel", model, ignore.case = TRUE), "Intel", NA_character_)))
    backend <- ifelse(vendor == "NVIDIA" & has_cuda(), "cuda",
        ifelse(vendor == "AMD" & has_rocm(), "rocm", NA_character_))
    backend[is.na(backend) & !is.na(vendor) & has_opencl()] <- "opencl"
    data.frame(id = seq_along(lines) - 1L, vendor = vendor, model = model,
        memory_mb = NA_real_, backend = backend, compute_capability = NA_character_,
        stringsAsFactors = FALSE)
}

.gpu_generic_windows <- function() {
    if (!identical(Sys.info()[["sysname"]], "Windows")) return(.gpuinfo_empty_gpus())
    result <- .gpuinfo_command("wmic", c("path", "win32_VideoController", "get", "Name,AdapterRAM", "/format:csv"))
    if (!result$ok) return(.gpuinfo_empty_gpus())
    lines <- result$output[grepl(",", result$output, fixed = TRUE)]
    lines <- lines[!grepl("AdapterRAM", lines, fixed = TRUE)]
    if (!length(lines)) return(.gpuinfo_empty_gpus())
    parsed <- lapply(lines, function(line) trimws(strsplit(line, ",", fixed = TRUE)[[1L]]))
    parsed <- parsed[lengths(parsed) >= 3L]
    if (!length(parsed)) return(.gpuinfo_empty_gpus())
    models <- vapply(parsed, function(x) utils::tail(x, 1L), character(1L))
    bytes <- suppressWarnings(as.numeric(vapply(parsed, function(x) x[[length(x) - 1L]], character(1L))))
    vendor <- ifelse(grepl("NVIDIA", models, ignore.case = TRUE), "NVIDIA",
        ifelse(grepl("AMD|Radeon", models, ignore.case = TRUE), "AMD",
            ifelse(grepl("Intel", models, ignore.case = TRUE), "Intel", NA_character_)))
    backend <- ifelse(vendor == "NVIDIA" & has_cuda(), "cuda",
        ifelse(vendor == "AMD" & has_rocm(), "rocm",
            ifelse(!is.na(vendor) & has_opencl(), "opencl", NA_character_)))
    data.frame(id = seq_along(models) - 1L, vendor = vendor, model = models,
        memory_mb = bytes / 1024^2, backend = backend, compute_capability = NA_character_,
        stringsAsFactors = FALSE)
}

#' Detect graphics processors
#'
#' @return A data frame with one row per detected GPU and stable columns `id`,
#'   `vendor`, `model`, `memory_mb`, `backend`, and `compute_capability`.
#'   Unknown values are represented by `NA`.
#' @export
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

#' GPU summary helpers
#'
#' @param unit Unit used for memory values.
#' @return `has_gpu()` returns one logical value. `gpu_count()` returns a
#'   non-negative integer. `gpu_memory()` returns a named numeric vector of
#'   total memory sizes (possibly `NA`).
#' @name gpu_helpers
NULL

#' @rdname gpu_helpers
#' @export
has_gpu <- function() nrow(gpu_info()) > 0L

#' @rdname gpu_helpers
#' @export
gpu_count <- function() as.integer(nrow(gpu_info()))

#' @rdname gpu_helpers
#' @export
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
#' Dynamic usage is currently reported when `nvidia-smi` exposes it. Other
#' backends retain known total memory and return `NA` for used/free memory.
#'
#' @param unit Unit used for memory values.
#' @return A data frame with one row per detected GPU.
#' @export
gpu_memory_info <- function(unit = c("bytes", "GB", "GiB", "MB", "MiB")) {
    unit <- match.arg(unit)
    gpu <- gpu_info()
    out <- data.frame(id = gpu$id, model = gpu$model, total = gpu$memory_mb,
        used = rep(NA_real_, nrow(gpu)), free = rep(NA_real_, nrow(gpu)),
        stringsAsFactors = FALSE)
    nvidia <- .cuda_memory_rows()
    if (nrow(nvidia)) {
        for (i in seq_len(nrow(out))) {
            hit <- which(nvidia$id == out$id[[i]] & nvidia$model == out$model[[i]])
            if (length(hit)) {
                out$total[[i]] <- nvidia$total[[hit[[1L]]]]
                out$used[[i]] <- nvidia$used[[hit[[1L]]]]
                out$free[[i]] <- nvidia$free[[hit[[1L]]]]
            }
        }
    }
    factor <- switch(unit, bytes = 1024^2, GB = 1024^2 / 1e9,
                     GiB = 1 / 1024, MB = 1024^2 / 1e6, MiB = 1)
    out[c("total", "used", "free")] <- out[c("total", "used", "free")] * factor
    attr(out, "unit") <- unit
    out
}
