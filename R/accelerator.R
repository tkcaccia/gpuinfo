.gpuinfo_backend_status <- function(backend, info) {
    if (backend == "cpu") {
        return(c(status = "available", reason = "CPU fallback is always available"))
    }
    usable <- isTRUE(info$usable %||% info$available)
    if (usable) return(c(status = "available", reason = "Backend initialized successfully"))

    uncertain <- switch(backend,
        cuda = isTRUE(info$gpu) || isTRUE(info$driver),
        metal = isTRUE(info$platform) && (isTRUE(info$gpu) || isTRUE(info$framework)),
        rocm = isTRUE(info$gpu) || isTRUE(info$installed),
        opencl = isTRUE(info$loader),
        FALSE
    )
    reason <- switch(backend,
        cuda = if (uncertain) "NVIDIA hardware or driver found, but CUDA could not initialize" else "No usable NVIDIA GPU or CUDA driver was detected",
        metal = if (uncertain) "Metal components were found, but no usable device was confirmed" else "Metal is unavailable on this platform",
        rocm = if (uncertain) "AMD hardware or ROCm components found, but ROCm could not initialize" else "No usable AMD GPU or ROCm runtime was detected",
        opencl = if (uncertain) "An OpenCL loader was found, but no platform could be enumerated" else "No OpenCL loader or platform was detected"
    )
    c(status = if (uncertain) "unknown" else "unavailable", reason = reason)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Discover compute accelerators
#'
#' Returns a vendor-neutral representation of accelerators visible to the
#' current R process. CPU fallback information is available through
#' [backend_info()] and [cpu_info()].
#'
#' @return A data frame with stable columns describing each accelerator.
#' @export
accelerator_info <- function() {
    gpu <- gpu_info()
    if (!nrow(gpu)) {
        return(data.frame(
            id = integer(), type = character(), vendor = character(),
            model = character(), backend = character(),
            memory_total_bytes = numeric(), memory_free_bytes = numeric(),
            status = character(), reason = character(),
            stringsAsFactors = FALSE
        ))
    }
    memory <- gpu_memory_info("bytes")
    free <- rep(NA_real_, nrow(gpu))
    if (nrow(memory)) {
        for (i in seq_len(nrow(gpu))) {
            match_row <- which(memory$id == gpu$id[[i]])
            if (length(match_row)) free[[i]] <- memory$free[[match_row[[1L]]]]
        }
    }
    status_table <- backend_info()
    status <- reason <- rep(NA_character_, nrow(gpu))
    for (i in seq_len(nrow(gpu))) {
        row <- which(status_table$backend == gpu$backend[[i]])
        if (length(row)) {
            status[[i]] <- status_table$status[[row[[1L]]]]
            reason[[i]] <- status_table$reason[[row[[1L]]]]
        } else {
            status[[i]] <- "unknown"
            reason[[i]] <- "The device was detected without a confirmed compute backend"
        }
    }
    data.frame(
        id = gpu$id,
        type = rep("GPU", nrow(gpu)),
        vendor = gpu$vendor,
        model = gpu$model,
        backend = gpu$backend,
        memory_total_bytes = gpu$memory_mb * 1024^2,
        memory_free_bytes = free,
        status = status,
        reason = reason,
        stringsAsFactors = FALSE
    )
}

#' Inspect compute backends
#'
#' Reports three states: `"available"`, `"unavailable"`, and `"unknown"`.
#' The last state means relevant hardware or software was observed but a usable
#' backend could not be confirmed.
#'
#' @return A data frame with one row per supported backend.
#' @export
backend_info <- function() {
    registry <- .gpuinfo_backend_registry()
    details <- lapply(registry, function(entry) entry$info())
    state <- lapply(names(details), function(name) .gpuinfo_backend_status(name, details[[name]]))
    version <- c(
        details$cuda$toolkit_version %||% NA_character_,
        if (isTRUE(details$metal$platform)) unname(Sys.info()[["release"]]) else NA_character_,
        details$rocm$version %||% NA_character_,
        NA_character_, R.version$version.string
    )
    gpu <- gpu_info()
    counts <- c(
        sum(gpu$backend == "cuda", na.rm = TRUE),
        sum(gpu$backend == "metal", na.rm = TRUE),
        sum(gpu$backend == "rocm", na.rm = TRUE),
        details$opencl$device_count %||% NA_integer_, 1L
    )
    data.frame(
        backend = names(details),
        status = vapply(state, `[[`, character(1L), "status"),
        reason = vapply(state, `[[`, character(1L), "reason"),
        version = as.character(version),
        device_count = as.integer(counts),
        stringsAsFactors = FALSE
    )
}

#' Test whether a CPU is available
#' @return A single `TRUE` value.
#' @export
has_cpu <- function() TRUE
