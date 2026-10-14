.cuda_empty_devices <- function() {
    data.frame(
        logical_id = integer(), physical_id = integer(), uuid = character(),
        vendor = character(), model = character(),
        total_memory_bytes = numeric(), used_memory_bytes = numeric(),
        free_memory_bytes = numeric(), compute_capability_major = integer(),
        compute_capability_minor = integer(), compute_capability = character(),
        cuda_arch = integer(), backend = character(),
        detection_source = character(), visibility_status = character(),
        process_visible = logical(), stringsAsFactors = FALSE
    )
}

.cuda_normalize_uuid <- function(x) {
    x <- trimws(as.character(x))
    x[!nzchar(x) | tolower(x) %in% c("n/a", "na", "unknown", "[not supported]")] <- NA_character_
    toupper(x)
}

.cuda_capability_parts <- function(x) {
    x <- trimws(as.character(x))
    match <- regexec("^([0-9]+)\\.([0-9]+)$", x, perl = TRUE)
    pieces <- regmatches(x, match)
    major <- minor <- rep(NA_integer_, length(x))
    valid <- lengths(pieces) == 3L
    if (any(valid)) {
        major[valid] <- as.integer(vapply(pieces[valid], `[[`, character(1L), 2L))
        minor[valid] <- as.integer(vapply(pieces[valid], `[[`, character(1L), 3L))
    }
    list(
        major = major,
        minor = minor,
        text = ifelse(valid, paste0(major, ".", minor), NA_character_),
        arch = ifelse(valid, major * 10L + minor, NA_integer_)
    )
}

.cuda_parse_smi_devices <- function(lines, with_capability = TRUE) {
    expected <- if (with_capability) 7L else 6L
    pieces <- .gpuinfo_split_csv(lines, expected)
    if (!length(pieces)) return(.cuda_empty_devices())
    rows <- lapply(pieces, function(x) {
        physical_id <- suppressWarnings(as.integer(x[[1L]]))
        uuid <- .cuda_normalize_uuid(x[[2L]])
        model <- .gpuinfo_na(x[[3L]])
        memory <- suppressWarnings(as.numeric(x[4:6]))
        if (is.na(physical_id) || is.na(model) || is.na(memory[[1L]])) return(NULL)
        capability <- if (with_capability) .gpuinfo_na(x[[7L]]) else NA_character_
        parts <- .cuda_capability_parts(capability)
        data.frame(
            logical_id = NA_integer_, physical_id = physical_id, uuid = uuid,
            vendor = "NVIDIA", model = model,
            total_memory_bytes = memory[[1L]] * 1024^2,
            used_memory_bytes = memory[[2L]] * 1024^2,
            free_memory_bytes = memory[[3L]] * 1024^2,
            compute_capability_major = parts$major,
            compute_capability_minor = parts$minor,
            compute_capability = parts$text, cuda_arch = parts$arch,
            backend = "cuda", detection_source = "nvidia-smi",
            visibility_status = "physical_visibility_unknown",
            process_visible = FALSE, stringsAsFactors = FALSE
        )
    })
    rows <- rows[!vapply(rows, is.null, logical(1L))]
    if (!length(rows)) return(.cuda_empty_devices())
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}

.cuda_smi_devices <- function() {
    fields <- "index,uuid,name,memory.total,memory.used,memory.free,compute_cap"
    result <- .gpuinfo_command("nvidia-smi", c(
        paste0("--query-gpu=", fields), "--format=csv,noheader,nounits"
    ))
    if (result$ok) return(.cuda_parse_smi_devices(result$output, TRUE))
    fields <- "index,uuid,name,memory.total,memory.used,memory.free"
    result <- .gpuinfo_command("nvidia-smi", c(
        paste0("--query-gpu=", fields), "--format=csv,noheader,nounits"
    ))
    if (!result$ok) return(.cuda_empty_devices())
    .cuda_parse_smi_devices(result$output, FALSE)
}

.cuda_native_devices <- function(native = .gpuinfo_native_cuda()) {
    if (!isTRUE(native$initialized) || native$count < 1L)
        return(.cuda_empty_devices())
    n <- as.integer(native$count)
    recycle <- function(x, default, mode = c("character", "numeric", "integer")) {
        mode <- match.arg(mode)
        if (!length(x)) x <- rep(default, n)
        x <- rep_len(x, n)
        switch(mode, character = as.character(x), numeric = as.numeric(x),
               integer = as.integer(x))
    }
    capability <- recycle(native$compute_capability, NA_character_, "character")
    parts <- .cuda_capability_parts(capability)
    major <- recycle(native$compute_capability_major %||% parts$major,
                     NA_integer_, "integer")
    minor <- recycle(native$compute_capability_minor %||% parts$minor,
                     NA_integer_, "integer")
    known <- !is.na(major) & !is.na(minor)
    capability[known] <- paste0(major[known], ".", minor[known])
    bytes <- native$memory_bytes
    if (!length(bytes) && length(native$memory_mb))
        bytes <- as.numeric(native$memory_mb) * 1024^2
    data.frame(
        logical_id = seq_len(n) - 1L, physical_id = rep(NA_integer_, n),
        uuid = .cuda_normalize_uuid(recycle(native$uuids, NA_character_, "character")),
        vendor = rep("NVIDIA", n),
        model = recycle(native$names, NA_character_, "character"),
        total_memory_bytes = recycle(bytes, NA_real_, "numeric"),
        used_memory_bytes = rep(NA_real_, n), free_memory_bytes = rep(NA_real_, n),
        compute_capability_major = major, compute_capability_minor = minor,
        compute_capability = capability,
        cuda_arch = ifelse(known, major * 10L + minor, NA_integer_),
        backend = rep("cuda", n), detection_source = rep("cuda-driver", n),
        visibility_status = rep("process_visible", n),
        process_visible = rep(TRUE, n), stringsAsFactors = FALSE
    )
}

.cuda_reconcile_devices <- function(driver, physical, driver_initialized = FALSE) {
    if (!nrow(driver)) {
        if (nrow(physical)) {
            physical$visibility_status <- if (isTRUE(driver_initialized))
                "physical_only_not_process_visible" else "physical_visibility_unknown"
        }
        return(physical)
    }

    used <- rep(FALSE, nrow(physical))
    for (i in seq_len(nrow(driver))) {
        if (is.na(driver$uuid[[i]]) || !nrow(physical)) next
        hit <- which(!used & !is.na(physical$uuid) &
                     .cuda_normalize_uuid(physical$uuid) == driver$uuid[[i]])
        if (!length(hit)) next
        j <- hit[[1L]]
        used[[j]] <- TRUE
        driver$physical_id[[i]] <- physical$physical_id[[j]]
        if (is.na(driver$model[[i]])) driver$model[[i]] <- physical$model[[j]]
        if (is.na(driver$total_memory_bytes[[i]]))
            driver$total_memory_bytes[[i]] <- physical$total_memory_bytes[[j]]
        driver$used_memory_bytes[[i]] <- physical$used_memory_bytes[[j]]
        driver$free_memory_bytes[[i]] <- physical$free_memory_bytes[[j]]
        if (is.na(driver$compute_capability_major[[i]])) {
            driver$compute_capability_major[[i]] <- physical$compute_capability_major[[j]]
            driver$compute_capability_minor[[i]] <- physical$compute_capability_minor[[j]]
            driver$compute_capability[[i]] <- physical$compute_capability[[j]]
            driver$cuda_arch[[i]] <- physical$cuda_arch[[j]]
        }
        driver$detection_source[[i]] <- "cuda-driver+nvidia-smi-uuid"
    }

    extra <- physical[!used, , drop = FALSE]
    if (nrow(extra)) {
        extra$visibility_status <- "physical_only_not_process_visible"
        extra$process_visible <- FALSE
        extra$logical_id <- NA_integer_
    }
    out <- if (nrow(extra)) rbind(driver, extra) else driver
    rownames(out) <- NULL
    out
}

.cuda_driver_version <- function() {
    result <- .gpuinfo_command("nvidia-smi", c(
        "--query-gpu=driver_version", "--format=csv,noheader"
    ))
    if (!result$ok) return(NA_character_)
    values <- unique(vapply(result$output, .gpuinfo_na, character(1L)))
    values <- values[!is.na(values)]
    if (length(values)) values[[1L]] else NA_character_
}

.cuda_supported_version <- function() {
    result <- .gpuinfo_command("nvidia-smi")
    if (!result$ok) return(NA_character_)
    .gpuinfo_match(result$output, "CUDA Version:\\s*([0-9]+(?:\\.[0-9]+)+)")
}

.cuda_reason_codes <- function(hardware, native, runtime, static, visible_count) {
    codes <- character()
    if (!hardware) codes <- c(codes, "no_nvidia_hardware")
    if (!isTRUE(native$library)) codes <- c(codes, "cuda_driver_library_not_found")
    else if (!isTRUE(native$initialized)) codes <- c(codes, "cuda_driver_initialization_failed")
    else if (visible_count < 1L) codes <- c(codes, "no_visible_cuda_device")
    if (!isTRUE(runtime$library)) codes <- c(codes, "cuda_runtime_library_not_found")
    else if (!isTRUE(runtime$initialized)) codes <- c(codes, "cuda_runtime_initialization_failed")
    if (!isTRUE(static$toolkit_available)) codes <- c(codes, "cuda_toolkit_not_found")
    if (!isTRUE(static$compiler_available)) codes <- c(codes, "nvcc_not_found")
    unique(codes)
}

#' Inspect CUDA hardware and system components
#'
#' Independently reports NVIDIA hardware, the CUDA driver library and driver
#' initialization, the CUDA runtime library and runtime initialization, the
#' Toolkit, and `nvcc`. Devices visible to the process are obtained through the
#' CUDA driver API and reconciled with physical `nvidia-smi` devices by UUID.
#' The `libraries` component is the consolidated table returned by
#' [cuda_libraries()].
#'
#' `driver_supported_cuda_version` is the maximum CUDA version advertised by
#' the installed driver. It is not the installed runtime or Toolkit version.
#' `runtime_version` comes only from `cudaRuntimeGetVersion()` in a dynamically
#' loaded CUDA runtime library.
#' `driver_initialization_status` records `cuInit()` separately from
#' `driver_device_count_status`, which records `cuDeviceGetCount()`. Therefore
#' a successfully initialized driver can still have an unavailable device
#' count, and neither state alone implies that a CUDA device is visible.
#'
#' @param refresh Re-run cached static Toolkit and compiler discovery.
#' @return A named list containing component states, versions, a CUDA-library
#'   table, status codes, visible and physical device information, and
#'   compatibility aliases.
#' @export
cuda_info <- function(refresh = FALSE) {
    native <- .gpuinfo_native_cuda()
    physical <- .cuda_smi_devices()
    driver_devices <- .cuda_native_devices(native)
    devices <- .cuda_reconcile_devices(
        driver_devices, physical, isTRUE(native$initialized)
    )
    static <- .cuda_static_info(refresh = refresh)
    runtime <- .gpuinfo_native_cudart(static$runtime_candidates)
    libraries <- cuda_libraries(refresh = refresh)

    visible <- devices[devices$process_visible %in% TRUE, , drop = FALSE]
    visible_count <- nrow(visible)
    hardware_visible <- nrow(devices) > 0L
    driver_library <- isTRUE(native$library)
    driver_initialized <- isTRUE(native$initialized)
    driver_usable <- driver_library && driver_initialized && visible_count > 0L
    runtime_library <- isTRUE(runtime$library)
    runtime_initialized <- isTRUE(runtime$initialized)
    runtime_version <- .gpuinfo_cuda_version(runtime$runtime_version)
    runtime_version_status <- if (!runtime_library) {
        "runtime_library_not_found"
    } else if (is.na(runtime$version_query_status)) {
        "runtime_version_symbol_not_found"
    } else if (!identical(as.integer(runtime$version_query_status), 0L)) {
        "runtime_version_query_failed"
    } else if (is.na(runtime_version)) {
        "runtime_version_unavailable"
    } else {
        "available"
    }
    driver_version <- .cuda_driver_version()
    if (is.na(driver_version))
        driver_version <- .gpuinfo_cuda_version(native$driver_version)
    libraries$version[libraries$component == "driver"] <- driver_version
    libraries$version[libraries$component == "runtime"] <- runtime_version
    reason_codes <- .cuda_reason_codes(
        hardware_visible, native, runtime, static, visible_count
    )

    list(
        hardware_visible = .gpuinfo_bool(hardware_visible),
        driver_library = .gpuinfo_bool(driver_library),
        driver_initialized = .gpuinfo_bool(driver_initialized),
        driver_usable = .gpuinfo_bool(driver_usable),
        driver_initialization_status = native$init_status %||% NA_integer_,
        driver_device_count_status = native$count_status %||% NA_integer_,
        driver_version = driver_version,
        driver_supported_cuda_version = .cuda_supported_version(),
        runtime_library = .gpuinfo_bool(runtime_library),
        runtime_library_path = runtime$library_path %||% NA_character_,
        runtime_initialized = .gpuinfo_bool(runtime_initialized),
        runtime_usable = .gpuinfo_bool(runtime_library && runtime_initialized),
        runtime_initialization_status = runtime$init_status %||% NA_integer_,
        runtime_version = runtime_version,
        runtime_version_status = runtime_version_status,
        toolkit_available = .gpuinfo_bool(static$toolkit_available),
        toolkit_root = static$toolkit_root,
        toolkit_version = static$toolkit_version,
        compiler_available = .gpuinfo_bool(static$compiler_available),
        nvcc_path = static$nvcc_path,
        compiler_version = static$nvcc_version,
        libraries = libraries,
        devices = devices,
        physical_devices = physical,
        visible_device_count = as.integer(visible_count),
        compute_capability = visible$compute_capability,
        cuda_architectures = visible$cuda_arch,
        cuda_visible_devices = Sys.getenv("CUDA_VISIBLE_DEVICES", unset = NA_character_),
        status = if (driver_usable) "available" else if (hardware_visible || driver_library)
            "partial" else "unavailable",
        diagnostic_code = if (length(reason_codes)) reason_codes[[1L]] else "cuda_available",
        reason_codes = reason_codes,
        gpu = .gpuinfo_bool(hardware_visible),
        driver = .gpuinfo_bool(driver_library),
        runtime = .gpuinfo_bool(runtime_library),
        toolkit = .gpuinfo_bool(static$toolkit_available),
        nvcc = .gpuinfo_bool(static$compiler_available),
        usable = .gpuinfo_bool(driver_usable)
    )
}

#' Test whether CUDA appears usable
#'
#' Returns `TRUE` only when at least one CUDA device is visible to the current
#' process through a successfully initialized NVIDIA CUDA driver. The CUDA
#' runtime, Toolkit, `nvcc`, FAISS, cuVS, and machine-learning frameworks are
#' independent and are not required.
#'
#' @return One non-missing logical value.
#' @examples
#' has_cuda()
#' cuda_info()$devices
#' @seealso [cuda_info()], [hardware_info()], [has_gpu()]
#' @export
has_cuda <- function() cuda_info()$driver_usable

has_nvidia_gpu <- function() cuda_info()$hardware_visible

has_cuda_driver <- function() cuda_info()$driver_library

has_cuda_runtime <- function() cuda_info()$runtime_library

has_cuda_toolkit <- function() cuda_info()$toolkit_available

has_nvcc <- function() cuda_info()$compiler_available

# Compatibility wrappers used by older internal code and tests.
.cuda_parse_smi_rows <- function(lines, with_capability = TRUE) {
    pieces <- .gpuinfo_split_csv(lines, if (with_capability) 4L else 3L)
    if (!length(pieces)) return(.gpuinfo_empty_gpus())
    rows <- lapply(pieces, function(x) data.frame(
        id = suppressWarnings(as.integer(x[[1L]])), vendor = "NVIDIA",
        model = .gpuinfo_na(x[[2L]]),
        memory_mb = suppressWarnings(as.numeric(x[[3L]])), backend = "cuda",
        compute_capability = if (with_capability) .gpuinfo_na(x[[4L]]) else NA_character_,
        stringsAsFactors = FALSE
    ))
    do.call(rbind, rows)
}

.cuda_native_rows <- function(native = .gpuinfo_native_cuda()) {
    rows <- .cuda_native_devices(native)
    data.frame(
        id = rows$logical_id, vendor = rows$vendor, model = rows$model,
        memory_mb = rows$total_memory_bytes / 1024^2, backend = rows$backend,
        compute_capability = rows$compute_capability, stringsAsFactors = FALSE
    )
}

.cuda_rows <- function() {
    native <- .cuda_native_rows()
    if (nrow(native)) native else .cuda_parse_smi_rows_from_query()
}

.cuda_parse_smi_rows_from_query <- function() {
    rows <- .cuda_smi_devices()
    data.frame(
        id = rows$physical_id, vendor = rows$vendor, model = rows$model,
        memory_mb = rows$total_memory_bytes / 1024^2, backend = rows$backend,
        compute_capability = rows$compute_capability, stringsAsFactors = FALSE
    )
}

.cuda_nvcc_version <- function() .cuda_static_info()$nvcc_version

.cuda_parse_memory_rows <- function(lines) {
    pieces <- .gpuinfo_split_csv(lines, 5L)
    if (!length(pieces)) return(data.frame())
    rows <- lapply(pieces, function(x) data.frame(
        id = suppressWarnings(as.integer(x[[1L]])), model = .gpuinfo_na(x[[2L]]),
        total = suppressWarnings(as.numeric(x[[3L]])),
        used = suppressWarnings(as.numeric(x[[4L]])),
        free = suppressWarnings(as.numeric(x[[5L]])), stringsAsFactors = FALSE
    ))
    do.call(rbind, rows)
}

.cuda_memory_rows <- function() {
    result <- .gpuinfo_command("nvidia-smi", c(
        "--query-gpu=index,name,memory.total,memory.used,memory.free",
        "--format=csv,noheader,nounits"
    ))
    if (!result$ok) return(data.frame())
    .cuda_parse_memory_rows(result$output)
}
