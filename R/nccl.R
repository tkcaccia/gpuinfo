.nccl_supported_platform <- function() {
    identical(Sys.info()[["sysname"]], "Linux")
}

.nccl_candidates <- function(refresh = FALSE) {
    if (!.nccl_supported_platform())
        return(character())
    explicit <- Sys.getenv("NCCL_LIBRARY", unset = "")
    roots <- unique(c(
        Sys.getenv("NCCL_HOME", unset = ""),
        Sys.getenv("NCCL_ROOT", unset = "")
    ))
    roots <- roots[nzchar(roots)]
    dirs <- .cuda_static_info(refresh = refresh)$library_dirs
    if (length(roots))
        dirs <- c(unlist(lapply(roots, file.path, c("lib", "lib64")),
                         use.names = FALSE), dirs)
    dirs <- unique(dirs[dir.exists(dirs)])
    files <- unlist(lapply(dirs, function(dir) {
        list.files(dir, "^libnccl\\.so(\\.[0-9]+)*$",
            full.names = TRUE)
    }), use.names = FALSE)
    unique(c(explicit[nzchar(explicit)], files, "libnccl.so.2", "libnccl.so"))
}

.nccl_version_string <- function(value) {
    if (!length(value))
        return(NA_character_)
    value <- suppressWarnings(as.integer(value[[1L]]))
    if (is.na(value) || value <= 0L)
        return(NA_character_)
    if (value >= 10000L) {
        major <- value %/% 10000L
        minor <- (value %/% 100L) %% 100L
    } else {
        major <- value %/% 1000L
        minor <- (value %/% 100L) %% 10L
    }
    patch <- value %% 100L
    paste(major, minor, patch, sep = ".")
}

#' Inspect the NCCL shared library
#'
#' Loads the NCCL shared library and calls `ncclGetVersion()` without
#' creating a communicator or changing device selection. A successful
#' version query does not prove that two GPUs can communicate. Packages
#' using NCCL must still validate their own build and communicator.
#' The probe is supported on Linux, where it also checks `NCCL_LIBRARY`,
#' `NCCL_HOME`, and `NCCL_ROOT` before normal CUDA and system library paths.
#'
#' @param refresh Re-run cached static CUDA path discovery. This does not alter
#'   the environment or install software.
#' @return A list with library presence, loaded path or soname, decoded and
#'   integer library versions, version-query status, structured diagnostic
#'   fields, and the number of process-visible CUDA devices.
#'   `communication_tested` is always `FALSE`.
#' @seealso [cuda_info()], [hardware_info()]
#' @export
nccl_info <- function(refresh = FALSE) {
    cuda <- cuda_info(refresh = refresh)
    .nccl_info_from_cuda(cuda, refresh = refresh)
}

.nccl_info_from_cuda <- function(cuda, refresh = FALSE) {
    supported <- .nccl_supported_platform()
    native <- .gpuinfo_native_nccl(.nccl_candidates(refresh = refresh))
    query_status <- if (length(native$version_status)) {
        as.integer(native$version_status[[1L]])
    } else NA_integer_
    version_integer <- if (length(native$version_integer)) {
        as.integer(native$version_integer[[1L]])
    } else NA_integer_
    version <- .nccl_version_string(version_integer)
    if (!supported) {
        status <- "unavailable"
        code <- "nccl_unsupported_operating_system"
        reasons <- code
    } else if (!isTRUE(native$library)) {
        status <- "unavailable"
        code <- "nccl_library_not_found"
        reasons <- code
    } else if (is.na(query_status)) {
        status <- "partial"
        code <- "nccl_version_symbol_not_found"
        reasons <- code
    } else if (!identical(as.integer(query_status), 0L)) {
        status <- "partial"
        code <- "nccl_version_query_failed"
        reasons <- code
    } else if (is.na(version)) {
        status <- "partial"
        code <- "nccl_version_unavailable"
        reasons <- code
    } else {
        status <- "available"
        code <- "ok"
        reasons <- character()
    }
    count <- as.integer(cuda$visible_device_count)
    list(
        library = isTRUE(native$library),
        library_path = native$library_path,
        version = version,
        version_integer = version_integer,
        version_query_status = query_status,
        visible_cuda_device_count = count,
        visible_device_count = count,
        multiple_visible_cuda_devices = count > 1L,
        status = status,
        diagnostic_code = code,
        reason_codes = reasons,
        communication_tested = FALSE
    )
}
