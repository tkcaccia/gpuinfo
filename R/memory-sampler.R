.gpuinfo_empty_memory_sample <- function() {
    data.frame(
        sample_time = character(), pid = integer(), logical_id = integer(),
        physical_id = integer(), uuid = character(), vendor = character(),
        model = character(), backend = character(),
        total_memory_bytes = numeric(), used_memory_bytes = numeric(),
        free_memory_bytes = numeric(), measurement_scope = character(),
        measurement_source = character(), measurement_status = character(),
        reason_code = character(), stringsAsFactors = FALSE
    )
}

.cuda_parse_process_memory <- function(lines) {
    pieces <- .gpuinfo_split_csv(lines, 3L)
    if (!length(pieces)) {
        return(data.frame(
            pid = integer(), uuid = character(), used_memory_bytes = numeric(),
            stringsAsFactors = FALSE
        ))
    }
    rows <- lapply(pieces, function(x) {
        process_id <- suppressWarnings(as.integer(x[[1L]]))
        uuid <- .cuda_normalize_uuid(x[[2L]])
        mib <- suppressWarnings(as.numeric(x[[3L]]))
        if (is.na(process_id) || is.na(uuid)) return(NULL)
        data.frame(
            pid = process_id, uuid = uuid,
            used_memory_bytes = mib * 1024^2,
            stringsAsFactors = FALSE
        )
    })
    rows <- rows[!vapply(rows, is.null, logical(1L))]
    if (!length(rows)) {
        return(data.frame(
            pid = integer(), uuid = character(), used_memory_bytes = numeric(),
            stringsAsFactors = FALSE
        ))
    }
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}

.cuda_process_memory <- function() {
    result <- .gpuinfo_command("nvidia-smi", c(
        "--query-compute-apps=pid,gpu_uuid,used_gpu_memory",
        "--format=csv,noheader,nounits"
    ))
    list(
        ok = isTRUE(result$ok),
        rows = if (isTRUE(result$ok)) .cuda_parse_process_memory(result$output) else
            .cuda_parse_process_memory(character()),
        status = result$status %||% NA_integer_
    )
}

#' Sample process-level GPU memory
#'
#' Takes one timestamped, non-cached memory sample for one or more processes.
#' CUDA process accounting is queried once for all requested PIDs, so the
#' returned device rows share an observation time. Call the function repeatedly
#' from benchmark code to construct a time series. The function does not run a
#' background monitor and does not synchronize GPU work.
#'
#' CUDA memory is read from `nvidia-smi`'s compute-process table and matched to
#' process-visible devices by UUID. It represents memory attributed by the
#' NVIDIA driver to the selected process. On Windows in WDDM mode, NVIDIA may
#' report per-process memory as unavailable; in that case the value is `NA` and
#' `reason_code` explains why. A successful query with no row for the process
#' is reported as zero bytes with status `not_active`.
#'
#' Metal memory uses `MTLDevice.currentAllocatedSize` for the current R process.
#' This is the current size of Metal resources allocated by this process; it is
#' not resident memory, peak memory, total unified memory, or memory allocated
#' by other processes. Metal cannot inspect an arbitrary PID through this API.
#'
#' Values for unsupported or unavailable measurements are `NA`, never inferred
#' from total device memory. All byte columns use bytes.
#'
#' @param pid One or more distinct positive process identifiers. CUDA can
#'   report other visible processes when permitted. Metal supports only the
#'   current R process.
#' @param refresh Re-run cached static discovery before constructing the device
#'   table. The memory observation itself is always fresh.
#' @return A data frame with one row per PID and process-visible GPU.
#'   `measurement_status`
#'   is one of `measured`, `not_active`, `unavailable`, or `unsupported`.
#' @examples
#' before <- gpu_memory_sample()
#' # Run GPU work here, optionally synchronizing through the compute framework.
#' after <- gpu_memory_sample()
#' @seealso [gpu_memory_info()], [hardware_manifest()]
#' @export
gpu_memory_sample <- function(pid = Sys.getpid(), refresh = FALSE) {
    if (!is.numeric(pid) || !length(pid) || anyNA(pid) ||
        any(!is.finite(pid)) || any(pid < 1) ||
        any(pid > .Machine$integer.max) || any(pid != floor(pid)) ||
        anyDuplicated(pid)) {
        stop("'pid' must contain distinct positive integers.",
            call. = FALSE)
    }
    pid <- as.integer(pid)
    devices <- gpu_devices(refresh = refresh)
    devices <- devices[devices$process_visible %in% TRUE, , drop = FALSE]
    if (!nrow(devices)) return(.gpuinfo_empty_memory_sample())

    indices <- rep(seq_len(nrow(devices)), times = length(pid))
    n <- length(indices)
    sample_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    out <- data.frame(
        sample_time = rep(sample_time, n),
        pid = rep(pid, each = nrow(devices)),
        logical_id = devices$logical_id[indices],
        physical_id = devices$physical_id[indices],
        uuid = devices$uuid[indices], vendor = devices$vendor[indices],
        model = devices$model[indices], backend = devices$backend[indices],
        total_memory_bytes = devices$total_memory_bytes[indices],
        used_memory_bytes = rep(NA_real_, n), free_memory_bytes = rep(NA_real_, n),
        measurement_scope = rep("process", n),
        measurement_source = rep(NA_character_, n),
        measurement_status = rep("unsupported", n),
        reason_code = rep("backend_process_memory_unsupported", n),
        stringsAsFactors = FALSE
    )

    cuda_rows <- which(out$backend == "cuda")
    if (length(cuda_rows)) {
        query <- .cuda_process_memory()
        for (i in cuda_rows) {
            out$measurement_source[[i]] <- "nvidia-smi-compute-apps"
            if (!query$ok) {
                out$measurement_status[[i]] <- "unavailable"
                out$reason_code[[i]] <- "nvidia_smi_process_query_failed"
            } else if (is.na(out$uuid[[i]])) {
                out$measurement_status[[i]] <- "unavailable"
                out$reason_code[[i]] <- "cuda_device_uuid_unavailable"
            } else {
                hit <- which(query$rows$pid == out$pid[[i]] &
                    .cuda_normalize_uuid(query$rows$uuid) ==
                    .cuda_normalize_uuid(out$uuid[[i]]))
                if (!length(hit)) {
                    out$used_memory_bytes[[i]] <- 0
                    out$measurement_status[[i]] <- "not_active"
                    out$reason_code[[i]] <- "process_not_using_device"
                } else if (all(is.na(query$rows$used_memory_bytes[hit]))) {
                    out$measurement_status[[i]] <- "unavailable"
                    out$reason_code[[i]] <- "cuda_process_memory_unavailable"
                } else {
                    out$used_memory_bytes[[i]] <- sum(
                        query$rows$used_memory_bytes[hit], na.rm = TRUE
                    )
                    out$measurement_status[[i]] <- "measured"
                    out$reason_code[[i]] <- "ok"
                }
            }
        }
    }

    metal_rows <- which(out$backend == "metal")
    if (length(metal_rows)) {
        native <- .gpuinfo_native_metal()
        default_metal <- which(devices$backend == "metal")[[1L]]
        for (i in metal_rows) {
            out$measurement_source[[i]] <- "MTLDevice.currentAllocatedSize"
            if (out$pid[[i]] != Sys.getpid()) {
                out$measurement_status[[i]] <- "unsupported"
                out$reason_code[[i]] <- "metal_current_process_only"
            } else if (indices[[i]] != default_metal) {
                out$measurement_status[[i]] <- "unavailable"
                out$reason_code[[i]] <- "metal_nondefault_device_unavailable"
            } else if (!isTRUE(native$device)) {
                out$measurement_status[[i]] <- "unavailable"
                out$reason_code[[i]] <- "metal_device_unavailable"
            } else if (!isTRUE(native$allocation_query_supported) ||
                       is.na(native$current_allocated_size)) {
                out$measurement_status[[i]] <- "unavailable"
                out$reason_code[[i]] <- "metal_allocation_query_unavailable"
            } else {
                out$used_memory_bytes[[i]] <- as.numeric(native$current_allocated_size)
                out$measurement_status[[i]] <- "measured"
                out$reason_code[[i]] <- "ok"
            }
        }
    }
    out
}
