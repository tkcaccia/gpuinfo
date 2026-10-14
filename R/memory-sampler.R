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
    rows <- if (isTRUE(result$ok)) .cuda_parse_process_memory(result$output) else
        .cuda_parse_process_memory(character())
    # An empty successful query means no active compute processes. Malformed
    # rows instead make accounting uncertain; do not convert them to zero.
    valid <- nrow(rows) == sum(nzchar(trimws(result$output)))
    list(
        ok = isTRUE(result$ok) && valid,
        rows = rows,
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
    .gpuinfo_memory_sample_from_devices(pid, devices)
}

.gpuinfo_memory_sample_from_devices <- function(pid, devices) {
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
                } else if (any(!is.finite(query$rows$used_memory_bytes[hit])) ||
                           any(query$rows$used_memory_bytes[hit] < 0)) {
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

.gpuinfo_empty_memory_peak <- function() {
    data.frame(
        logical_id = integer(), physical_id = integer(), uuid = character(),
        vendor = character(), model = character(), backend = character(),
        total_memory_bytes = numeric(), peak_used_memory_bytes = numeric(),
        peak_sample_time = character(), observed_sample_count = integer(),
        incomplete_sample_count = integer(), worker_count = integer(),
        measurement_status = character(), reason_code = character(),
        stringsAsFactors = FALSE
    )
}

.gpuinfo_peak_devices <- function(expected_devices) {
    if (is.character(expected_devices)) {
        expected_devices <- data.frame(
            logical_id = rep(NA_integer_, length(expected_devices)),
            physical_id = rep(NA_integer_, length(expected_devices)),
            uuid = expected_devices,
            vendor = rep(NA_character_, length(expected_devices)),
            model = rep(NA_character_, length(expected_devices)),
            backend = rep("cuda", length(expected_devices)),
            total_memory_bytes = rep(NA_real_, length(expected_devices)),
            stringsAsFactors = FALSE
        )
    }
    if (!is.data.frame(expected_devices))
        stop("'expected_devices' must be a device data frame or UUID vector.",
             call. = FALSE)
    if (!"uuid" %in% names(expected_devices))
        stop("'expected_devices' must contain a 'uuid' column.", call. = FALSE)
    defaults <- list(
        logical_id = NA_integer_, physical_id = NA_integer_,
        vendor = NA_character_, model = NA_character_, backend = "cuda",
        total_memory_bytes = NA_real_
    )
    for (name in names(defaults)) {
        if (!name %in% names(expected_devices))
            expected_devices[[name]] <- rep(defaults[[name]], nrow(expected_devices))
    }
    expected_devices <- expected_devices[c(
        "logical_id", "physical_id", "uuid", "vendor", "model", "backend",
        "total_memory_bytes"
    )]
    expected_devices$uuid <- .cuda_normalize_uuid(expected_devices$uuid)
    known <- expected_devices$uuid[!is.na(expected_devices$uuid)]
    if (anyDuplicated(known))
        stop("'expected_devices' must contain distinct non-missing UUIDs.",
             call. = FALSE)
    expected_devices
}

.gpuinfo_peak_samples <- function(samples) {
    if (is.data.frame(samples)) samples <- list(samples)
    if (!is.list(samples))
        stop("'samples' must be a sample data frame or a list of them.",
             call. = FALSE)
    if (!length(samples)) {
        out <- .gpuinfo_empty_memory_sample()
        out$.sample_key <- character()
        return(out)
    }
    required <- c(
        "sample_time", "pid", "uuid", "used_memory_bytes",
        "measurement_status"
    )
    rows <- lapply(seq_along(samples), function(i) {
        value <- samples[[i]]
        if (!is.data.frame(value) || !all(required %in% names(value)))
            stop("Every sample must be a data frame returned by gpu_memory_sample().",
                 call. = FALSE)
        value <- value[required]
        value$.sample_key <- if (nrow(value))
            paste(i, value$sample_time, sep = ":") else character()
        value
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out$uuid <- .cuda_normalize_uuid(out$uuid)
    out
}

#' Calculate observed peak process memory for each GPU
#'
#' Aggregates repeated [gpu_memory_sample()] results into one row per expected
#' GPU. Worker processes are combined within each sampling instant by UUID,
#' and the largest complete sum is returned. Logical and physical device
#' numbers are metadata only and are never used to match devices.
#'
#' Use the same worker PID set for all samples. The worker set is the union of
#' PIDs observed across the supplied samples. An instant is complete only when
#' every worker has one row for that GPU, a finite non-negative memory value,
#' and status `measured` or `not_active`. Incomplete instants are
#' ignored rather than treated as zero. A device with no complete observation
#' is returned with `peak_used_memory_bytes = NA`, status `unmeasured`, and an
#' explanatory reason code.
#'
#' @param samples A data frame returned by [gpu_memory_sample()], several such
#'   data frames combined with `rbind()`, or a list of sample data frames. A
#'   list preserves separate observations even if timestamps are identical.
#' @param expected_devices A device table, normally the process-visible rows
#'   from [gpu_devices()], or a character vector of expected GPU UUIDs. Passing
#'   this inventory is what allows completely missing GPUs to be reported as
#'   unmeasured.
#' @return A data frame with one row per expected device, identified by UUID,
#'   containing the highest complete worker-process sum in bytes, the time of
#'   that observation, observation counts, worker count, status, and reason.
#' @examples
#' devices <- gpu_devices()
#' if (nrow(devices)) {
#'     samples <- list(gpu_memory_sample(), gpu_memory_sample())
#'     gpu_memory_peak(samples, devices[devices$process_visible, ])
#' }
#' @seealso [gpu_memory_sample()], [gpu_memory_info()]
#' @export
gpu_memory_peak <- function(samples, expected_devices) {
    devices <- .gpuinfo_peak_devices(expected_devices)
    sample_rows <- .gpuinfo_peak_samples(samples)
    if (!nrow(devices)) return(.gpuinfo_empty_memory_peak())
    expected_pids <- unique(sample_rows$pid[!is.na(sample_rows$pid)])

    rows <- lapply(seq_len(nrow(devices)), function(i) {
        uuid <- devices$uuid[[i]]
        hit <- if (is.na(uuid)) integer() else which(sample_rows$uuid == uuid)
        device_samples <- sample_rows[hit, , drop = FALSE]
        worker_count <- length(expected_pids)
        peak <- NA_real_
        peak_time <- NA_character_
        observed <- incomplete <- 0L
        reason <- "no_samples_for_device"

        if (is.na(uuid)) {
            reason <- "device_uuid_unavailable"
        } else if (nrow(device_samples)) {
            groups <- split(seq_len(nrow(device_samples)),
                            device_samples$.sample_key)
            complete <- vapply(groups, function(index) {
                group_pids <- device_samples$pid[index]
                all(is.finite(device_samples$used_memory_bytes[index])) &&
                    all(device_samples$used_memory_bytes[index] >= 0) &&
                    all(device_samples$measurement_status[index] %in%
                        c("measured", "not_active")) &&
                    !anyNA(group_pids) && !anyDuplicated(group_pids) &&
                    setequal(group_pids, expected_pids)
            }, logical(1L))
            observed <- sum(complete)
            incomplete <- sum(!complete)
            if (observed) {
                totals <- vapply(groups[complete], function(index) {
                    sum(device_samples$used_memory_bytes[index])
                }, numeric(1L))
                best <- which.max(totals)
                peak <- unname(totals[[best]])
                best_rows <- groups[complete][[best]]
                peak_time <- as.character(
                    device_samples$sample_time[best_rows[[1L]]]
                )
                reason <- if (incomplete) "partial_samples_ignored" else "ok"
            } else {
                reason <- "no_complete_samples_for_device"
            }
        }
        data.frame(
            logical_id = as.integer(devices$logical_id[[i]]),
            physical_id = as.integer(devices$physical_id[[i]]),
            uuid = uuid,
            vendor = as.character(devices$vendor[[i]]),
            model = as.character(devices$model[[i]]),
            backend = as.character(devices$backend[[i]]),
            total_memory_bytes = as.numeric(devices$total_memory_bytes[[i]]),
            peak_used_memory_bytes = peak,
            peak_sample_time = peak_time,
            observed_sample_count = as.integer(observed),
            incomplete_sample_count = as.integer(incomplete),
            worker_count = as.integer(worker_count),
            measurement_status = if (observed) "measured" else "unmeasured",
            reason_code = reason,
            stringsAsFactors = FALSE
        )
    })
    answer <- do.call(rbind, rows)
    rownames(answer) <- NULL
    answer
}
