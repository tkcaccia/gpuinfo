.cuda_parse_smi_rows <- function(lines, with_capability = TRUE) {
    pieces <- .gpuinfo_split_csv(lines, if (with_capability) 4L else 3L)
    if (!length(pieces)) return(.gpuinfo_empty_gpus())
    rows <- lapply(pieces, function(x) {
        data.frame(
            id = suppressWarnings(as.integer(x[[1L]])),
            vendor = "NVIDIA",
            model = .gpuinfo_na(x[[2L]]),
            memory_mb = suppressWarnings(as.numeric(x[[3L]])),
            backend = "cuda",
            compute_capability = if (with_capability) .gpuinfo_na(x[[4L]]) else NA_character_,
            stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}

.cuda_smi_rows <- function() {
    query <- c("--query-gpu=index,name,memory.total,compute_cap", "--format=csv,noheader,nounits")
    result <- .gpuinfo_command("nvidia-smi", query)
    with_capability <- result$ok
    if (!result$ok) {
        query <- c("--query-gpu=index,name,memory.total", "--format=csv,noheader,nounits")
        result <- .gpuinfo_command("nvidia-smi", query)
        with_capability <- FALSE
    }
    if (!result$ok) return(.gpuinfo_empty_gpus())
    .cuda_parse_smi_rows(result$output, with_capability)
}

.cuda_native_rows <- function(native = .gpuinfo_native_cuda()) {
    if (!isTRUE(native$initialized) || native$count < 1L) return(.gpuinfo_empty_gpus())
    n <- native$count
    data.frame(
        id = seq_len(n) - 1L,
        vendor = rep("NVIDIA", n),
        model = rep_len(native$names, n),
        memory_mb = rep_len(as.numeric(native$memory_mb), n),
        backend = rep("cuda", n),
        compute_capability = rep_len(native$compute_capability, n),
        stringsAsFactors = FALSE
    )
}

.cuda_rows <- function() {
    native <- .cuda_native_rows()
    if (nrow(native)) native else .cuda_smi_rows()
}

.cuda_nvcc_version <- function() {
    result <- .gpuinfo_command("nvcc", "--version")
    if (!result$ok) return(NA_character_)
    .gpuinfo_match(result$output, "release\\s+([0-9]+(?:\\.[0-9]+)+)")
}

.cuda_driver_version <- function() {
    result <- .gpuinfo_command("nvidia-smi", c("--query-gpu=driver_version", "--format=csv,noheader"))
    if (!result$ok) return(NA_character_)
    .gpuinfo_na(result$output)
}

.cuda_parse_memory_rows <- function(lines) {
    pieces <- .gpuinfo_split_csv(lines, 5L)
    if (!length(pieces)) return(data.frame())
    do.call(rbind, lapply(pieces, function(x) data.frame(
        id = suppressWarnings(as.integer(x[[1L]])),
        model = .gpuinfo_na(x[[2L]]),
        total = suppressWarnings(as.numeric(x[[3L]])),
        used = suppressWarnings(as.numeric(x[[4L]])),
        free = suppressWarnings(as.numeric(x[[5L]])),
        stringsAsFactors = FALSE
    )))
}

.cuda_memory_rows <- function() {
    result <- .gpuinfo_command("nvidia-smi", c(
        "--query-gpu=index,name,memory.total,memory.used,memory.free",
        "--format=csv,noheader,nounits"
    ))
    if (!result$ok) return(data.frame())
    .cuda_parse_memory_rows(result$output)
}

.cuda_supported_version <- function() {
    result <- .gpuinfo_command("nvidia-smi")
    if (!result$ok) return(NA_character_)
    .gpuinfo_match(result$output, "CUDA Version:\\s*([0-9]+(?:\\.[0-9]+)+)")
}

#' Inspect CUDA availability
#'
#' Distinguishes NVIDIA GPU presence, the NVIDIA driver, a local CUDA runtime,
#' the CUDA Toolkit, and `nvcc`. The `driver_supported_cuda_version` value is
#' the maximum CUDA version advertised by the driver; it is not the installed
#' Toolkit version.
#'
#' @return A named list. `usable` is `TRUE` when an NVIDIA GPU is visible to a
#'   working NVIDIA driver.
#' @noRd
cuda_info <- function() {
    native <- .gpuinfo_native_cuda()
    gpus <- .cuda_rows()
    smi <- .gpuinfo_command("nvidia-smi", "-L")
    gpu <- nrow(gpus) > 0L || native$count > 0L || (smi$ok && any(grepl("GPU", smi$output, fixed = TRUE)))
    driver_version <- .cuda_driver_version()
    if (is.na(driver_version)) driver_version <- .gpuinfo_cuda_version(native$driver_version)
    driver <- native$library || smi$ok || !is.na(driver_version) || .gpuinfo_library_exists("cuda_driver")
    nvcc_path <- unname(Sys.which("nvcc"))
    nvcc <- length(nvcc_path) > 0L && nzchar(nvcc_path)
    toolkit_version <- .cuda_nvcc_version()
    cuda_home <- c(Sys.getenv("CUDA_HOME", unset = ""), Sys.getenv("CUDA_PATH", unset = ""))
    toolkit <- nvcc || any(dir.exists(cuda_home[nzchar(cuda_home)])) || dir.exists("/usr/local/cuda")
    runtime <- .gpuinfo_library_exists("cuda_runtime")

    list(
        gpu = .gpuinfo_bool(gpu),
        driver = .gpuinfo_bool(driver),
        driver_version = driver_version,
        driver_supported_cuda_version = .cuda_supported_version(),
        runtime = .gpuinfo_bool(runtime),
        runtime_version = if (runtime && !is.na(toolkit_version)) toolkit_version else NA_character_,
        toolkit = .gpuinfo_bool(toolkit),
        toolkit_version = toolkit_version,
        nvcc = .gpuinfo_bool(nvcc),
        nvcc_path = if (nvcc) nvcc_path else NA_character_,
        compute_capability = if (nrow(gpus)) gpus$compute_capability[[1L]] else NA_character_,
        usable = .gpuinfo_bool(gpu && driver && (native$initialized || smi$ok))
    )
}

#' Test whether CUDA appears usable
#'
#' Returns `TRUE` when an NVIDIA GPU is visible through an apparently working
#' NVIDIA driver. Detection uses a dynamically loaded CUDA driver library and,
#' when available, read-only `nvidia-smi` queries.
#'
#' The CUDA Toolkit and `nvcc` compiler are not required: applications can use
#' a GPU through a driver and a bundled runtime without a local Toolkit. A
#' positive result does not imply that torch, TensorFlow, or another R package
#' was built with CUDA support. Inspect `hardware_info()$cuda` to distinguish
#' GPU, driver, runtime, Toolkit, compiler, and version information.
#'
#' @return One non-missing logical value: `TRUE` when CUDA appears usable,
#'   otherwise `FALSE`.
#'
#' @examples
#' has_cuda()
#' hardware_info()$cuda
#'
#' @seealso [hardware_info()], [has_gpu()]
#' @export
has_cuda <- function() cuda_info()$usable

has_nvidia_gpu <- function() cuda_info()$gpu

has_cuda_driver <- function() cuda_info()$driver

has_cuda_runtime <- function() cuda_info()$runtime

has_cuda_toolkit <- function() cuda_info()$toolkit

has_nvcc <- function() cuda_info()$nvcc
