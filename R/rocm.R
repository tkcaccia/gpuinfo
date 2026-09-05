.rocm_has_amd_gpu <- function() {
    rocminfo <- .gpuinfo_command("rocminfo")
    if (rocminfo$ok && any(grepl("Marketing Name:|gfx[0-9]", rocminfo$output, ignore.case = TRUE))) return(TRUE)
    lspci <- .gpuinfo_command("lspci")
    lspci$ok && any(grepl("VGA|3D controller", lspci$output, ignore.case = TRUE) &
        grepl("AMD|ATI|Advanced Micro Devices", lspci$output, ignore.case = TRUE))
}

.rocm_native_rows <- function(native = .gpuinfo_native_rocm()) {
    if (!isTRUE(native$initialized) || native$count < 1L) return(.gpuinfo_empty_gpus())
    data.frame(
        id = seq_len(native$count) - 1L,
        vendor = rep("AMD", native$count),
        model = rep(NA_character_, native$count),
        memory_mb = rep(NA_real_, native$count),
        backend = rep("rocm", native$count),
        compute_capability = rep(NA_character_, native$count),
        stringsAsFactors = FALSE
    )
}

.rocm_version <- function() {
    candidates <- c("/opt/rocm/.info/version", "/opt/rocm/.info/version-dev")
    existing <- candidates[file.exists(candidates)]
    if (length(existing)) {
        value <- tryCatch(readLines(existing[[1L]], n = 1L, warn = FALSE), error = function(e) character())
        if (length(value)) return(.gpuinfo_na(value))
    }
    result <- .gpuinfo_command("hipcc", "--version")
    if (!result$ok) return(NA_character_)
    .gpuinfo_match(result$output, "HIP version:\\s*([0-9]+(?:\\.[0-9]+)+)")
}

#' Inspect ROCm availability
#'
#' @return A named list describing AMD GPU visibility, the kernel interface,
#'   ROCm utilities, installation, version, and apparent usability.
#' @export
rocm_info <- function() {
    native <- .gpuinfo_native_rocm()
    rocminfo <- .gpuinfo_command("rocminfo")
    smi <- .gpuinfo_command("rocm-smi", "--showproductname")
    hipcc_path <- unname(Sys.which("hipcc"))
    hipcc <- length(hipcc_path) > 0L && nzchar(hipcc_path)
    installed <- native$library || rocminfo$ok || smi$ok || hipcc || .gpuinfo_library_exists("rocm")
    gpu <- native$count > 0L || .rocm_has_amd_gpu()
    kernel_driver <- file.exists("/dev/kfd") || smi$ok
    runtime <- native$initialized || rocminfo$ok || .gpuinfo_files_exist(c("/opt/rocm/lib/libamdhip64.so", "/opt/rocm/lib64/libamdhip64.so"))
    version <- .rocm_version()
    if (is.na(version)) version <- .gpuinfo_hip_version(native$runtime_version)
    list(
        gpu = .gpuinfo_bool(gpu),
        kernel_driver = .gpuinfo_bool(kernel_driver),
        runtime = .gpuinfo_bool(runtime),
        installed = .gpuinfo_bool(installed),
        version = version,
        rocminfo = .gpuinfo_bool(rocminfo$ok),
        rocm_smi = .gpuinfo_bool(smi$ok),
        hipcc = .gpuinfo_bool(hipcc),
        usable = .gpuinfo_bool(gpu && runtime && (native$initialized || kernel_driver || rocminfo$ok))
    )
}

#' Test whether ROCm appears usable
#' @return A single `TRUE` or `FALSE`.
#' @export
has_rocm <- function() rocm_info()$usable
