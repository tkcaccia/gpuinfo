#' Inspect execution-environment restrictions
#'
#' Detects common containers, CI systems, HPC schedulers, WSL, and accelerator
#' visibility variables. No information is transmitted.
#'
#' @return A named list describing the current execution environment.
#' @export
environment_info <- function() {
    cgroup <- if (file.exists("/proc/1/cgroup"))
        tryCatch(readLines("/proc/1/cgroup", warn = FALSE), error = function(e) character()) else character()
    container <- if (file.exists("/.dockerenv") || any(grepl("docker", cgroup, ignore.case = TRUE))) {
        "docker"
    } else if (file.exists("/run/.containerenv") || any(grepl("podman", cgroup, ignore.case = TRUE))) {
        "podman"
    } else if (nzchar(Sys.getenv("APPTAINER_CONTAINER")) || nzchar(Sys.getenv("SINGULARITY_CONTAINER"))) {
        "apptainer"
    } else if (nzchar(Sys.getenv("KUBERNETES_SERVICE_HOST"))) {
        "kubernetes"
    } else {
        NA_character_
    }
    release <- paste(unname(Sys.info()[c("release", "version")]), collapse = " ")
    scheduler <- if (nzchar(Sys.getenv("SLURM_JOB_ID"))) "slurm" else
        if (nzchar(Sys.getenv("PBS_JOBID"))) "pbs" else
        if (nzchar(Sys.getenv("LSB_JOBID"))) "lsf" else NA_character_
    list(
        container = container,
        wsl = grepl("microsoft|wsl", release, ignore.case = TRUE),
        ci = nzchar(Sys.getenv("CI")),
        ci_provider = if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) "github-actions" else
            if (nzchar(Sys.getenv("GITLAB_CI"))) "gitlab-ci" else NA_character_,
        scheduler = scheduler,
        visible_devices = visible_devices()
    )
}

#' Report accelerator visibility controls
#'
#' @return A named character vector. Unset variables are represented by `NA`.
#' @export
visible_devices <- function() {
    variables <- c("CUDA_VISIBLE_DEVICES", "NVIDIA_VISIBLE_DEVICES",
                   "ROCR_VISIBLE_DEVICES", "HIP_VISIBLE_DEVICES",
                   "GPU_DEVICE_ORDINAL", "ZE_AFFINITY_MASK")
    values <- Sys.getenv(variables, unset = NA_character_)
    stats::setNames(unname(values), variables)
}

#' GPU device identifiers and counts
#'
#' `available_gpu_count()` counts devices visible to this R process.
#' `physical_gpu_count()` uses `nvidia-smi` when possible to bypass CUDA
#' visibility filters, but otherwise returns `NA` when the physical count
#' cannot be distinguished safely.
#'
#' @return An integer vector or scalar.
#' @name gpu_visibility
NULL

#' @rdname gpu_visibility
#' @export
gpu_devices <- function() gpu_info()$id

#' @rdname gpu_visibility
#' @export
available_gpu_count <- function() gpu_count()

#' @rdname gpu_visibility
#' @export
physical_gpu_count <- function() {
    smi <- .cuda_smi_rows()
    if (nrow(smi)) return(as.integer(nrow(smi)))
    if (identical(Sys.info()[["sysname"]], "Darwin")) return(gpu_count())
    NA_integer_
}
