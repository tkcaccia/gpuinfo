#' gpuinfo: Lightweight hardware and GPU-backend detection
#'
#' `gpuinfo` provides framework-independent, read-only detection of CPU and GPU
#' hardware and the apparent availability of CUDA, Metal, ROCm, and OpenCL. It
#' does not require Python, a machine-learning framework, or a vendor SDK.
#'
#' @section Public API:
#' The package intentionally exposes only seven functions. Use
#' [hardware_info()] for the complete structured report, [gpu_sitrep()] for a
#' human-readable diagnostic report, and [has_gpu()], [has_cuda()],
#' [has_metal()], [has_rocm()], or [has_opencl()] for strict scalar checks.
#'
#' @section Interpretation:
#' A positive backend predicate means that `gpuinfo` found the relevant visible
#' hardware and enough driver/runtime support to regard that backend as usable.
#' It does not prove that another R package was compiled with that backend. For
#' example, [has_cuda()] can be `TRUE` while an installed deep-learning package
#' is CPU-only.
#'
#' Detection is limited to devices visible to the current process. Containers,
#' schedulers, remote sessions, and variables such as `CUDA_VISIBLE_DEVICES`
#' can intentionally hide physical devices. Missing tools, libraries, drivers,
#' or permissions produce `FALSE`, empty tables, or `NA` fields rather than an
#' error.
#'
#' @section Detection behavior:
#' Probes may load vendor libraries dynamically and may run read-only system
#' utilities such as `nvidia-smi`, `system_profiler`, `lspci`, `rocminfo`, or
#' `clinfo` when available. The package does not modify drivers, environment
#' variables, device settings, or files, and it does not make network requests.
#'
#' @seealso [hardware_info()], [gpu_sitrep()]
#' @keywords internal
"_PACKAGE"
