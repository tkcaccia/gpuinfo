#' gpuinfo: Lightweight hardware and GPU-backend detection
#'
#' `gpuinfo` provides framework-independent, read-only detection of CPU and GPU
#' hardware and the apparent availability of CUDA, Metal, ROCm, and OpenCL. It
#' does not require Python, a machine-learning framework, or a vendor SDK.
#'
#' @section Public API:
#' Use [hardware_info()] for the complete structured report, [cpu_info()] and
#' [system_memory_info()] for CPU and RAM resources, [gpu_sitrep()] for
#' a printable diagnostic object, [gpu_devices()] and [gpu_memory_info()] for
#' device tables, [gpu_memory_sample()] for process-level benchmark samples,
#' [gpu_memory_peak()] for UUID-based observed peak aggregation,
#' [hardware_manifest()] for reusable benchmark provenance, and
#' [cuda_info()], [cuda_libraries()], [cuda_build_info()], and
#' [cuda_preflight()] for provider-independent CUDA diagnostics, and
#' [nccl_info()] for optional NCCL library/version inspection. The
#' [has_gpu()], [has_cuda()], [has_metal()], [has_rocm()], and [has_opencl()]
#' functions provide strict scalar checks.
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
