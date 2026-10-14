# gpuinfo

`gpuinfo` is a lightweight hardware and GPU-backend detector for R. It does not
depend on torch, TensorFlow, Python, or a vendor SDK. A tiny compiled layer
dynamically queries CUDA, Metal, ROCm, and OpenCL libraries; portable
operating-system and command probes provide fallbacks.

```r
install.packages("gpuinfo")

library(gpuinfo)

has_gpu()
has_cuda()
has_metal()
has_rocm()
has_opencl()

cpu_info()
system_memory_info()
gpu_devices()
cuda_info()
cuda_libraries()
nccl_info()
cuda_build_info()
cuda_preflight()
gpu_memory_info()
gpu_memory_sample()
gpu_memory_peak(samples, expected_devices)
hardware_manifest()

info <- hardware_info()
info$cpu
info$memory
info$gpu
info$cuda
gpu_sitrep()
```

## Detection semantics

The package describes host capabilities, not whether a particular framework
was built with GPU support. In particular:

- `has_cuda()` means an NVIDIA GPU is visible through a working NVIDIA driver.
- `cpu_info()` reports identity, physical and logical topology, instruction-set
  flags when available, and the cores available to the current process after
  detectable container, affinity, or scheduler limits.
- `system_memory_info()` reports host RAM, effective container limits, swap,
  and resident memory for a selected process in bytes. Dynamic values are not
  cached.
- `cuda_info()` independently reports hardware visibility, driver loading and
  initialization, device enumeration, runtime loading and initialization,
  Toolkit, `nvcc`, component versions, and all visible devices. The
  `driver_initialization_status` and `driver_device_count_status` fields keep
  `cuInit()` and `cuDeviceGetCount()` outcomes separate. The CUDA version
  printed by `nvidia-smi` is labelled `driver_supported_cuda_version`; it is
  not the installed runtime or Toolkit.
- `cuda_libraries()` returns one table for the CUDA driver, runtime, cuBLAS,
  cuFFT, cuSOLVER, cuRAND, and NCCL. A positive row means the shared library is
  loadable in the current process; it does not prove that development headers
  or every symbol required by another package are available. cuVS and RAFT
  remain the responsibility of their consuming package.
- `nccl_info()` optionally loads NCCL and queries `ncclGetVersion()`, then
  reports the process-visible CUDA device count. It never creates a
  communicator, does not prove inter-GPU communication, and does not affect
  `has_cuda()` or ordinary GPU availability.
- `gpu_devices()` distinguishes process-visible CUDA ordinals from physical
  `nvidia-smi` indices and reconciles them by UUID.
- `cuda_build_info()` generates native, PTX, CMake, and `nvcc` architecture
  settings without installing or changing CUDA.
- `cuda_preflight()` returns structured diagnostics and remediation rather than
  stopping by default.
- `gpu_memory_info()` reports total, used, and free memory explicitly in bytes;
  used and free memory are dynamic snapshots.
- `gpu_memory_sample()` takes a non-cached, process-level sample suitable for
  benchmark logs. CUDA uses the `nvidia-smi` compute-process table reconciled by
  GPU UUID. Metal uses `MTLDevice.currentAllocatedSize` for the current R
  process. Unsupported measurements are explicit `NA` values with reason codes.
- `gpu_memory_peak()` combines worker processes by GPU UUID at each sampling
  instant and reports the highest complete observed sum per expected GPU.
  Use the same worker PID set for every sample. Incomplete observations are
  skipped; a GPU with no complete observation is `unmeasured` with `NA` memory.
- `hardware_manifest()` records versioned CPU, host/effective RAM, device,
  driver, runtime, Toolkit, optional NCCL information, environment, and
  optional GPU-memory data as an R list or JSON.
- `has_metal()` reports macOS hardware/OS Metal capability and does not inspect
  torch MPS support.
- `has_rocm()` requires an AMD GPU plus an apparent usable ROCm runtime/driver.
- `has_opencl()` requires a loader, an enumerated platform, and at least one
  compute device; it does not imply that a particular R package can use OpenCL.

All probes are defensive. Missing commands, drivers, files, or hardware return
`FALSE`, empty data frames, or `NA` fields rather than errors.

`gpuinfo` never detects FAISS, cuVS, ANN methods, or package-specific CUDA
capabilities. The integration boundary is documented in the
[FAISS integration guide](https://github.com/tkcaccia/gpuinfo/blob/main/docs/FAISSR-INTEGRATION.md).

Detailed backend inspection in `hardware_info()` uses three states:
`available`, `unavailable`, and `unknown`. Predicates remain strict scalar
logicals; an unknown state is never silently promoted to `TRUE`.

## NVIDIA library inventory

Use one call to inspect the NVIDIA libraries that are commonly useful to
GPU-enabled R packages:

```r
libraries <- cuda_libraries()
libraries[, c("component", "loadable", "version", "library_path")]

# The same table is included in the complete CUDA report.
cuda_info()$libraries
```

The table covers the CUDA driver and runtime, cuBLAS, cuFFT, cuSOLVER, cuRAND,
and NCCL. `loadable = TRUE` means the current R process successfully opened the
shared library and its dynamic dependencies. It does not mean that development
headers are installed or that a consuming package was compiled against that
library. Only the driver row contributes to `has_cuda()`, together with driver
initialization and a visible device.

This division lets packages reuse generic discovery while retaining their own
execution checks. For example, a consumer may use the runtime for kernels,
cuFFT for transforms, cuBLAS/cuSOLVER/cuRAND for PCA, and NCCL for multi-GPU
collectives. Higher-level dependencies such as cuVS and RAFT remain checks for
that consumer because they are not evidence of general CUDA availability.

## Observed peak GPU memory

Benchmarks control their own sampling interval and synchronization. Collect
several process-level samples, then summarize them without confusing CUDA
logical ordinals with physical device numbers:

```r
devices <- subset(gpu_devices(), process_visible)
workers <- c(1234L, 1235L) # benchmark worker PIDs

samples <- lapply(seq_len(20), function(i) {
    Sys.sleep(0.1)
    gpu_memory_sample(workers)
})

gpu_memory_peak(samples, devices)
```

For each timestamp and UUID, memory attributed to all supplied worker PIDs is
summed. The helper then selects the highest complete observation. If an
expected UUID has no complete observation, its peak is `NA` with status
`unmeasured`; it is not silently reported as zero. This is an observed peak,
whose accuracy depends on sampling frequency, not a driver-maintained lifetime
high-water mark.

## Containers and schedulers

`hardware_info()$environment` reports Docker, Podman, Apptainer, Kubernetes,
WSL, CI, common HPC schedulers, and device-visibility controls without changing
them.

## Validation

`hardware_info()$validation` returns the real-hardware validation evidence
shipped with the installed package. Mock parser tests and CPU-only CI are
intentionally not presented as real GPU validation. The source repository also
contains manually triggered workflows for labeled NVIDIA, AMD, and Intel
self-hosted runners.
Account setup, provider links, exact validation commands, credential handling,
and cleanup procedures are recorded in the
[hardware access and validation playbook](https://github.com/tkcaccia/gpuinfo/blob/main/docs/HARDWARE-ACCESS.md).

The CRAN check workflow covers Linux x86-64 and ARM64, Windows, and macOS on
Apple Silicon and Intel. It also checks R-devel on Linux and Windows and
R-oldrel on Linux. Every job runs `R CMD check --as-cran` and treats NOTEs as
failures. Report examples collect one snapshot each; repeated probes share
observations only for the duration of a report.

The validation evidence for the 0.2.0 candidate covers Apple M3 Metal/OpenCL,
NVIDIA Tesla T4 CUDA/OpenCL, NVIDIA RTX 5060 Ti CUDA 13.0 (including compute
capability 12.0 build architecture), Intel OpenCL alongside that NVIDIA device,
Linux ARM64 CPU, and hosted Linux, Windows, and macOS CPU runners.
ROCm on AMD hardware and CUDA/OpenCL on Windows GPU hardware remain
unvalidated. Intel OpenCL evidence is limited to the Linux device listed
above. Other defensive probes are included for early testing, but they are not
represented as validated hardware support. Later releases will update this
evidence only after real-device runs pass.

## License

MIT © 2026 Stefano Cacciatore.
