# gpuinfo 0.2.0

- Removed the incoming DESCRIPTION spelling NOTE by replacing "loadability"
  with "shared-library" and shortened the report examples to one scan each.
- Reused command and native probe results within each report to avoid repeated
  PowerShell launches on Windows. A new report or memory sample observes fresh
  dynamic values, and temporary observations are discarded even after errors.
- Preserved missing memory for malformed or partly unavailable CUDA process
  accounting, absent worker rows, and empty sample lists.
- Prevented OS NVIDIA fallback rows from overriding an initialized CUDA driver
  that reports no visible devices when `nvidia-smi` is unavailable.
- Corrected `has_gpu()` so physical-only CUDA devices hidden from the process
  are not reported as usable process-visible GPUs, while retaining operating-
  system fallback detection when CUDA provides no device observations.
- Separated successful CUDA driver initialization from device-count query
  status and exposed `driver_device_count_status` in `cuda_info()`.
- Made `hardware_manifest()` reuse one CUDA/device snapshot across its CUDA,
  NCCL, device, and optional memory sections.
- Added `gpu_memory_peak()` to combine worker-process samples by GPU UUID and
  return the highest complete observed memory sum for every expected device.
  Missing observations remain explicitly unmeasured rather than becoming zero.
- Added `cuda_libraries()`, a consolidated loadability table for the CUDA
  driver, CUDA runtime, cuBLAS, cuFFT, cuSOLVER, cuRAND, and NCCL. Optional
  libraries do not affect `has_cuda()`.
- Added optional `nccl_info()` inspection that dynamically loads NCCL, queries
  its version, and reports process-visible CUDA devices without creating a
  communicator or making NCCL a requirement for CUDA use.
- Added public `cpu_info()` with physical, logical, and process-available core
  counts, packages, threads per core, CPU features, and structured provenance.
- Added `system_memory_info()` with host and effective container RAM, swap, and
  current-process resident-memory fields in bytes on Linux, macOS, and Windows.
- Extended `hardware_info()` and `hardware_manifest()` with host memory and
  explicit visible/physical GPU counts. Multi-GPU visibility is not presented
  as evidence that a consuming package can execute across multiple GPUs.
- Added `gpu_memory_sample()` for timestamped process-level GPU memory samples.
  CUDA samples are matched by process ID and GPU UUID; Metal samples use
  `MTLDevice.currentAllocatedSize` for the current R process.
- Added `hardware_manifest()` for versioned, serializable benchmark provenance
  covering the host, devices, generic compute components, and memory.
- Preserved `gpu_memory_info()` as the backward-compatible device-wide memory
  snapshot API.
- Added explicit measurement status and reason codes when process memory is
  unavailable or unsupported, including Windows WDDM limitations.

# gpuinfo 0.1.0

- Initial CRAN release with framework-independent CPU, GPU, CUDA, Metal, ROCm,
  and OpenCL discovery.
