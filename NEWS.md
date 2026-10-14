# gpuinfo 0.2.0

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
