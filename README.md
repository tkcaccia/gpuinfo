# gpuinfo

`gpuinfo` is a lightweight hardware and GPU-backend detector for R. It does not
depend on torch, TensorFlow, Python, or a vendor SDK. A tiny compiled layer
dynamically queries CUDA, Metal, ROCm, and OpenCL libraries; portable
operating-system and command probes provide fallbacks.

```r
install.packages("gpuinfo") # once released on CRAN

library(gpuinfo)

has_gpu()
has_cuda()
has_metal()
has_rocm()
has_opencl()

info <- hardware_info()
info$cpu
info$gpu
info$cuda
gpu_sitrep()
```

## Detection semantics

The package describes host capabilities, not whether a particular framework
was built with GPU support. In particular:

- `has_cuda()` means an NVIDIA GPU is visible through a working NVIDIA driver.
- `hardware_info()$cuda` separately reports GPU, driver, local runtime,
  Toolkit, and `nvcc` status. The CUDA version printed by `nvidia-smi` is labelled
  `driver_supported_cuda_version`, because it is not the installed Toolkit.
- `has_metal()` reports macOS hardware/OS Metal capability and does not inspect
  torch MPS support.
- `has_rocm()` requires an AMD GPU plus an apparent usable ROCm runtime/driver.
- `has_opencl()` requires a loader, an enumerated platform, and at least one
  compute device; it does not imply that a particular R package can use OpenCL.

All probes are defensive. Missing commands, drivers, files, or hardware return
`FALSE`, empty data frames, or `NA` fields rather than errors.

Detailed backend inspection in `hardware_info()` uses three states:
`available`, `unavailable`, and `unknown`. Predicates remain strict scalar
logicals; an unknown state is never silently promoted to `TRUE`.

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
[hardware access and validation playbook](docs/HARDWARE-ACCESS.md).

The 0.1.0 release has been validated on Apple M3 Metal, NVIDIA Tesla T4 CUDA
and OpenCL, Linux ARM64 CPU, and hosted Linux, Windows, and macOS CPU runners.
ROCm on AMD hardware, OpenCL on Intel GPU hardware, and CUDA/OpenCL on Windows
GPU hardware remain unvalidated in this release. Their defensive probes are
included for early testing, but they are not represented as validated hardware
support. Later releases will update this evidence only after real-device runs
pass.

## License

MIT © 2026 Stefano Cacciatore.
