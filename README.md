# gpuinfo

`gpuinfo` is a unified hardware and compute-capability discovery layer for R.
It lets packages detect accelerators, inspect their capabilities, and select an
appropriate backend without depending on torch, TensorFlow, Python, or a
vendor SDK. A tiny compiled layer dynamically queries CUDA, Metal, ROCm, and
OpenCL libraries; portable operating-system and command probes provide
fallbacks.

```r
install.packages("gpuinfo") # once released on CRAN

library(gpuinfo)

has_gpu()
has_cuda()
has_metal()
gpu_info()
accelerator_info()
gpu_capabilities()
cpu_info()

available_backends()
best_backend()
select_backend(require = "fp64")
check_accelerator(memory = "4GiB", precision = "fp64")
gpu_sitrep()
```

## Detection semantics

The package describes host capabilities, not whether a particular framework
was built with GPU support. In particular:

- `has_cuda()` means an NVIDIA GPU is visible through a working NVIDIA driver.
- `cuda_info()` separately reports GPU, driver, local runtime, Toolkit, and
  `nvcc` status. The CUDA version printed by `nvidia-smi` is labelled
  `driver_supported_cuda_version`, because it is not the installed Toolkit.
- `has_metal()` reports macOS hardware/OS Metal capability and does not inspect
  torch MPS support.
- `has_rocm()` requires an AMD GPU plus an apparent usable ROCm runtime/driver.
- `has_opencl()` detects an OpenCL loader or a working `clinfo` installation;
  it does not imply that a particular R package can use OpenCL.

All probes are defensive. Missing commands, drivers, files, or hardware return
`FALSE`, empty data frames, or `NA` fields rather than errors.

Detailed backend inspection uses three states: `available`, `unavailable`, and
`unknown`. Convenience predicates remain strict scalar logicals; an unknown
state is never silently promoted to `TRUE`.

## Backend priority

The default priority is CUDA, Metal, ROCm, OpenCL, then CPU. It is configurable:

```r
best_backend(c("metal", "cuda", "cpu"))

select_backend(
    require = "fp64",
    prefer = c("cuda", "rocm", "metal", "cpu")
)

select_device(min_memory = "8GiB", strategy = "most_memory")
```

## Containers and schedulers

`environment_info()` reports Docker, Podman, Apptainer, Kubernetes, WSL, CI,
and common HPC schedulers. `visible_devices()` reports controls such as
`CUDA_VISIBLE_DEVICES`, `ROCR_VISIBLE_DEVICES`, and `ZE_AFFINITY_MASK` without
changing them.

## Validation

`validation_info()` returns the real-hardware validation evidence shipped with
the installed package. Mock parser tests and CPU-only CI are intentionally not
presented as real GPU validation. The source repository also contains manually
triggered workflows for labeled NVIDIA, AMD, and Intel self-hosted runners.

## License

MIT © 2026 Stefano Cacciatore.
