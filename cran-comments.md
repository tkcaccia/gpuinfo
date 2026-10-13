## Resubmission

This is a resubmission. In response to the CRAN review:

- Software names in the `Description` field are now enclosed in single quotes.
  The `Title` contains only generic terms and no software names requiring
  quotation.
- The invalid relative file URI in `README.md` was replaced with an absolute
  repository URL.
- The package performs hardware discovery rather than implementing a named
  published method, so no DOI is applicable. The `Description` field now cites
  the official NVIDIA, Apple, AMD, and Khronos interface documentation on
  which backend detection is based.
- `gpu_sitrep()` now returns a structured `gpuinfo_sitrep` object without
  writing to the console when assigned. Human-readable and JSON output is
  produced only by its registered `print()` method.

## R CMD check results

Tested from the source tarball with R 4.6.0 on macOS 14.5 (Apple Silicon):

- 0 errors
- 0 warnings
- 0 notes

The CUDA, multi-device, and NCCL paths are covered by hardware-independent
mock tests. The saved Ubuntu NVIDIA validation host was offline during the
final archive check, so the optional NCCL addition was not represented as a
new real-hardware validation result.

## Validation scope

Maintainer evidence shipped with the package covers Apple M3 Metal/OpenCL,
Ubuntu 22.04.5 on an NVIDIA GeForce RTX 5060 Ti plus an Intel OpenCL device,
and NVIDIA Tesla T4 CUDA/OpenCL. The RTX run confirmed compute capability 12.0,
CUDA architecture 120, CUDA Toolkit/runtime 13.0, and
`sm_120`/`compute_120` build flags before the optional NCCL probe was added.
CPU-only checks cover Linux ARM64 and hosted Linux, Windows, and macOS runners.
NCCL itself, AMD ROCm, and Windows GPU backends remain explicitly unvalidated
on real hardware for this candidate.

The package performs local, read-only hardware discovery. Missing drivers,
libraries, commands, and devices are handled without errors. Tests do not
require network access or GPU hardware.

## Motivation

`gpuinfo` centralizes lightweight CPU and GPU detection used across packages
such as KODAMA, fastEmbedR, fastPLS, and faissR. It avoids duplicated code and
heavy framework dependencies while providing consistent, read-only hardware
detection.

Version 0.2.0 adds process-aware CPU topology, system/container RAM, UUID-aware
multi-GPU inventories, CUDA component separation, GPU-memory sampling, and a
versioned benchmark manifest while preserving the version 0.1 public fields.
It also adds a consolidated read-only loadability inventory for the CUDA
driver, runtime, cuBLAS, cuFFT, cuSOLVER, cuRAND, and NCCL, plus optional NCCL
version inspection. These optional libraries are not required for ordinary
CUDA detection or use.
