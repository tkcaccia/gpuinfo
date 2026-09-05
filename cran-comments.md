## R CMD check results

Tested with R 4.6.0 on macOS 14.5 (Apple Silicon):

- 0 errors
- 0 warnings
- 1 expected note: new submission

## Validation scope

Real-device validation for version 0.1.0 covers Apple M3 Metal and NVIDIA
Tesla T4 CUDA/OpenCL. CPU-only checks cover Linux ARM64 and hosted Linux,
Windows, and macOS runners. AMD ROCm, Intel GPU OpenCL, and Windows GPU
backends are reported as unvalidated and will only be promoted after successful
real-hardware runs.

The package performs local, read-only hardware discovery. Missing drivers,
libraries, commands, and devices are handled without errors. Tests do not
require network access or GPU hardware.
