# Hardware validation matrix

Only completed real-hardware runs are marked as validated. Parser fixtures and
no-hardware CI are useful tests, but are not presented as hardware validation.

| Platform | Hardware | OS | Backend | Status |
|---|---|---|---|---|
| Apple | Apple M3 | macOS 14.5 | Metal | Validated locally |
| Apple | Apple M3 | macOS 14.5 | OpenCL | Loader/platform found; no compute device enumerated |
| NVIDIA | T4 or A10G | Linux | CUDA | Pending cloud run |
| NVIDIA | A10G | Windows | CUDA/OpenCL | Pending cloud run |
| AMD | MI300X or Radeon Pro V710 | Linux | ROCm | Pending cloud run |
| Intel | Arc/iGPU | Linux | OpenCL | Pending hardware access |
| CPU only | x86-64 | Linux/Windows | CPU | Pending hosted CI |
| CPU only | ARM64 | Linux | CPU | Pending hosted CI |
