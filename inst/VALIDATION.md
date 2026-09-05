# Hardware validation matrix

Only completed real-hardware runs are marked as validated. Parser fixtures and
no-hardware CI are useful tests, but are not presented as hardware validation.

| Platform | Hardware | OS | Backend | Status |
|---|---|---|---|---|
| Apple | Apple M3 | macOS 14.5 | Metal | Validated locally |
| Apple | Apple M3 | macOS 14.5 | OpenCL | Loader/platform found; no compute device enumerated |
| NVIDIA | Tesla T4 | Amazon Linux host / Ubuntu 22.04 container | CUDA 12.6 | Validated on Hugging Face Jobs |
| NVIDIA | A10G | Windows | CUDA/OpenCL | Pending cloud run |
| AMD | MI300X or Radeon Pro V710 | Linux | ROCm | Pending cloud run |
| Intel | Arc/iGPU | Linux | OpenCL | Pending hardware access |
| CPU only | x86-64 | Linux/Windows | CPU | Validated by hosted CI |
| CPU only | ARM64 | Linux | CPU | Pending hosted CI |

The NVIDIA run used an isolated `t4-small` worker, NVIDIA driver 580.178.04,
CUDA 12.6 development image, R 4.1.2, and package commit `e456699`. The complete
public job is [6a9c0df1e686246ca69a3ada](https://huggingface.co/jobs/tkcaccia/6a9c0df1e686246ca69a3ada).
It confirmed a native CUDA driver initialization, one Tesla T4, compute
capability 7.5, 15,828,320,256 bytes of memory, the local runtime and Toolkit,
and backend selection. The first live run also exposed an OpenCL vendor-name
deduplication issue; the resulting regression fix is covered by the fixture
test suite.
