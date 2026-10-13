# Hardware validation matrix

Only completed real-hardware runs are marked as validated. Parser fixtures and
no-hardware CI are useful tests, but are not presented as hardware validation.

| Platform | Hardware | OS | Backend | Status |
|---|---|---|---|---|
| Apple | Apple M3 | macOS 14.5 | Metal | Validated locally |
| Apple | Apple M3 | macOS 14.5 | OpenCL | Validated locally |
| NVIDIA | Tesla T4 | Amazon Linux host / Ubuntu 22.04 container | CUDA 12.6 | Validated on Hugging Face Jobs |
| NVIDIA | GeForce RTX 5060 Ti | Ubuntu 22.04.5 | CUDA 13.0 | Validated on dedicated Linux host |
| Intel | Device a780 | Ubuntu 22.04.5 | OpenCL | Validated alongside NVIDIA device |
| NVIDIA | A10G | Windows | CUDA/OpenCL | Pending cloud run |
| AMD | MI300X or Radeon Pro V710 | Linux | ROCm | Pending cloud run |
| Intel | Arc/iGPU | Linux | OpenCL | Pending hardware access |
| CPU only | x86-64 | Linux/Windows | CPU | Validated by hosted CI |
| CPU only | ARM64 | Ubuntu 24.04 | CPU | Validated by hosted CI |

The NVIDIA run used an isolated `t4-small` worker, NVIDIA driver 580.178.04,
CUDA 12.6 development image, R 4.1.2, and package commit `e456699`. The complete
public job is [6a9c0df1e686246ca69a3ada](https://huggingface.co/jobs/tkcaccia/6a9c0df1e686246ca69a3ada).
It confirmed a native CUDA driver initialization, one Tesla T4, compute
capability 7.5, 15,828,320,256 bytes of memory, the local runtime and Toolkit,
and backend selection. The first live run also exposed an OpenCL vendor-name
deduplication issue; the resulting regression fix is covered by the fixture
test suite. A second T4 run against commit `6aa149b` confirmed that the physical
GPU is reported exactly once while CUDA and OpenCL remain available; see job
[6a9c0fdbe686246ca69a3b08](https://huggingface.co/jobs/tkcaccia/6a9c0fdbe686246ca69a3b08).

The Linux ARM64 package build and test run is preserved in GitHub Actions run
[33967507957](https://github.com/tkcaccia/gpuinfo/actions/runs/33967507957).

The version 0.2.0 candidate was also validated from one source archive on an
Apple M3 and an Ubuntu 22.04.5 host. The NVIDIA run detected an RTX 5060 Ti by
UUID with compute capability 12.0 and 16,610,820,096 bytes of memory. CUDA
driver and runtime initialization succeeded; CUDA Toolkit 13.0 `nvcc` reported
support for architecture 120, and `cuda_build_info()` produced `sm_120`,
`compute_120`, CMake real/virtual architecture values, and matching `-gencode`
flags. The same host enumerated its Intel OpenCL GPU separately and correctly
preserved NVIDIA/Intel vendor identities.
