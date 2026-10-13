# Future faissR integration

`gpuinfo` owns provider-independent hardware and CUDA system discovery. It does
not know about FAISS, cuVS, approximate-nearest-neighbor methods, metrics, or
package installation policy.

A future `faissR` release can combine the packages without merging their
responsibilities:

```r
cuda <- gpuinfo::cuda_info()
build <- gpuinfo::cuda_build_info()

faiss <- faissR::faiss_available()
faiss_gpu <- faissR::faiss_gpu_available()
cuvs <- faissR::cuvs_available()

list(
    cuda_driver_usable = cuda$driver_usable,
    cuda_runtime_usable = cuda$runtime_usable,
    visible_cuda_devices = cuda$devices[cuda$devices$process_visible, ],
    cuda_build = build,
    faiss_available = faiss,
    faiss_gpu_available = faiss_gpu,
    cuvs_available = cuvs
)
```

`faissR` should use `gpuinfo::cuda_preflight()` only for generic CUDA
prerequisites. It must retain its own header/library discovery, compile/link
probes, runtime execution tests, method and metric capabilities, route
selection, and `FAISSR_*` policy variables.

A positive `gpuinfo::has_cuda()` means only that at least one CUDA device is
visible through a working NVIDIA driver. It does not imply that the CUDA
runtime, Toolkit, `nvcc`, FAISS, cuVS, or a GPU-enabled `faissR` build exists.

Recommended `faissR` decision order:

1. Inspect generic CUDA state with `gpuinfo::cuda_info()`.
2. If compilation is required, inspect `gpuinfo::cuda_build_info()` and reject
   unsupported target architectures before invoking a compiler.
3. Run `faissR`-owned FAISS and cuVS discovery and compile/link probes.
4. Run a package-specific GPU execution test.
5. Select a route only from capabilities proved by both layers; otherwise fail
   explicitly or select a documented CPU route.

Do not add FAISS or cuVS detection to `gpuinfo`, and do not interpret
`cuda_build_info()` as proof that a particular package was compiled for the
reported architectures.
