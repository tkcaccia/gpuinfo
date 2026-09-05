#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

extern SEXP gpuinfo_cuda_probe(void);
extern SEXP gpuinfo_opencl_probe(void);
extern SEXP gpuinfo_metal_probe(void);
extern SEXP gpuinfo_rocm_probe(void);

static const R_CallMethodDef call_methods[] = {
    {"gpuinfo_cuda_probe", (DL_FUNC) &gpuinfo_cuda_probe, 0},
    {"gpuinfo_opencl_probe", (DL_FUNC) &gpuinfo_opencl_probe, 0},
    {"gpuinfo_metal_probe", (DL_FUNC) &gpuinfo_metal_probe, 0},
    {"gpuinfo_rocm_probe", (DL_FUNC) &gpuinfo_rocm_probe, 0},
    {NULL, NULL, 0}
};

void attribute_visible R_init_gpuinfo(DllInfo *dll) {
    R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
