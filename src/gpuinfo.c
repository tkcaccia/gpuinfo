#include <R.h>
#include <Rinternals.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
# include <windows.h>
typedef HMODULE gpuinfo_lib;
static gpuinfo_lib gpuinfo_open(const char **names) {
    int i;
    for (i = 0; names[i] != NULL; ++i) {
        HMODULE lib = LoadLibraryA(names[i]);
        if (lib != NULL) return lib;
    }
    return NULL;
}
# define GPUINFO_SYM(lib, name) GetProcAddress((lib), (name))
# define GPUINFO_CLOSE(lib) FreeLibrary((lib))
#else
# include <dlfcn.h>
typedef void *gpuinfo_lib;
static gpuinfo_lib gpuinfo_open(const char **names) {
    int i;
    for (i = 0; names[i] != NULL; ++i) {
        void *lib = dlopen(names[i], RTLD_LAZY | RTLD_LOCAL);
        if (lib != NULL) return lib;
    }
    return NULL;
}
# define GPUINFO_SYM(lib, name) dlsym((lib), (name))
# define GPUINFO_CLOSE(lib) dlclose((lib))
#endif

typedef int (*cu_init_fn)(unsigned int);
typedef int (*cu_device_count_fn)(int *);
typedef int (*cu_driver_version_fn)(int *);
typedef int (*cu_device_get_fn)(int *, int);
typedef int (*cu_device_name_fn)(char *, int, int);
typedef int (*cu_device_memory_fn)(size_t *, int);
typedef int (*cu_device_capability_fn)(int *, int *, int);

SEXP gpuinfo_cuda_probe(void) {
#ifdef _WIN32
    const char *names[] = {"nvcuda.dll", NULL};
#elif __APPLE__
    const char *names[] = {"libcuda.dylib", "/usr/local/cuda/lib/libcuda.dylib", NULL};
#else
    const char *names[] = {"libcuda.so.1", "libcuda.so", "/usr/lib/wsl/lib/libcuda.so.1", NULL};
#endif
    gpuinfo_lib lib = gpuinfo_open(names);
    int initialized = 0, count = 0, version = 0, i;
    SEXP answer, answer_names, device_names, memory, capability;
    const char *fields[] = {"library", "initialized", "count", "driver_version",
                            "names", "memory_mb", "compute_capability"};

    PROTECT(answer = allocVector(VECSXP, 7));
    PROTECT(answer_names = allocVector(STRSXP, 7));
    for (i = 0; i < 7; ++i) SET_STRING_ELT(answer_names, i, mkChar(fields[i]));
    setAttrib(answer, R_NamesSymbol, answer_names);
    if (lib == NULL) {
        SET_VECTOR_ELT(answer, 0, ScalarLogical(0));
        SET_VECTOR_ELT(answer, 1, ScalarLogical(0));
        SET_VECTOR_ELT(answer, 2, ScalarInteger(0));
        SET_VECTOR_ELT(answer, 3, ScalarInteger(NA_INTEGER));
        SET_VECTOR_ELT(answer, 4, allocVector(STRSXP, 0));
        SET_VECTOR_ELT(answer, 5, allocVector(REALSXP, 0));
        SET_VECTOR_ELT(answer, 6, allocVector(STRSXP, 0));
        UNPROTECT(2);
        return answer;
    }

    {
        cu_init_fn cuInit = (cu_init_fn) GPUINFO_SYM(lib, "cuInit");
        cu_device_count_fn cuDeviceGetCount = (cu_device_count_fn) GPUINFO_SYM(lib, "cuDeviceGetCount");
        cu_driver_version_fn cuDriverGetVersion = (cu_driver_version_fn) GPUINFO_SYM(lib, "cuDriverGetVersion");
        if (cuDriverGetVersion != NULL) cuDriverGetVersion(&version);
        if (cuInit != NULL && cuDeviceGetCount != NULL && cuInit(0) == 0 && cuDeviceGetCount(&count) == 0) {
            initialized = 1;
            if (count < 0 || count > 1024) count = 0;
        }
    }

    PROTECT(device_names = allocVector(STRSXP, count));
    PROTECT(memory = allocVector(REALSXP, count));
    PROTECT(capability = allocVector(STRSXP, count));
    for (i = 0; i < count; ++i) {
        cu_device_get_fn cuDeviceGet = (cu_device_get_fn) GPUINFO_SYM(lib, "cuDeviceGet");
        cu_device_name_fn cuDeviceGetName = (cu_device_name_fn) GPUINFO_SYM(lib, "cuDeviceGetName");
        cu_device_memory_fn cuDeviceTotalMem = (cu_device_memory_fn) GPUINFO_SYM(lib, "cuDeviceTotalMem_v2");
        cu_device_capability_fn cuDeviceComputeCapability = (cu_device_capability_fn) GPUINFO_SYM(lib, "cuDeviceComputeCapability");
        int device = i, major = 0, minor = 0;
        size_t bytes = 0;
        char label[256] = "", cap[32] = "";
        if (cuDeviceGet != NULL) cuDeviceGet(&device, i);
        if (cuDeviceGetName != NULL && cuDeviceGetName(label, 255, device) == 0)
            SET_STRING_ELT(device_names, i, mkCharCE(label, CE_UTF8));
        else SET_STRING_ELT(device_names, i, NA_STRING);
        if (cuDeviceTotalMem != NULL && cuDeviceTotalMem(&bytes, device) == 0)
            REAL(memory)[i] = (double) bytes / (1024.0 * 1024.0);
        else REAL(memory)[i] = NA_REAL;
        if (cuDeviceComputeCapability != NULL && cuDeviceComputeCapability(&major, &minor, device) == 0) {
            snprintf(cap, sizeof(cap), "%d.%d", major, minor);
            SET_STRING_ELT(capability, i, mkChar(cap));
        } else SET_STRING_ELT(capability, i, NA_STRING);
    }
    SET_VECTOR_ELT(answer, 0, ScalarLogical(1));
    SET_VECTOR_ELT(answer, 1, ScalarLogical(initialized));
    SET_VECTOR_ELT(answer, 2, ScalarInteger(count));
    SET_VECTOR_ELT(answer, 3, version > 0 ? ScalarInteger(version) : ScalarInteger(NA_INTEGER));
    SET_VECTOR_ELT(answer, 4, device_names);
    SET_VECTOR_ELT(answer, 5, memory);
    SET_VECTOR_ELT(answer, 6, capability);
    GPUINFO_CLOSE(lib);
    UNPROTECT(5);
    return answer;
}

typedef int (*cl_platforms_fn)(uint32_t, void **, uint32_t *);
typedef int (*cl_devices_fn)(void *, uint64_t, uint32_t, void **, uint32_t *);
typedef int (*cl_device_info_fn)(void *, uint32_t, size_t, void *, size_t *);

SEXP gpuinfo_opencl_probe(void) {
#ifdef _WIN32
    const char *names[] = {"OpenCL.dll", NULL};
#elif __APPLE__
    const char *names[] = {"/System/Library/Frameworks/OpenCL.framework/OpenCL", NULL};
#else
    const char *names[] = {"libOpenCL.so.1", "libOpenCL.so", NULL};
#endif
    gpuinfo_lib lib = gpuinfo_open(names);
    uint32_t platform_count = 0, device_count = 0, total = 0, p, d, offset = 0;
    void **platforms = NULL;
    SEXP answer, answer_names, device_names, gpu_flags, memory_bytes, fp64_flags;
    const char *fields[] = {"library", "platform_count", "device_count", "devices",
                            "gpu", "memory_bytes", "fp64"};

    PROTECT(answer = allocVector(VECSXP, 7));
    PROTECT(answer_names = allocVector(STRSXP, 7));
    for (p = 0; p < 7; ++p) SET_STRING_ELT(answer_names, p, mkChar(fields[p]));
    setAttrib(answer, R_NamesSymbol, answer_names);
    if (lib != NULL) {
        cl_platforms_fn getPlatforms = (cl_platforms_fn) GPUINFO_SYM(lib, "clGetPlatformIDs");
        cl_devices_fn getDevices = (cl_devices_fn) GPUINFO_SYM(lib, "clGetDeviceIDs");
        if (getPlatforms != NULL && getDevices != NULL && getPlatforms(0, NULL, &platform_count) == 0 && platform_count > 0) {
            platforms = (void **) R_alloc(platform_count, sizeof(void *));
            if (getPlatforms(platform_count, platforms, NULL) == 0) {
                for (p = 0; p < platform_count; ++p) {
                    device_count = 0;
                    if (getDevices(platforms[p], (uint64_t) 0xFFFFFFFF, 0, NULL, &device_count) == 0) total += device_count;
                }
            }
        }
    }
    PROTECT(device_names = allocVector(STRSXP, total));
    PROTECT(gpu_flags = allocVector(LGLSXP, total));
    PROTECT(memory_bytes = allocVector(REALSXP, total));
    PROTECT(fp64_flags = allocVector(LGLSXP, total));
    if (lib != NULL && total > 0) {
        cl_devices_fn getDevices = (cl_devices_fn) GPUINFO_SYM(lib, "clGetDeviceIDs");
        cl_device_info_fn getInfo = (cl_device_info_fn) GPUINFO_SYM(lib, "clGetDeviceInfo");
        for (p = 0; p < platform_count; ++p) {
            void **devices;
            device_count = 0;
            if (getDevices(platforms[p], (uint64_t) 0xFFFFFFFF, 0, NULL, &device_count) != 0 || device_count == 0) continue;
            devices = (void **) R_alloc(device_count, sizeof(void *));
            if (getDevices(platforms[p], (uint64_t) 0xFFFFFFFF, device_count, devices, NULL) != 0) continue;
            for (d = 0; d < device_count; ++d) {
                char name[512] = "";
                char extensions[4096] = "";
                uint64_t type = 0, memory = 0, double_config = 0;
                if (getInfo != NULL && getInfo(devices[d], 0x102B, sizeof(name) - 1, name, NULL) == 0)
                    SET_STRING_ELT(device_names, offset, mkCharCE(name, CE_UTF8));
                else SET_STRING_ELT(device_names, offset, NA_STRING);
                if (getInfo != NULL && getInfo(devices[d], 0x1000, sizeof(type), &type, NULL) == 0)
                    LOGICAL(gpu_flags)[offset] = (type & 4) != 0;
                else LOGICAL(gpu_flags)[offset] = NA_LOGICAL;
                if (getInfo != NULL && getInfo(devices[d], 0x101F, sizeof(memory), &memory, NULL) == 0)
                    REAL(memory_bytes)[offset] = (double) memory;
                else REAL(memory_bytes)[offset] = NA_REAL;
                if (getInfo != NULL) {
                    getInfo(devices[d], 0x1030, sizeof(extensions) - 1, extensions, NULL);
                    if (getInfo(devices[d], 0x1032, sizeof(double_config), &double_config, NULL) == 0)
                        LOGICAL(fp64_flags)[offset] = double_config != 0 || strstr(extensions, "cl_khr_fp64") != NULL;
                    else LOGICAL(fp64_flags)[offset] = strstr(extensions, "cl_khr_fp64") != NULL;
                } else LOGICAL(fp64_flags)[offset] = NA_LOGICAL;
                ++offset;
            }
        }
    }
    SET_VECTOR_ELT(answer, 0, ScalarLogical(lib != NULL));
    SET_VECTOR_ELT(answer, 1, ScalarInteger((int) platform_count));
    SET_VECTOR_ELT(answer, 2, ScalarInteger((int) total));
    SET_VECTOR_ELT(answer, 3, device_names);
    SET_VECTOR_ELT(answer, 4, gpu_flags);
    SET_VECTOR_ELT(answer, 5, memory_bytes);
    SET_VECTOR_ELT(answer, 6, fp64_flags);
    if (lib != NULL) GPUINFO_CLOSE(lib);
    UNPROTECT(6);
    return answer;
}

SEXP gpuinfo_metal_probe(void) {
    SEXP answer, answer_names;
    int framework = 0, device = 0;
    const char *fields[] = {"framework", "device"};
#ifdef __APPLE__
    const char *names[] = {"/System/Library/Frameworks/Metal.framework/Metal", NULL};
    gpuinfo_lib lib = gpuinfo_open(names);
    if (lib != NULL) {
        void *(*create_device)(void) = (void *(*)(void)) GPUINFO_SYM(lib, "MTLCreateSystemDefaultDevice");
        framework = 1;
        if (create_device != NULL && create_device() != NULL) device = 1;
        GPUINFO_CLOSE(lib);
    }
#endif
    PROTECT(answer = allocVector(VECSXP, 2));
    PROTECT(answer_names = allocVector(STRSXP, 2));
    SET_STRING_ELT(answer_names, 0, mkChar(fields[0]));
    SET_STRING_ELT(answer_names, 1, mkChar(fields[1]));
    setAttrib(answer, R_NamesSymbol, answer_names);
    SET_VECTOR_ELT(answer, 0, ScalarLogical(framework));
    SET_VECTOR_ELT(answer, 1, ScalarLogical(device));
    UNPROTECT(2);
    return answer;
}

typedef int (*hip_init_fn)(unsigned int);
typedef int (*hip_device_count_fn)(int *);
typedef int (*hip_runtime_version_fn)(int *);

SEXP gpuinfo_rocm_probe(void) {
#ifdef _WIN32
    const char *names[] = {"amdhip64.dll", NULL};
#elif __APPLE__
    const char *names[] = {"libamdhip64.dylib", NULL};
#else
    const char *names[] = {"libamdhip64.so", "/opt/rocm/lib/libamdhip64.so",
                           "/opt/rocm/lib64/libamdhip64.so", NULL};
#endif
    gpuinfo_lib lib = gpuinfo_open(names);
    int initialized = 0, count = 0, version = 0;
    SEXP answer, answer_names;
    const char *fields[] = {"library", "initialized", "count", "runtime_version"};
    if (lib != NULL) {
        hip_init_fn hipInit = (hip_init_fn) GPUINFO_SYM(lib, "hipInit");
        hip_device_count_fn hipGetDeviceCount = (hip_device_count_fn) GPUINFO_SYM(lib, "hipGetDeviceCount");
        hip_runtime_version_fn hipRuntimeGetVersion = (hip_runtime_version_fn) GPUINFO_SYM(lib, "hipRuntimeGetVersion");
        if (hipRuntimeGetVersion != NULL) hipRuntimeGetVersion(&version);
        if (hipInit != NULL && hipGetDeviceCount != NULL && hipInit(0) == 0 && hipGetDeviceCount(&count) == 0) {
            initialized = 1;
            if (count < 0 || count > 1024) count = 0;
        }
    }
    PROTECT(answer = allocVector(VECSXP, 4));
    PROTECT(answer_names = allocVector(STRSXP, 4));
    SET_STRING_ELT(answer_names, 0, mkChar(fields[0]));
    SET_STRING_ELT(answer_names, 1, mkChar(fields[1]));
    SET_STRING_ELT(answer_names, 2, mkChar(fields[2]));
    SET_STRING_ELT(answer_names, 3, mkChar(fields[3]));
    setAttrib(answer, R_NamesSymbol, answer_names);
    SET_VECTOR_ELT(answer, 0, ScalarLogical(lib != NULL));
    SET_VECTOR_ELT(answer, 1, ScalarLogical(initialized));
    SET_VECTOR_ELT(answer, 2, ScalarInteger(count));
    SET_VECTOR_ELT(answer, 3, version > 0 ? ScalarInteger(version) : ScalarInteger(NA_INTEGER));
    if (lib != NULL) GPUINFO_CLOSE(lib);
    UNPROTECT(2);
    return answer;
}
