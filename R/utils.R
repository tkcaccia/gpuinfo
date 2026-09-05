.gpuinfo_cache <- new.env(parent = emptyenv())

.gpuinfo_cached <- function(key, compute) {
    if (exists(key, envir = .gpuinfo_cache, inherits = FALSE))
        return(get(key, envir = .gpuinfo_cache, inherits = FALSE))
    value <- compute()
    assign(key, value, envir = .gpuinfo_cache)
    value
}

.gpuinfo_empty_gpus <- function() {
    data.frame(
        id = integer(),
        vendor = character(),
        model = character(),
        memory_mb = numeric(),
        backend = character(),
        compute_capability = character(),
        stringsAsFactors = FALSE
    )
}

.gpuinfo_command <- function(command, args = character(), timeout = 5L) {
    path <- unname(Sys.which(command))
    if (!length(path) || !nzchar(path)) {
        return(list(ok = FALSE, output = character(), status = NA_integer_, path = NA_character_))
    }

    result <- tryCatch(
        suppressWarnings(system2(path, args = args, stdout = TRUE, stderr = TRUE,
                                 timeout = timeout)),
        error = function(e) structure(character(), status = 1L)
    )
    status <- attr(result, "status")
    if (is.null(status)) status <- 0L
    list(
        ok = identical(as.integer(status), 0L),
        output = enc2utf8(as.character(result)),
        status = as.integer(status),
        path = path
    )
}

.gpuinfo_na <- function(x) {
    if (!length(x)) return(NA_character_)
    x <- trimws(as.character(x[[1L]]))
    if (!nzchar(x) || tolower(x) %in% c("n/a", "na", "unknown", "[not supported]")) {
        NA_character_
    } else {
        x
    }
}

.gpuinfo_match <- function(x, pattern, group = 1L, ignore.case = TRUE) {
    if (!length(x)) return(NA_character_)
    hit <- regexec(pattern, x, perl = TRUE, ignore.case = ignore.case)
    matches <- regmatches(x, hit)
    matches <- matches[lengths(matches) > group]
    if (!length(matches)) return(NA_character_)
    .gpuinfo_na(matches[[1L]][[group + 1L]])
}

.gpuinfo_files_exist <- function(paths) {
    any(file.exists(path.expand(paths)))
}

.gpuinfo_library_exists <- function(kind) {
    os <- Sys.info()[["sysname"]]
    paths <- switch(kind,
        cuda_runtime = c(
            Sys.getenv("CUDA_PATH", unset = NA_character_),
            Sys.getenv("CUDA_HOME", unset = NA_character_),
            "/usr/local/cuda/lib64/libcudart.so",
            "/usr/lib/x86_64-linux-gnu/libcudart.so",
            "C:/Windows/System32/cudart64_*.dll"
        ),
        cuda_driver = c(
            "/usr/lib/x86_64-linux-gnu/libcuda.so.1",
            "/usr/lib64/libcuda.so.1",
            "/usr/lib/wsl/lib/libcuda.so.1",
            "C:/Windows/System32/nvcuda.dll"
        ),
        opencl = c(
            "/System/Library/Frameworks/OpenCL.framework/OpenCL",
            "/usr/lib/x86_64-linux-gnu/libOpenCL.so.1",
            "/usr/lib64/libOpenCL.so.1",
            "C:/Windows/System32/OpenCL.dll"
        ),
        rocm = c(
            Sys.getenv("ROCM_PATH", unset = NA_character_),
            "/opt/rocm/lib/libamdhip64.so",
            "/dev/kfd"
        ),
        character()
    )
    paths <- paths[!is.na(paths) & nzchar(paths)]
    if (!length(paths)) return(FALSE)
    any(file.exists(paths) | lengths(Sys.glob(paths)) > 0L) ||
        (identical(os, "Darwin") && identical(kind, "opencl") && file.exists(paths[[1L]]))
}

.gpuinfo_split_csv <- function(lines, expected) {
    if (!length(lines)) return(list())
    parts <- strsplit(lines, ",", fixed = TRUE)
    parts <- lapply(parts, trimws)
    parts[lengths(parts) >= expected]
}

.gpuinfo_bool <- function(x) isTRUE(x)

.gpuinfo_yesno <- function(x) if (isTRUE(x)) "yes" else "no"
