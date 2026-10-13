.cuda_run_path <- function(path, args = character(), timeout = 5L) {
    if (!length(path) || is.na(path) || !nzchar(path) || !file.exists(path))
        return(list(ok = FALSE, output = character(), status = NA_integer_, path = NA_character_))
    result <- tryCatch(
        suppressWarnings(system2(path, args = args, stdout = TRUE, stderr = TRUE,
                                 timeout = timeout)),
        error = function(e) structure(character(), status = 1L)
    )
    status <- attr(result, "status")
    if (is.null(status)) status <- 0L
    list(ok = identical(as.integer(status), 0L), output = enc2utf8(as.character(result)),
         status = as.integer(status), path = path)
}

.cuda_candidate_roots <- function(os = Sys.info()[["sysname"]],
                                  env = Sys.getenv()) {
    get_env <- function(name) {
        if (is.null(names(env)) || !name %in% names(env)) return("")
        value <- unname(env[[name]])
        if (!length(value) || is.na(value)) "" else value
    }
    roots <- c(get_env("CUDA_HOME"), get_env("CUDA_PATH"),
               get_env("CUDAToolkit_ROOT"))
    if (identical(os, "Windows")) {
        program_files <- unique(c(get_env("ProgramFiles"), get_env("ProgramW6432")))
        program_files <- program_files[nzchar(program_files)]
        roots <- c(roots, unlist(lapply(program_files, function(path) {
            Sys.glob(file.path(path, "NVIDIA GPU Computing Toolkit", "CUDA", "v*"))
        }), use.names = FALSE))
    } else if (identical(os, "Linux")) {
        roots <- c(roots, "/usr/local/cuda", Sys.glob("/usr/local/cuda-*"),
                   "/opt/cuda", Sys.glob("/opt/cuda-*"))
    }
    roots <- unique(path.expand(roots[nzchar(roots)]))
    roots[dir.exists(roots)]
}

.cuda_target_dirs <- function(roots) {
    unique(unlist(lapply(roots, function(root) {
        targets <- Sys.glob(file.path(root, "targets", "*"))
        targets[dir.exists(targets)]
    }), use.names = FALSE))
}

.cuda_runtime_candidates <- function(roots, target_dirs,
                                     os = Sys.info()[["sysname"]]) {
    if (identical(os, "Windows")) {
        files <- as.character(unlist(lapply(roots, function(root) {
            c(Sys.glob(file.path(root, "bin", "cudart64_*.dll")),
              Sys.glob(file.path(root, "lib", "x64", "cudart*.dll")))
        }), use.names = FALSE))
        defaults <- paste0("cudart64_", c(130, 129, 128, 127, 126, 125, 124,
                                           123, 122, 121, 120, 118, 117, 116,
                                           115, 114, 113, 112, 111, 110), ".dll")
    } else if (identical(os, "Darwin")) {
        files <- as.character(unlist(lapply(roots, function(root) {
            Sys.glob(file.path(root, "lib", "libcudart*.dylib"))
        }), use.names = FALSE))
        defaults <- c("libcudart.dylib")
    } else {
        bases <- unique(c(roots, target_dirs))
        files <- as.character(unlist(lapply(bases, function(root) {
            unlist(lapply(c("lib64", "lib", file.path("lib", "x86_64-linux-gnu")),
                          function(lib) Sys.glob(file.path(root, lib, "libcudart.so*"))),
                   use.names = FALSE)
        }), use.names = FALSE))
        defaults <- c("libcudart.so", "libcudart.so.13", "libcudart.so.12",
                      "libcudart.so.11.0")
    }
    unique(c(files[file.exists(files)], defaults))
}

.cuda_nvcc_path <- function(roots, os = Sys.info()[["sysname"]]) {
    name <- if (identical(os, "Windows")) "nvcc.exe" else "nvcc"
    found <- unname(Sys.which(name))
    # Prefer an explicitly configured or standard Toolkit root over a stale
    # compiler earlier on PATH. This keeps headers, libraries, and nvcc from
    # the same installation when several Toolkit versions coexist.
    candidates <- c(file.path(roots, "bin", name), found)
    candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
    if (length(candidates)) normalizePath(candidates[[1L]], winslash = "/",
                                           mustWork = FALSE) else NA_character_
}

.cuda_version_from_files <- function(roots) {
    files <- as.character(unlist(lapply(roots, function(root) {
        c(file.path(root, "version.json"), file.path(root, "version.txt"))
    }), use.names = FALSE))
    files <- files[file.exists(files)]
    for (file in files) {
        lines <- tryCatch(readLines(file, warn = FALSE), error = function(e) character())
        version <- .gpuinfo_match(lines,
            "(?:CUDA Version|\\\"version\\\"\\s*:)\\s*\\\"?([0-9]+(?:\\.[0-9]+){1,2})")
        if (!is.na(version)) return(version)
    }
    NA_character_
}

.cuda_nvcc_version_path <- function(path) {
    result <- .cuda_run_path(path, "--version")
    if (!result$ok) return(NA_character_)
    .gpuinfo_match(result$output, "release\\s+([0-9]+(?:\\.[0-9]+)+)")
}

.cuda_discover_static <- function(os = Sys.info()[["sysname"]],
                                  env = Sys.getenv()) {
    roots <- .cuda_candidate_roots(os, env)
    target_dirs <- .cuda_target_dirs(roots)
    include_dirs <- unique(c(file.path(roots, "include"),
                             file.path(target_dirs, "include")))
    include_dirs <- include_dirs[dir.exists(include_dirs)]
    cuda_headers <- file.path(include_dirs, "cuda.h")
    runtime_headers <- file.path(include_dirs, "cuda_runtime_api.h")
    header_available <- any(file.exists(cuda_headers))
    runtime_header_available <- any(file.exists(runtime_headers))
    library_dirs <- unique(c(
        file.path(roots, "lib64"), file.path(roots, "lib"),
        file.path(roots, "lib", "x64"), file.path(roots, "bin"),
        file.path(target_dirs, "lib64"), file.path(target_dirs, "lib")
    ))
    library_dirs <- library_dirs[dir.exists(library_dirs)]
    runtime_candidates <- .cuda_runtime_candidates(roots, target_dirs, os)
    runtime_files <- runtime_candidates[file.exists(runtime_candidates)]
    nvcc_path <- .cuda_nvcc_path(roots, os)
    compiler_available <- !is.na(nvcc_path)
    nvcc_version <- if (compiler_available) .cuda_nvcc_version_path(nvcc_path) else NA_character_
    toolkit_root <- NA_character_
    if (compiler_available) {
        possible <- normalizePath(file.path(dirname(nvcc_path), ".."),
                                  winslash = "/", mustWork = FALSE)
        if (dir.exists(possible)) toolkit_root <- possible
    }
    if (is.na(toolkit_root) && length(roots)) {
        complete <- vapply(roots, function(root) {
            file.exists(file.path(root, "include", "cuda.h")) &&
                file.exists(file.path(root, "include", "cuda_runtime_api.h"))
        }, logical(1L))
        if (any(complete)) toolkit_root <- roots[which(complete)[[1L]]]
    }
    toolkit_available <- header_available && runtime_header_available &&
        (compiler_available || length(runtime_files) > 0L)
    toolkit_version <- nvcc_version
    if (is.na(toolkit_version)) toolkit_version <- .cuda_version_from_files(roots)
    list(
        roots = roots, target_dirs = target_dirs, include_dirs = include_dirs,
        library_dirs = library_dirs, header_available = header_available,
        runtime_header_available = runtime_header_available,
        runtime_candidates = runtime_candidates,
        runtime_library_files = runtime_files,
        toolkit_available = toolkit_available, toolkit_root = toolkit_root,
        toolkit_version = toolkit_version,
        compiler_available = compiler_available, nvcc_path = nvcc_path,
        nvcc_version = nvcc_version, os = os
    )
}

.cuda_static_info <- function(refresh = FALSE) {
    key <- "cuda_static_info"
    if (isTRUE(refresh) && exists(key, envir = .gpuinfo_cache, inherits = FALSE))
        rm(list = key, envir = .gpuinfo_cache)
    .gpuinfo_cached(key, .cuda_discover_static)
}

.cuda_parse_nvcc_arches <- function(lines, prefix) {
    if (!length(lines)) return(integer())
    matches <- gregexpr(paste0(prefix, "_([0-9]+)"), lines, perl = TRUE)
    values <- unlist(regmatches(lines, matches), use.names = FALSE)
    values <- suppressWarnings(as.integer(sub(paste0("^", prefix, "_"), "", values)))
    sort(unique(values[!is.na(values)]))
}

.cuda_nvcc_supported <- function(path) {
    if (is.na(path)) return(list(
        native = integer(), ptx = integer(), query_supported = FALSE,
        native_query_status = NA_integer_, ptx_query_status = NA_integer_
    ))
    code <- .cuda_run_path(path, "--list-gpu-code")
    arch <- .cuda_run_path(path, "--list-gpu-arch")
    list(
        native = .cuda_parse_nvcc_arches(code$output, "sm"),
        ptx = sort(unique(c(.cuda_parse_nvcc_arches(code$output, "compute"),
                            .cuda_parse_nvcc_arches(arch$output, "compute")))),
        query_supported = code$ok || arch$ok,
        native_query_status = code$status, ptx_query_status = arch$status
    )
}

.cuda_normalize_architectures <- function(x) {
    if (is.null(x) || !length(x)) return(integer())
    normalize_one <- function(value) {
        if (is.numeric(value)) {
            value <- as.numeric(value)
            if (is.na(value) || value <= 0) return(NA_integer_)
            if (value < 20) return(as.integer(round(value * 10)))
            return(as.integer(round(value)))
        }
        value <- tolower(trimws(as.character(value)))
        value <- sub("-(real|virtual)$", "", value)
        value <- sub("^(sm_|compute_)", "", value)
        if (grepl("^[0-9]+\\.[0-9]+$", value))
            return(as.integer(round(as.numeric(value) * 10)))
        if (grepl("^[0-9]+$", value)) return(as.integer(value))
        NA_integer_
    }
    values <- vapply(as.list(x), normalize_one, integer(1L))
    sort(unique(values[!is.na(values)]))
}

#' Generate CUDA build architecture information
#'
#' Discovers visible device architectures and the local CUDA Toolkit/compiler
#' layout without installing or modifying CUDA. Native code is selected for
#' every requested architecture and PTX defaults to the highest architecture.
#'
#' @param architectures 'auto' or CUDA architectures such as 8.0, 80,
#'   'sm_80', or 'compute_120'.
#' @param ptx 'auto' for PTX at the highest architecture, FALSE for no PTX,
#'   or one explicit architecture.
#' @param refresh Re-run cached static CUDA discovery.
#' @return A structured list containing detection status, Toolkit paths,
#'   supported/requested architectures, CMake values, and nvcc flags.
#' @export
cuda_build_info <- function(architectures = "auto", ptx = "auto",
                            refresh = FALSE) {
    automatic <- length(architectures) == 1L &&
        identical(tolower(as.character(architectures)), "auto")
    info <- cuda_info(refresh = refresh)
    detected_arch <- sort(unique(info$devices$cuda_arch[
        info$devices$process_visible %in% TRUE & !is.na(info$devices$cuda_arch)
    ]))
    requested <- if (automatic) detected_arch else .cuda_normalize_architectures(architectures)
    requires_explicit <- automatic && !length(requested)
    static <- .cuda_static_info(refresh = refresh)
    supported <- .cuda_nvcc_supported(static$nvcc_path)

    if (length(ptx) == 1L && identical(tolower(as.character(ptx)), "auto")) {
        ptx_arch <- if (length(requested)) max(requested) else integer()
    } else if (isFALSE(ptx) || identical(ptx, "none")) {
        ptx_arch <- integer()
    } else if (isTRUE(ptx)) {
        ptx_arch <- if (length(requested)) max(requested) else integer()
    } else {
        ptx_arch <- .cuda_normalize_architectures(ptx)
        if (length(ptx_arch) > 1L) ptx_arch <- max(ptx_arch)
    }

    unsupported_native <- if (supported$query_supported)
        setdiff(requested, supported$native) else integer()
    unsupported_ptx <- if (supported$query_supported)
        setdiff(ptx_arch, supported$ptx) else integer()
    native_names <- paste0("sm_", requested)
    ptx_name <- if (length(ptx_arch)) paste0("compute_", ptx_arch) else NA_character_
    cmake <- paste0(requested, "-real")
    if (length(ptx_arch)) cmake <- c(cmake, paste0(ptx_arch, "-virtual"))
    gencode <- if (length(requested)) paste0(
        "-gencode=arch=compute_", requested, ",code=sm_", requested
    ) else character()
    if (length(ptx_arch)) gencode <- c(gencode, paste0(
        "-gencode=arch=compute_", ptx_arch, ",code=compute_", ptx_arch
    ))

    reason <- if (requires_explicit) {
        "no_visible_cuda_device"
    } else if (!length(requested)) {
        "no_valid_architecture"
    } else if (!static$compiler_available) {
        "nvcc_not_found"
    } else if (length(unsupported_native) || length(unsupported_ptx)) {
        "architecture_not_supported_by_nvcc"
    } else if (!supported$query_supported) {
        "nvcc_architecture_query_unavailable"
    } else {
        "ok"
    }
    ok <- identical(reason, "ok")
    list(
        ok = ok, detected = length(detected_arch) > 0L,
        reason = reason, requires_explicit_architecture = requires_explicit,
        detected_architectures = as.integer(detected_arch),
        architectures = as.integer(requested),
        native_architectures = native_names,
        ptx_architecture = ptx_name,
        cmake_architectures = cmake,
        nvcc_gencode_flags = gencode,
        nvcc_supported_native_architectures = supported$native,
        nvcc_supported_ptx_architectures = supported$ptx,
        nvcc_architecture_query_supported = supported$query_supported,
        unsupported_native_architectures = unsupported_native,
        unsupported_ptx_architectures = unsupported_ptx,
        toolkit_available = static$toolkit_available,
        toolkit_root = static$toolkit_root,
        toolkit_version = static$toolkit_version,
        cuda_home = static$toolkit_root,
        nvcc_path = static$nvcc_path,
        nvcc_version = static$nvcc_version,
        include_dirs = static$include_dirs,
        library_dirs = static$library_dirs,
        target_dirs = static$target_dirs,
        runtime_libraries = static$runtime_library_files
    )
}
