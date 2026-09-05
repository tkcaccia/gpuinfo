#' List available compute backends
#'
#' Hardware/driver availability is reported independently of support in any R
#' package. CPU is always included.
#'
#' @return A character vector using lowercase backend names.
#' @export
available_backends <- function() {
    registry <- .gpuinfo_backend_registry()
    available <- vapply(registry, function(entry) isTRUE(entry$available()), logical(1L))
    names(registry)[available]
}

#' Select the preferred available backend
#'
#' @param prefer Ordered character vector of backend names. Names must be among
#'   `"cuda"`, `"metal"`, `"rocm"`, `"opencl"`, and `"cpu"`.
#'
#' @return One backend name as a character scalar.
#' @export
best_backend <- function(prefer = c("cuda", "metal", "rocm", "opencl", "cpu")) {
    select_backend(prefer = prefer)
}
