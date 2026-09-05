#' Package hardware-validation evidence
#'
#' Returns the real-hardware results shipped with this version. This reports
#' evidence collected by the maintainers; it does not probe the current host.
#'
#' @return A data frame containing package version, validation date, operating
#'   system, architecture, hardware, backend, and result.
#' @export
validation_info <- function() {
    path <- system.file("extdata", "validation.csv", package = "gpuinfo")
    if (!nzchar(path)) return(data.frame())
    utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}
