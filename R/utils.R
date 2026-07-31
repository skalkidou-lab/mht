#' Get first non-NA value from vector
#'
#' Copied verbatim from `swereg::first_non_na()` so that `mht` carries no
#' dependency on `swereg`. Called by the 2026 exposure logic.
#'
#' @param x Vector of any type
#' @return First non-NA value in the vector
#' @noRd
first_non_na <- function(x){
  dplyr::first(na.omit(x))
}
