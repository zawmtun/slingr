#' Add label attribute to a vector.
#'
#' @param .x A vector
#' @param .label A character vector of length 1. Label of the vector.
#' @return A labelled vector (i.e. a vector with `label` attribute)
#' @examples
#' \dontrun{
#' v <- 1:3
#' v <- add_label(v, "Some numbers")
#' v
#' attributes(v)
#' }
add_label <- function(.x, .label) {
  attr(.x, "label") <- .label
  .x
}