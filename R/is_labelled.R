#' Test if a vector is labelled (i.e. if `label` attribute exists.)
#'
#' @param .x A vector
#' @return logical, length 1
#' @examples
#' \dontrun{
#' dat <- data.frame(age = c(23, 45))
#' dat <- set_labels(dat, "age", "Age of participant")
#' is_labelled(datt$age) # return TRUE
#' }
#' @export
is_labelled <- function(.x) {
  any(names(attributes(.x)) %in% "label")
}