#' Create a tableby object using {arsenal} package with more intuitive default parameters. The tableby object should be passed to summary() to create a descriptive table.
#'
#' @param dat A dataframe
#' @param by A string with length 1. Name of grouping variable by which data are described. If "" (default), overall total will be shown.
#' @param p_value A logical vector with length 1. Toggle to show p value (default is FALSE).
#' @param total A logical vector with length 1. Toggle to show overall total (default is FALSE) when grouping variable is defined.
#' @param ... Arguments passed to tableby(). ?tableby for more details.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' data(mockstudy)
#'
#' mockstudy %>%
#'   select(arm, sex, age) %>%
#'   describe_data(by = "arm") %>%
#'   summary(text = TRUE)
#' }
describe_data <- function(dat, by = "", p_value = FALSE, total = FALSE, ...) {
  vars <- names(dat)
  vars <- if (by == "") {
    paste(vars, collapse = " + ")
  } else {
    paste(vars[vars != by], collapse = " + ")
  }

  f <- paste(by, vars, sep = " ~ ")

  tab <- arsenal::tableby(
    stats::as.formula(f),
    data = dat,
    test = p_value,
    total = total,
    ...
  )
}
