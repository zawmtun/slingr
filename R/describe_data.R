#' Describe data using {arsenal} package.
#'
#' This function creates a tableby object using more intuitive default
#' parameters. The tableby object should be passed to summary() to create a
#' descriptive table.
#'
#' @param dat A dataframe
#' @param by A string. Name of grouping variable by which data are described. If "" (default), overall total will be shown.
#' @param p_value A logical. Toggle to show p value (default is FALSE).
#' @param total A logical. Toggle to show overall total (default is FALSE) when grouping variable is defined.
#' @param digits An integer. Control the number of decimal place for continuous values (default is 1).
#' @param digits.pct An integer. Control the number of decimal place for percents (default is 1).
#' @param digits.p An integer. Control the number of decimal place for p-values (default is 1).
#' @param ... Arguments passed to tableby(). ?tableby for more details.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(arsenal)
#' library(dplyr)
#'
#' data(mockstudy)
#'
#' mockstudy %>%
#'   select(arm, sex, age) %>%
#'   describe_data(by = "arm") %>%
#'   summary(text = TRUE)
#'
#' mockstudy %>%
#'   select(arm, sex, age) %>%
#'   describe_data() %>%
#'   summary()
#' }
describe_data <- function(dat,
                          by = "",
                          p_value = FALSE,
                          total = FALSE,
                          digits = 1L,
                          digits.pct = 1L,
                          digits.p = 4L,
                          ...) {
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
