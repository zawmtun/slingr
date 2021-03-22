#' Data function: Describe data using tableby() from {arsenal} package.
#'
#' This function creates a tableby object using more intuitive default
#' parameters. The tableby object should be passed to summary() to create a
#' descriptive table.
#'
#' @param .data A dataframe
#' @param by Grouping variable by which data are described. If NULL (default), overall total will be shown.
#' @param p_value A logical. Toggle to show p value (default is FALSE).
#' @param total A logical. Toggle to show overall total (default is FALSE) when grouping variable is defined.
#' @param digits An integer. Control the number of decimal place for continuous values (default is 1).
#' @param digits.pct An integer. Control the number of decimal place for percents (default is 1).
#' @param digits.pvalue An integer. Control the number of decimal place for p-values (default is 1).
#' @param ... Arguments passed to tableby(). ?tableby for more details.
#'
#' @return A tableby object
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
#'   data_describe(by = arm) %>%
#'   summary(text = TRUE)
#'
#' mockstudy %>%
#'   select(arm, sex, age) %>%
#'   data_describe() %>%
#'   summary()
#' }
data_describe <- function(.data,
                          by = NULL,
                          p_value = FALSE,
                          total = FALSE,
                          digits = 1L,
                          digits.pct = 1L,
                          digits.pvalue = 4L,
                          ...) {
  .by_var <- rlang::enexpr(by)
  .by_var <- if (is.null(.by_var)) "" else rlang::as_name(.by_var)
  .f <- paste(.by_var, ".", sep = " ~ ")

  arsenal::tableby(
    stats::as.formula(.f),
    data = .data,
    test = p_value,
    total = total,
    digits = digits,
    digits.pct = digits.pct,
    digits.p = digits.pvalue,
    ...
  )
}