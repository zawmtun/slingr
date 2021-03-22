#' Set labels to columns in a dataframe.
#'
#' @param .data A dataframe
#' @param .cols A character vector; Colomns to set labels to
#' @param .labels A character vector; Labels for the columns. If NULL, each element of .colnames must be named with its label.
#' @return Dataframe with labelled columns.
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   age = c(23, 45),
#'   gender = c("M", "F")
#' )
#'
#' # Labels in a separate vector
#' var <- c("age", "gender")
#' lbl <- c("Age of participant", "Gender of participant")
#' data_set_labels(dat, var, lbl)
#'
#' # Labels in the names of `.cols` vector
#' names(var) <- lbl
#' data_set_labels(dat, var)
#' }
#' @export
data_set_labels <- function(.data, .cols, .labels = NULL) {
  stopifnot(
    "`.data` must be data.frame." = is.data.frame(.data),
    "`.cols` must be character." = is.vector(.cols, mode = "character"),
    "None of `.cols` were found in `.data`" = any(.cols %in% names(.data))
  )

  if (is.null(.labels)) {
    stopifnot(
      "`.cols` must have names when `.labels` is not provided." =
        !is.null(names(.cols))
    )
  } else {
    stopifnot(
      "`.labels` must be character." = is.vector(.labels, mode = "character"),
      "`x` and `lbl` must have the same length." =
        identical(length(.cols), length(.labels))
    )
  }

  x <- .cols[.cols %in% names(.data)]

  lbl <- if (is.null(.labels)) {
    names(.cols)
  } else {
    .labels[.cols %in% names(.data)]
  }

  for (i in seq_along(x)) {
    .data[[x[i]]] <- add_label(.data[[x[i]]], lbl[i])
  }

  .data
}