describe_data <- function(dat, by = "", p_value = FALSE, total = FALSE, ...) {
  vars <- names(dat)
  vars <- if (by == "") {
    paste(vars, collapse = " + ")
  } else {
    paste(vars[vars != by], collapse = " + ")
  }

  f <- paste(by, vars, sep = " ~ ")

  tab <- arsenal::tableby(
    as.formula(f),
    data = dat,
    test = p_value,
    total = total
  )
  summary(tab, ...)
}
