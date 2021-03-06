% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_labelled.R
\name{is_labelled}
\alias{is_labelled}
\title{Test if a vector is labelled (i.e. if \code{label} attribute exists.)}
\usage{
is_labelled(.x)
}
\arguments{
\item{.x}{A vector}
}
\value{
logical, length 1
}
\description{
Test if a vector is labelled (i.e. if \code{label} attribute exists.)
}
\examples{
\dontrun{
dat <- data.frame(age = c(23, 45))
dat <- set_labels(dat, "age", "Age of participant")
is_labelled(datt$age) # return TRUE
}
}
