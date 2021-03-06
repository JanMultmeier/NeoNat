% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/BornBy.R
\name{BornBy}
\alias{BornBy}
\title{Calculating Date of Birth from the First Day of the Last Menstrual Cycle}
\usage{
BornBy(x, format = "\%d.\%m.\%Y", get = "point", sd = 1.96)
}
\arguments{
\item{x}{a vector, first day of the last menstrual cycle}

\item{format}{a text string explicating the date format for x, defaults to "%d.%m.%Y"}

\item{get}{either "point" or "interval" for just the point estimate or the interval boundaries, using a standard deviation of 13 days, respectively}

\item{sd}{indicates the width of the uncertainty interval as multiples of the standard deviation (13 days)}
}
\value{
a vector of estimated dates of birth (for "point") or a data.frame of dates of birth and lower and upper boundaries
}
\description{
Calculating Date of Birth from the First Day of the Last Menstrual Cycle
}
\examples{
BornBy("31.12.1984")
BornBy(c("11.11.1984","08.07.1990"),get="interval",sd=1)
}
\keyword{birthday}
\keyword{naegele}

