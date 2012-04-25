\name{RegularTimeIntervalDataFrame}
\alias{RegularTimeIntervalDataFrame}
\title{Create a regular TimeIntervalDataFrame from scratch}
\usage{
  RegularTimeIntervalDataFrame(from, to, by, period,
    timezone = "UTC", data = NULL)
}
\arguments{
  \item{from}{POSIXct or character representing a time with
  a valid format (see
  \code{\link[base:as.POSIXct]{POSIXct}}). It represents
  the start of the object (and of its first interval).}

  \item{to}{POSIXct or character representing a time with a
  valid format (see
  \code{\link[base:as.POSIXct]{POSIXct}}). It represents
  the end of the object : the end of the last interval is
  as near as possible from this date, but before it. If
  missing, its value is deduced from \sQuote{from},
  \sQuote{by} and \sQuote{data}.}

  \item{by}{a \code{\link{POSIXctp}} object indicating the
  increment to use between the start of each interval}

  \item{period}{a \code{\link{POSIXctp}} object indicating
  the period of each interval. If missing, it is given the
  value of \code{by}.}

  \item{data}{a data.frame with a number of rows the fit
  with the number of intervals created by the function. If
  \code{NULL}, the data slot will be a data.frame with zero
  column.}

  \item{timezone}{character representing a valid timezone
  (see \code{\link[base]{timezone}}).}
}
\value{
  a
  \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
  object.
}
\description{
  To see all methods related to this class, see
  \code{\link{TimeIntervalDataFrame-class}}
}
\details{
  To access to the class documentation, type in the R
  console :

  \code{class?TimeIntervalDataFrame}
}
\seealso{
  \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}},
  \code{\link{TimeIntervalDataFrame}},
  \code{\link{timetools}}
}

