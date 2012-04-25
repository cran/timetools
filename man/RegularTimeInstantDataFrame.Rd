\name{RegularTimeInstantDataFrame}
\alias{RegularTimeInstantDataFrame}
\title{Create a regular TimeInstantDataFrame from scratch}
\usage{
  RegularTimeInstantDataFrame(from, to, by,
    timezone = "UTC", data = NULL)
}
\arguments{
  \item{from}{POSIXct or character representing a time with
  a valid format (see
  \code{\link[base:as.POSIXct]{POSIXct}}). It represents
  the start of the object.}

  \item{to}{POSIXct or character representing a time with a
  valid format (see
  \code{\link[base:as.POSIXct]{POSIXct}}). It represents
  the end of the object. If missing, its value is deduced
  from \sQuote{from}, \sQuote{by} and \sQuote{data}.}

  \item{by}{a \code{\link{POSIXctp}} object indicating the
  increment to use between instants of the object.}

  \item{data}{a data.frame with a number of rows the fit
  with the number of intervals created by the function. If
  \code{NULL}, the data slot will be a data.frame with zero
  column.}

  \item{timezone}{character representing a valid timezone
  (see \code{\link[base]{timezone}}).}
}
\value{
  a
  \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}}
  object.
}
\description{
  To see all methods related to this class, see
  \code{\link{TimeInstantDataFrame-class}}
}
\details{
  To access to the class documentation, type in the R
  console :

  \code{class?TimeInstantDataFrame}
}
\seealso{
  \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}},
  \code{\link{TimeInstantDataFrame}},
  \code{\link{timetools}}
}

