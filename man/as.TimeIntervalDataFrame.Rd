\name{as.TimeIntervalDataFrame}
\alias{as.TimeIntervalDataFrame}
\alias{as.TimeIntervalDataFrame.TimeInstantDataFrame}
\title{Convert an object to a TimeIntervalDataFrame}
\usage{
  as.TimeIntervalDataFrame(from, ...)

  \method{as.TimeIntervalDataFrame}{TimeInstantDataFrame}(from,
    period, ...)
}
\arguments{
  \item{from}{object to convert to a TimeIntervalDataFrame}

  \item{\dots}{more args to or from other methods}

  \item{period}{\code{\link{POSIXctp}} object indicating
  the period to add to \sQuote{when} slot of \code{from} to
  determine the end of the new period (the \sQuote{when} is
  used for the start of period)}
}
\value{
  a \code{\link{TimeIntervalDataFrame}}
}
\description{
  Convert an object to a TimeIntervalDataFrame
}

