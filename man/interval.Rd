\name{interval}
\alias{interval}
\alias{interval-methods}
\alias{interval,TimeIntervalDataFrame-method}
\title{Extract time intervals of a time object.}
\usage{
  interval(x, ...)

  \S4method{interval}{TimeIntervalDataFrame}(x, ...)
}
\arguments{
  \item{x}{object from which get time intervals}

  \item{\dots}{arguments to or from other methods}
}

\description{
  For objects based on time intervals. Return
  \code{\link{POSIXcti}}.
}


\seealso{
  \link{TimeIntervalDataFrame}, \link{POSIXcti}
}
