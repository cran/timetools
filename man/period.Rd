\name{period}
\alias{period}
\alias{period-methods}
%\alias{period,TimeIntervalDataFrame-method}

\title{Test or extract different properties of Time objects}
\usage{
  period(x, ...)
}
\arguments{
  \item{x}{object from which get the period}

  \item{\dots}{arguments to or from other methods}
}
\value{
  a time period object if possible. An error occur if not.
}
\description{
  For objects based on time intervals. Return
  \code{\link{POSIXctp}} of the object if it is homogeneous
  and continuous.
}

\seealso{
  \link{TimeIntervalDataFrame},
  \link{POSIXctp}
}
