\name{overlapping}
\alias{overlapping}
\alias{overlapping-methods}
%\alias{overlapping,TimeIntervalDataFrame-method}

\title{Test if any interval of a time intervals object intersect another}
\usage{
  overlapping(x, ...)
}
\arguments{
  \item{x}{object to test for overlapping}

  \item{\dots}{arguments to or from other methods}
}
\value{
  logical indicating if 'x' has any overlapping time interval.
}
\description{
  For objects based on time intervals. Test if any interval
  overlap another one. Because the test can be ressource
  consuming, it stops at the first case encoutered that
  does not satisfy this condition. The two indices
  corresponding are printed.
}

\seealso{
  \link{TimeIntervalDataFrame}, \link{POSIXcti}
}
