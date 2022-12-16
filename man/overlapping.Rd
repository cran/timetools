\name{overlapping}
\alias{overlapping}
\alias{overlapping-methods}
%\alias{overlapping,TimeIntervalDataFrame-method}
\alias{overlapping,TimeIntervalDataFrame,ANY-method}
\alias{overlapping,TimeIntervalDataFrame,logical-method}

\title{Test if any interval of a time intervals object intersect another}
\usage{
  overlapping(x, idx, ...)

  \S4method{overlapping}{TimeIntervalDataFrame,ANY}(x, idx, ...)
  \S4method{overlapping}{TimeIntervalDataFrame,logical}(x, idx, ...)
}
\arguments{
  \item{x}{object to test for overlapping}
  \item{idx}{Logical set to TRUE if indexes of all overlapping
  intervals are to retrieve.}

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
