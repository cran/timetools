\name{as.TimeInstantDataFrame}
\alias{as.TimeInstantDataFrame}
\alias{as.TimeInstantDataFrame.TimeIntervalDataFrame}
\title{Convert an object to a TimeInstantDataFrame}
\usage{
  as.TimeInstantDataFrame(from, ...)

  \method{as.TimeInstantDataFrame}{TimeIntervalDataFrame}(from,
    cursor = NULL, ...)
}
\arguments{
  \item{from}{object to convert to a TimeInstantDataFrame}

  \item{\dots}{more args to or from other methods}

  \item{cursor}{For TimeIntervalDataFrame, it indicates
  where the TimeInstant must be taken. If \code{0}, start
  of each intervals is taken as instant ; if \code{1} end
  of each intervals is taken as instant.  Any other value
  will determine a weigthed instant between start and end
  (actually, value higher than 1 or lower than 0 will give
  instant outside this range).}
}
\value{
  a \code{\link{TimeInstantDataFrame}}
}
\description{
  Convert an object to a TimeInstantDataFrame
}

