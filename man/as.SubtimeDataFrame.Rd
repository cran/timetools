\name{as.SubtimeDataFrame}
\alias{as.SubtimeDataFrame}
\alias{as.SubtimeDataFrame.TimeInstantDataFrame}
\alias{as.SubtimeDataFrame.TimeIntervalDataFrame}
\title{Convert an object to a SubtimeDataFrame}
\usage{
  as.SubtimeDataFrame(x, unit, of, ...)

  \method{as.SubtimeDataFrame}{TimeInstantDataFrame}(x,
    unit, of, FUN=NULL, ...)

  \method{as.SubtimeDataFrame}{TimeIntervalDataFrame}(x,
    unit, of, FUN=NULL, cursor=NULL, ...)
}
\arguments{
  \item{x}{object to convert to a TimeInstantDataFrame}

  \item{\dots}{more args to or from other methods or FUN}

  \item{FUN}{function to use for the agregation (if wanted,
  see \sQuote{details})}

  \item{unit}{indicates the subtime part to extract
  ('year', 'month', 'day', 'hour', 'minute', 'second')}

  \item{of}{used to specify the main period from wich the
  is to extract ('year', 'month', day', 'hour', 'minute').
  Not used for \sQuote{unit in c('year', 'month')}.}

  \item{cursor}{for TimeIntervalDataFrame, if not NULL, the
  object is first coerced to a TimeInstantDataFrame using
  the \code{\link{as.TimeInstantDataFrame}} method.}
}
\value{
  a \code{\link{SubtimeDataFrame}}
}
\description{
  Convert an object to a SubtimeDataFrame
}
\section{TimeIntervalDataFrame}{
  Conversion from a TimeIntervalDataFrame to a
  SubtimeDataFrame can be direct or after agregation.

  For a direct conversion (where date are only replaced by
  the desired subtime), FUN must be NULL.

  For an agregateed conversion, the function to use must be
  indicated by the FUN arg and all arguments to pass to
  this function can be given (namely).
}

\section{TimeIntervalDataFrame}{
  If \sQuote{from} is a
  \code{\link{TimeIntervalDataFrame}}, data is first
  converted to a TimeInstantDataFrame (see
  \code{\link{as.TimeInstantDataFrame}}). Then, this
  TimeInstantDataFrame is converted to a SubtimeDataFrame
  (see the appropriated section).
}

