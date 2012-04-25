\name{as.SubtimeDataFrame}
\alias{as.SubtimeDataFrame}
\alias{as.SubtimeDataFrame.TimeInstantDataFrame}
\alias{as.SubtimeDataFrame.TimeIntervalDataFrame}
\title{Convert an object to a SubtimeDataFrame}
\usage{
  as.SubtimeDataFrame(from, ...)

  \method{as.SubtimeDataFrame}{TimeInstantDataFrame}(from,
    representation, FUN=mean, ..., first.day=0)

  \method{as.SubtimeDataFrame}{TimeIntervalDataFrame}(from,
    representation, cursor=NULL, FUN=mean, ...,
    first.day=0)
}
\arguments{
  \item{from}{object to convert to a TimeInstantDataFrame}

  \item{\dots}{more args to or from other methods or FUN}

  \item{FUN}{function to use for the agregation (see
  \sQuote{details})}

  \item{representation}{character string indicating which
  subtime is to extract ('mday', 'mon', 'wday', 'yday',
  'sec', 'min', 'hour').}

  \item{first.day}{used only if \code{representation ==
  'wday'}. Interger indicating the first day of the week. 0
  -> sunday (the default), 1 -> monday, 2 -> tuesday, etc.}

  \item{cursor}{indicates where the TimeInstant must be
  taken. If \code{0}, start of each intervals is taken as
  instant ; if \code{1} end of each intervals is taken as
  instant.  Any other value will determine a weigthed
  instant between start and end (actually, value higher
  than 1 or lower than 0 will give instant outside this
  range).}
}
\value{
  a \code{\link{SubtimeDataFrame}}
}
\description{
  Convert an object to a SubtimeDataFrame
}
\section{TimeIntervalDataFrame}{
  If \sQuote{from} is a \code{\link{TimeInstantDataFrame}},
  data must be agregated befor conversion. The function to
  use is specified by \sQuote{FUN}. The default function is
  \code{\link[base]{mean}}.
}

\section{TimeIntervalDataFrame}{
  If \sQuote{from} is a
  \code{\link{TimeIntervalDataFrame}}, data is first
  converted to a TimeInstantDataFrame (see
  \code{\link{as.TimeInstantDataFrame}}). Then, this
  TimeInstantDataFrame is converted to a SubtimeDataFrame
  (see the appropriated section).
}

