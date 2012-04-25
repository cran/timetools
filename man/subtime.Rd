\name{subtime}
\alias{subtime}
\alias{subtime.default}
\alias{subtime.numeric}
\alias{subtime.POSIXct}
\alias{subtime.POSIXlt}
\alias{subtime.TimeInstantDataFrame}
\alias{subtime.TimeIntervalDataFrame}
\title{Extract a specific subtime of of POSIXt object or define a subtime object}
\usage{
  subtime(x, representation, ..., first.day = 0)

  \method{subtime}{default} (x, representation, ...,
    timezone = "UTC", first.day = 0)

  \method{subtime}{numeric} (x, representation, ...,
    timezone = "UTC", first.day = 0)

  \method{subtime}{POSIXct} (x, representation, ...,
    first.day = 0)

  \method{subtime}{POSIXlt} (x, representation, ...,
    first.day = 0)

  \method{subtime}{TimeInstantDataFrame} (x,
    representation, ..., first.day = 0)

  \method{subtime}{TimeIntervalDataFrame} (x,
    representation, cursor = NULL, ..., first.day = 0)
}
\arguments{
  \item{x}{object to which extract subtime}

  \item{representation}{character string indicating which
  subtime is to extract ('mday', 'mon', 'wday', 'yday',
  'sec', 'min', 'hour').}

  \item{\dots}{more arguments to or from other methods}

  \item{timezone}{a character string to specify the
  timezone of subtime}

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
  a factor which depends on the subtime asked for. See
  \sQuote{Details}.
}
\description{
  This function allows to extract a subtime of a time
  object or to build a subtime object (S3).
}
\details{
  \sQuote{subtime} extract one of those indicating in the
  \code{\link[base]{DateTimeClasses}} documentation.

  The result is an ordered factor containing the values
  corresponding to the given argument \sQuote{x}. The
  levels of the factor are all the available values for
  that subtime. To know what are those values, see
  \code{\link[base]{DateTimeClasses}} documentation.

  The factor is given an attributes 'timezone' which
  correspond of the the timezone of the argument.
}
\section{default}{
  If \sQuote{x} is missing, an empty factor with the
  appropriated levels (according to
  \sQuote{representation}) is returned.
}

\section{numeric}{
  If \sQuote{x} is a numeric, a appropriated subtime factor
  is returned (in accordance with representation).
}
\seealso{
  \code{\link[base]{DateTimeClasses}},
  \code{\link[base]{timezone}}
}

