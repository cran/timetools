\name{TimeIntervalDataFrame}
\alias{TimeIntervalDataFrame}
\title{Create a TimeIntervalDataFrame from scratch}
\usage{
  TimeIntervalDataFrame(start, end = NULL,
    timezone = "UTC", data = NULL, period = NULL, ...)
}
\arguments{
  \item{start}{POSIXct or character representing a time
  with a valid format (see
  \code{\link[base:as.POSIXct]{POSIXct}}). It gives the
  begining of each interval.}

  \item{end}{POSIXct or character representing a time with
  a valid format (see
  \code{\link[base:as.POSIXct]{POSIXct}}). It gives the end
  of each interval. If NULL, see \sQuote{Details}.}

  \item{timezone}{character representing a valid timezone
  (see \code{\link[base]{timezone}}).}

  \item{data}{a data.frame with as much rows as the length
  of \sQuote{start} and end, or with one row less than the
  length of \sQuote{start} if \sQuote{end} is \code{NULL}.
  Can be \code{NULL} (hence the data.frame has zero column
  and as much rows as needed).}

  \item{period}{if not NULL, a \code{\link{POSIXctp}} or a
  character that can be converted to a
  \code{\link{POSIXctp}} (see argument \sQuote{unit} of
  POSIXctp function). See Details to know how to use this
  argument.}

  \item{\dots}{arguments to or from other methods}
}
\value{
  a
  \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
  object.
}
\description{
  To see all methods related to this class, see
  \code{\link{TimeIntervalDataFrame-class}}
}
\details{
  If both \code{start} and \code{end} are given, they must
  have the same length. They are used to define the
  intervals of the object. If \code{data} is also given, it
  must have a number of rows identical to the length of
  \code{start} and \code{end}.

  If only \code{start} is given, a continuous (see
  \code{\link{continuous}}) TimeIntervalDataFrame is built.
  The first element of \code{start} is the start of the
  first interval, the second element is the end of the
  first interval and the start of the second interval. The
  last element of \code{start} is only the end of the last
  interval. This is why \code{data}, if given, must be one
  row shorter than \code{start}.

  If period is given it must be a \code{\link{POSIXctp}}
  object (or a valid character) and \sQuote{start} and
  \sQuote{end} must have length equal to 1. In that case, a
  TimeIntervalDataFrame will be created with start date
  equal to start \sQuote{floored} by the unit of
  \sQuote{period}, end date \sQuote{ceiled} by the unit of
  \sQuote{period} and with enough intervals of
  \sQuote{period} length to fit. If \sQuote{data} given, it
  must have a number of rows equal to the number of
  intervals calculated.

  To access to the class documentation, type in the R
  console :

  \code{class?TimeIntervalDataFrame}
}
\examples{
TimeIntervalDataFrame (
	c('2010-01-01', '2010-02-01'), c('2010-02-01', '2010-02-02'),
	'UTC', data.frame(ex=1:2) )

TimeIntervalDataFrame (
	c('2010-01-01', '2010-02-01', '2010-02-02'), NULL,
	'UTC', data.frame(ex=1:2) )

TimeIntervalDataFrame(
	as.POSIXct('2013-01-01'), as.POSIXct('2013-03-01'),
	period=POSIXctp('day')
)
}
\seealso{
  \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}},
  \code{\link{RegularTimeIntervalDataFrame}},
  \code{\link{timetools}}
}

