\name{TimeInstantDataFrame}
\alias{TimeInstantDataFrame}
\title{Create a TimeInstantDataFrame from scratch}
\usage{
  TimeInstantDataFrame(when, timezone = "UTC", data = NULL,
    ...)
}
\arguments{
  \item{when}{POSIXct or character representing a time with
  a valid format (see
  \code{\link[base:as.POSIXct]{POSIXct}}). It gives the
  instant of each row.}

  \item{timezone}{character representing a valid timezone
  (see \code{\link[base]{timezone}}).}

  \item{data}{a data.frame with as much rows as the length
  of \sQuote{when}. Can be \code{NULL} (hence the
  data.frame has zero column and as much rows as needed).}

  \item{\dots}{arguments to or from other methods}
}
\value{
  a
  \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}}
  object.
}
\description{
  To see all methods related to this class, see
  \code{\link{TimeInstantDataFrame-class}}
}
\details{
  If both \code{when} and \code{data} are given,
  \code{data} must have a number of rows identical to the
  length of \code{when}.

  To access to the class documentation, type in the R
  console :

  \code{class?TimeInstantDataFrame}
}
\examples{
TimeInstantDataFrame (
	c('2010-01-01', '2010-02-01'),
	'UTC', data.frame(ex=1:2) )

TimeInstantDataFrame (c('2010-01-01', '2010-02-01', '2010-02-02'), 'UTC')
}
\seealso{
  \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}},
  \code{\link{RegularTimeInstantDataFrame}},
  \code{\link{timetools}}
}

