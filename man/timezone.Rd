\name{timezone}
\alias{timezone}
\alias{timezone<-}
\alias{timezone<--methods}
\alias{timezone-methods}
\alias{timezone.POSIXst}
\alias{timezone,SubtimeDataFrame-method}
\alias{timezone<-,TimeInstantDataFrame-method}
\alias{timezone,TimeInstantDataFrame-method}
\alias{timezone<-,TimeIntervalDataFrame-method}
\alias{timezone,TimeIntervalDataFrame-method}
\title{Get or set timezone property}
\usage{
  timezone(object)
  timezone(object) <- value

  \method{timezone}{POSIXst} (object)

  \S4method{timezone}{SubtimeDataFrame}(object)

  \S4method{timezone}{TimeInstantDataFrame}(object)
  \S4method{timezone}{TimeInstantDataFrame}(object) <- value

  \S4method{timezone}{TimeIntervalDataFrame}(object)
  \S4method{timezone}{TimeIntervalDataFrame}(object) <- value
}
\arguments{
  \item{object}{object to get or set timezone property. }

  \item{value}{specify the new value for timezone. See 
  \link[base]{timezone} in the base package.}
}
\description{
  get or set the timezone of the time object (see
  \code{\link[base]{timezone}} in the base package).
}
\details{
  Changing the timezone of an object consist in reprojecting
  time coordinates from a system of reference to another.
  That is to say that not only the \sQuote{timezone} attribute
  is changed : for instance '2012-02-01 14:OO UTC' will
  be changed in '2012-02-01 15:00 CET' if \sQuote{timezone} is set
  to \sQuote{CET} (French local time).
}
