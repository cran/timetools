\name{continuous}
\alias{continuous}
\alias{continuous<-}
\alias{continuous<--methods}
\alias{continuous-methods}
%\alias{continuous,TimeIntervalDataFrame-method}
%\alias{continuous<-,TimeIntervalDataFrame-method}

\title{Test if a time object is continuous/set an time object continuous.}
\usage{
  continuous(x, ...)
  continuous(x) <- value
}
\arguments{
  \item{x}{object to test}

  \item{value}{logical indicating whether x must be 'continuify' or not.}

  \item{\dots}{arguments to or from other methods}
}
\value{
  Logical indicating if the object is continuous or not.

  or

  The object set continuous.
}

\description{
  For Time objects.
}

\details{
  For objects based on time intervals. After ordering
  intervals, test if the end of an interval is the start of
  the next interval. If any interval overlap another one,
  it returns \code{FALSE}.

  If not any interval overlap another, and the object is
  not continuous, the object can be set 'continuous' with

  \code{continuous(obj) <- TRUE}

  Intervals will be added such as the object can pass the
  test describe below. The data is filled with NA values.
}

\seealso{
  \link{TimeIntervalDataFrame}, \link{POSIXcti}
}
