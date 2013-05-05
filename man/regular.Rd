\name{regular}
\alias{regular}
\alias{regular-methods}
\alias{regular,TimeInstantDataFrame-method}
\alias{regular,TimeIntervalDataFrame-method}
\title{Test if a time object is regular}
\usage{

  regular(x, ...)

  \S4method{regular}{TimeInstantDataFrame}(x, ...)

  \S4method{regular}{TimeIntervalDataFrame}(x, ...)

}
\arguments{
  \item{x}{object to test, from which get or set a
  property}

  \item{\dots}{arguments to or from other methods}
}
\value{
  boolean indicating if x is regular or not
}
\description{
  Test for regularity of a time object.
}

\details{
  Test if the object is regular. A TimeInstantDataFrame is
  regular if all instants are equally spaced. A
  TimeIntervalDataFrame is regular if it is \link{homogeneous} and
  all interval's starts are equally spaced.
}


\seealso{
  \link{TimeIntervalDataFrame},
  \link{TimeInstantDataFrame}
}
