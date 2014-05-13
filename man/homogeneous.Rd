\name{homogeneous}
\alias{homogeneous}
\alias{homogeneous-methods}
%\alias{homogeneous,TimeIntervalDataFrame-method}

\title{Test if a time object is homogeneous}
\usage{
  homogeneous(x, ...)
}
\arguments{
  \item{x}{object to test for homogeneity}

  \item{\dots}{arguments to or from other methods}
}
\value{
  logical indicating if 'x' is homogeneous or not.
}
\description{
  For objects based on time intervals
  (\code{\link{POSIXcti}}). Test if intervals of the object
  are \sQuote{homogeneous} : if the period of each interval
  is the same.
}

\seealso{
  \link{TimeIntervalDataFrame}, \link{POSIXcti}
}
