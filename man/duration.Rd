\name{duration}
\alias{duration}
\alias{duration-methods}

\title{Extract duration of a Time object}
\usage{
  duration(x, ...)
}
\arguments{
  \item{x}{object from which get the duration}

  \item{\dots}{arguments to or from other methods}
}

\description{
  For Time objects.
}

\value{
  For \link[=POSIXcti]{time intervals} it returns a vector of
  integers indicating, for each time interval, the duration of 
  the interval in seconds.

  For \link[=POSIXctp]{time periods} it returns a vector of 
  integers indicating the duration of each time period using
  its own time unit. For instance :

  \code{duration(POSIXctp(1:2, c('month', 'year')))}
  \code{> 1 2}
}
\seealso{
  \link{POSIXcti}, \link{POSIXctp}
}
