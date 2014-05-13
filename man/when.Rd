\name{when}
\alias{when}
\alias{when-methods}
%\alias{when,SubtimeDataFrame-method}
%\alias{when,TimeInstantDataFrame-method}
%\alias{when,TimeIntervalDataFrame-method}

\title{Retrieve the 'timestamp' of a Time*DataFrame}

\usage{
  when(x, ...)
}
\arguments{
  \item{x}{object from which get the timestamp}

  \item{\dots}{arguments to or from other methods}
}

\description{
  For Time objects.
}

\value{
  If TimeInstantDataFrame, return the instants of the object ;
  
  if TimeIntervalDataFrame, return the intervals of the object.

  if SubtimeDataFrame, return the \link{POSIXst} of the object.
}

\seealso{
  \link{TimeIntervalDataFrame}, \link{POSIXcti}, 
  \link{TimeInstantDataFrame}, \link[base]{POSIXct},
  \link{SubtimeDataFrame}, \link{POSIXst}
}
