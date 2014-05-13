\name{unit}
\alias{units}
\alias{POSIXt.units}
\alias{unit}
\alias{unit-methods}
\alias{unit<-}
\alias{unit<--methods}
\alias{of}
\title{define valid units for time objects/retrieve-set time unit of a time object}
\usage{
  POSIXt.units(x = NULL, ...)

  unit(x, ...)
  unit(object) <- value
  of(x, ...)
}
\arguments{
  \item{x}{a character string representing the needed
  units for \code{POSIXt.units}. The object from which
  the time unit is to retrieve.}

  \item{object}{POSIXctp to which the unit is to be
  changed}

  \item{value}{a character or a \code{\link{POSIXt.units}}
  indicating the new units of object.}

  \item{\dots}{arguments to or from other methods}
}
\description{
  The timetools package use a set of valid time units which are
  roughly : year, month, week, day, hour, minute, second. 
  They can be combined in \code{\link[=POSIXst-class]{subtime objects}}.
  For instance : month of year, minute of day, minute of week, etc.
}

\section{POSIXt.units(x = NULL, ...)}{
  With no argument, the function return a factor
  containing the valid time units. With an argument, it
  returns the units asked for.
}

\section{unit(x, ...)}{
  Return the time unit of the object. In case 'x' is a 
  \code{\link{POSIXst}}, the unit is the 'left' part of 
  its unit : if 'x' is a 'minute of day', 'unit' will return
  'minute'.
}

\section{of(x, ...)}{
  For \code{\link{POSIXst}} only, it return the 'right' part
  of the unit of 'x' : if 'x' is a 'minute of day', 'of' will return
  'day'.
}

