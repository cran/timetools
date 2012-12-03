\name{year}
\alias{day}
\alias{day,ANY-method}
\alias{day-methods}
\alias{duration}
\alias{duration-methods}
\alias{duration,POSIXctp-method}
\alias{hour}
\alias{hour,ANY-method}
\alias{hour-methods}
\alias{minute}
\alias{minute,ANY-method}
\alias{minute-methods}
\alias{month}
\alias{month,ANY-method}
\alias{month-methods}
\alias{posix.properties}
\alias{second}
\alias{second,ANY-method}
\alias{second-methods}
\alias{unit}
\alias{unit<-}
\alias{unit<--methods}
\alias{unit-methods}
\alias{unit<-,POSIXctp-method}
\alias{unit,POSIXctp-method}
\alias{year}
\alias{year,ANY-method}
\alias{year-methods}
\title{Extract time properties from POSIX* objects}
\usage{
  year(x, ...)

  month(x, ...)

  day(x, of, ...)

  hour(x, of, ...)

  minute(x, of, ...)

  second(x, of, ...)

  unit(x, ...)

  unit(object) <- value

  duration(x, ...)
}
\arguments{
  \item{x}{object from which the property is to be
  extracted (POSIXct, POSIXcti, POSIXctp)}

  \item{of}{used to specify the main period from wich the
  time propertie is to extract. For instance : \code{day(x,
  'week')} will differ from \code{day(x, 'year')}.}

  \item{\dots}{arguments to or from other methods}

  \item{object}{POSIXctp to which the unit is to be
  changed}

  \item{value}{a character or a \code{\link{POSIXt.units}}
  indicating the new units of object. The conversion will
  be effective only if it makes sense (\sQuote{hour} to
  \sQuote{second}, ok ; \sQuote{year} to \sQuote{month}, ok
  ; \sQuote{month} to \sQuote{minute} , NOT ok ; etc.}
}
\description{
  Objects can be POSIXct, POSIXcti, POSIXctp, etc.
}
\details{
  \code{year}, \code{month}, \code{day}, \code{hour},
  \code{minute} and \code{second} are methods defined to
  extract the adequat information from a time object. These
  functions are wrappers to \code{\link{POSIXst}}.

  \code{unit} \code{unit<-} is used to extract or set the
  unit of a time object (cf \code{\link{POSIXt.units}}).
}

