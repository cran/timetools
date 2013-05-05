\name{POSIXst}
\alias{POSIXst}
\alias{POSIXst.default}
\alias{POSIXst.integer}
\alias{POSIXst.numeric}
\alias{POSIXst.POSIXct}
\alias{POSIXst.POSIXlt}
\alias{POSIXst.TimeInstantDataFrame}
\alias{POSIXst.TimeIntervalDataFrame}
\alias{day}
\alias{day,ANY-method}
\alias{day-methods}
\alias{hour}
\alias{hour,ANY-method}
\alias{hour-methods}
\alias{minute}
\alias{minute,ANY-method}
\alias{minute-methods}
\alias{month}
\alias{month,ANY-method}
\alias{month-methods}
\alias{second}
\alias{second,ANY-method}
\alias{second-methods}
\alias{year}
\alias{year,ANY-method}
\alias{year-methods}
\title{Create an object of class POSIXst (subtime object) (S4 class)}
\usage{
  POSIXst(x, unit, of = NULL, tz = "UTC", ...)

  \method{POSIXst}{default} (x, unit, of = NULL, tz = "UTC", ...)

  \method{POSIXst}{integer} (x, unit, of = NULL, tz = "UTC", ...)
  \method{POSIXst}{numeric} (x, unit, of = NULL, tz = "UTC", ...)

  \method{POSIXst}{POSIXct} (x, unit, of = NULL, tz = attributes(x)$tzone, ...)
  \method{POSIXst}{POSIXlt} (x, unit, of = NULL, tz = attributes(x)$tzone, ...)

  \method{POSIXst}{TimeInstantDataFrame} (x, unit, of = NULL, tz = timezone(x), ...)

  \method{POSIXst}{TimeIntervalDataFrame} (x, unit, of = NULL, tz = timezone(x), ..., cursor = NULL)

  year(x, ...)
  month(x, ...)
  day(x, of, ...)
  hour(x, of, ...)
  minute(x, of, ...)
  second(x, of, ...)
}
\arguments{
  \item{x}{object to convert into POSIXst}

  \item{unit}{indicates the subtime part to extract
  ('year', 'month', 'day', 'hour', 'minute', 'second')}

  \item{of}{used to specify the main period from wich the
  is to extract ('year', 'month', day', 'hour', 'minute').
  Not used for \sQuote{unit in c('year', 'month')}.}

  \item{tz}{if needed, specifies th timezone of POSIXst}

  \item{\dots}{more arguments to or from other methods}

  \item{cursor}{for TimeIntervalDataFrame, if not NULL, the
  object is first coerced to a TimeInstantDataFrame using
  the \code{\link{as.TimeInstantDataFrame}} method.}
}
\value{
  a factor which depends on the subtime asked for. See
  \sQuote{Details}.
}
\description{
  Create an object of class POSIXst (subtime object) (S4
  class)
}
\section{POSIXst('default', unit, of = NULL, tz = "UTC", ...)}{
  If \sQuote{x} is missing, an empty factor with the
  appropriated levels (according to \sQuote{unit and of})
  is returned.
}

\section{POSIXst('integer', x, unit, of = NULL, tz = "UTC", ...)}{
  This is the default constructor.
}

\section{POSIXst('numeric', unit, of = NULL, tz = "UTC", ...)}{
  If \sQuote{x} is a numeric, its values must be in the
  right range (see \code{\link[base]{DateTimeClasses}} and
  must be "like" an integer.
}

\section{POSIXst('POSIXct', unit, of = NULL, tz = attributes(x)$tzone, ...)}{
  with the \sQuote{tz} argument, one can specify/change the
  timezone of the resulting POSIXst
}

\section{POSIXst('POSIXlt', unit, of = NULL, tz = attributes(x)$tzone, ...)}{
  with the \sQuote{tz} argument, one can specify/change the
  timezone of the resulting POSIXst
}

\section{POSIXst('TimeInstantDataFrame', unit, of = NULL, tz = timezone(x), ...)}{
  This methode extract subtimes (\code{\link{POSIXst}}),
  from \code{\link{TimeInstantDataFrame}}.

  with the \sQuote{tz} argument, one can specify/change the
  timezone of the resulting POSIXst
}

\section{POSIXst('TimeIntervalDataFrame', unit, of = NULL, tz = timezone(x), ..., cursor = NULL)}{
  Because an time interval can contain several POSIXst of
  one kind for instance a day contains all 'hours of day'),
  the result of this function for TimeIntervalDataFrame is
  a list of POSIXst. Each element of the list contains the
  POSIXsts asked for corresponding to each row of the
  TimeIntervalDataFrame object.
}

\section{year(...), month(...), day(...), hour(...), minute(...) and second(...)}{
  \code{year}, \code{month}, \code{day}, \code{hour},
  \code{minute} and \code{second} are methods defined to
  extract the adequat information from a time object. These
  functions are wrappers to \code{POSIXst}.
  
  Each of these methods call POSIXst replacing the \code{unit}
  argument with its own name :

  \code{minute(x, of='day')} will call

  \code{POSIXst(x, unit='minute', of='day')}
}
\seealso{
  \code{\link{timezone}}, \code{\link{POSIXt.units}},
  \code{\link{POSIXst-class}}
}

