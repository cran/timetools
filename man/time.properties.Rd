\name{timezone}
\alias{continuous}
\alias{continuous<-}
\alias{continuous<--methods}
\alias{continuous-methods}
\alias{continuous<-,TimeIntervalDataFrame-method}
\alias{continuous,TimeIntervalDataFrame-method}
\alias{homogeneous}
\alias{homogeneous-methods}
\alias{homogeneous,TimeIntervalDataFrame-method}
\alias{interval}
\alias{interval-methods}
\alias{interval,TimeIntervalDataFrame-method}
\alias{overlapping}
\alias{overlapping-methods}
\alias{overlapping,TimeIntervalDataFrame-method}
\alias{period}
\alias{period-methods}
\alias{period,TimeIntervalDataFrame-method}
\alias{regular}
\alias{regular-methods}
\alias{regular,TimeInstantDataFrame-method}
\alias{regular,TimeIntervalDataFrame-method}
\alias{time.properties}
\alias{timezone}
\alias{timezone<-}
\alias{timezone<--methods}
\alias{timezone-methods}
\alias{timezone.subtime}
\alias{timezone,SubtimeDataFrame-method}
\alias{timezone<-,TimeInstantDataFrame-method}
\alias{timezone,TimeInstantDataFrame-method}
\alias{timezone<-,TimeIntervalDataFrame-method}
\alias{timezone,TimeIntervalDataFrame-method}
\alias{when}
\alias{when-methods}
\alias{when,SubtimeDataFrame-method}
\alias{when,TimeInstantDataFrame-method}
\alias{when,TimeIntervalDataFrame-method}
\title{Test or extract different properties of Time objects}
\usage{
  timezone(object)

  continuous(x, ...)

  continuous(x) <- value

  homogeneous(x, ...)

  overlapping(x, ...)

  period(x, ...)

  when(x, ...)

  regular(x, ...)

  timezone(object) <- value

  interval(x, ...)

  \method{timezone}{subtime} (object)
}
\arguments{
  \item{object}{object to test, from which get or set a
  property}

  \item{x}{object to test, from which get or set a
  property}

  \item{value}{boolean if possible, set this value to the
  property}

  \item{\dots}{arguments to or from other methods}
}
\value{
  depends on the method
}
\description{
  These functions can be applied to Time objects such as
  \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}},
  \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}}
  or \code{\link{subtime}}.
}
\section{reminder}{
  For each class, you can type the following code to know
  what methods are available.

  \code{methods(class='myclass')} #S3 methods\cr
  \code{showMethods(class='myclass')} #S4 'methods\cr

  For each S3 method, you can type the following code to
  know for which classes it is defined.

  \code{methods('mymethod')}

  For each S4 method, you can type the following code to
  know for which classes it is defined.

  \code{showMethods('mymethod')}
}

\section{continuous}{
  For objects based on time intervals. After ordering
  intervals, test if the end of an interval is the start of
  the next interval. If any interval overlap another one,
  it returns \code{FALSE}.

  If not any interval overlap another, and the object is
  not continuous, the object can be set 'continuous' with

  \code{continuous(obj) <- TRUE}

  Interval will be added such as the object can pass the
  test describe below. The data is filled with NA values.
}

\section{homogeneous}{
  For objects based on time intervals
  (\code{\link{POSIXcti}}). Test if intervals of the object
  are \sQuote{homogeneous} : if the period of each interval
  is the same.
}

\section{overlapping}{
  For objects based on time intervals. Test if any interval
  overlap another one. Because the test can be ressource
  consuming, it stops at the first case encoutered that
  does not satisfy this condition. The two indices
  corresponding are printed.
}

\section{period}{
  For objects based on time intervals. Return
  \code{\link{POSIXctp}} of the object if it is homogeneous
  and continuous.
}

\section{when}{
  For Time objects. If TimeInstantDataFrame, return the
  instants of the object ; if TimeIntervalDataFrame, return
  the intervals of the object.
}

\section{regular}{
  Test if the object is regular. A TimeInstantDataFrame is
  regular if all instants are equally spaced. A
  TimeIntervalDataFrame is regular if it is homogeneous and
  all interval's start are equally spaced.
}

\section{timezone}{
  get or set the timezone of the time object (see
  \code{\link[base]{timezone}} in the base package).
}

\section{interval}{
  For objects based on time intervals. Return
  \code{\link{POSIXcti}} of each interval.
}

