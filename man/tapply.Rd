\name{tapply}
\alias{tapply}
\alias{tapply,TimeIntervalDataFrame,POSIXcti-method}
\alias{tapply,TimeIntervalDataFrame,POSIXctp-method}
\alias{tapply,TimeIntervalDataFrame,TimeIntervalDataFrame-method}
\title{Apply a Function Over a time properties}
\usage{
  \S4method{tapply}{TimeIntervalDataFrame,TimeIntervalDataFrame}(X, INDEX, FUN, ...,
       min.coverage=1, weights.arg=NULL, merge.X=TRUE, split.X=FALSE,
       keep.INDEX=TRUE,default=NA, simplify=TRUE)

  \S4method{tapply}{TimeIntervalDataFrame,POSIXctp}(X, INDEX, FUN, ...,
       min.coverage=1, weights.arg=NULL, merge.X=TRUE, split.X=FALSE,
      default=NA, simplify=TRUE)

  \S4method{tapply}{TimeIntervalDataFrame,POSIXcti}(X, INDEX, FUN, ...,
       min.coverage=1, weights.arg=NULL, merge.X=TRUE, split.X=FALSE,
      default=NA, simplify=TRUE)
}
\arguments{
  \item{X}{a \code{\link{TimeIntervalDataFrame}} or a
  \code{\link{TimeInstantDataFrame}}}

  \item{INDEX}{an object corresponding to or containing a
  time properties. Classes available depend on X. See
  sections below to know all (X, INDEX) combination
  defined.}

  \item{FUN}{the function to be applied.}

  \item{\dots}{optional arguments to 'FUN'.}

  \item{simplify}{if FALSE a list of
  \sQuote{Time*DataFrame} is returned ; if TRUE
  \sQuote{tapply} try to reduce the list to a single
  \sQuote{Time*DataFrame}.}

  \item{default}{argument inherited from the \pkg{base} function,
  currently unused; see \code{\link[base]{tapply}}.}

  \item{min.coverage}{a numeric between 0 and 1 indicating
  the percentage of valid values over each interval to
  allow an aggregation. NA is returned if the percentage is
  not reach. In that configuration (min.coverage between 0
  and 1, overlapping intervals are not allowed). When a
  function (FUN) has a na.rm argument, the na.rm=TRUE
  behaviour is met if na.rm is set to TRUE and min.coverage
  to 0 (zero) ; the na.rm=FALSE behaviour is met if na.rm
  is set to FALSE whatever is the value of min.coverage. If
  min.coverage is set to NA, time coverage of the resulting
  interval is not checked. Moreover, overlapping of X
  intervals is not checked. Thus the aggregation is done
  according to \sQuote{weights.arg} argument (if given).}

  \item{weights.arg}{if FUN has a \sQuote{weight} argument,
  this parameter must be a character naming the weight
  argument. For instance, if FUN is
  \code{\link[stats]{weighted.mean}}, then weights.arg is
  \code{'w'}.}

  \item{merge.X}{logical indicating if data in \sQuote{X}
  can be merged over interval of the new time support.}

  \item{split.X}{logical indicating if data in \sQuote{X}
  that are over several intervals of 'INDEX' must be 'cut'
  to fit to new intervals (TRUE) or ignored (FALSE).}

  \item{keep.INDEX}{logical indicating if INDEX values must
  be kept on the resulting list.}
}
\description{
  Apply a function over a Time*DataFrame that is first
  splitted into several sets according to time properties
  specified by INDEX.
}
\details{
  These functions are equivalent to old
  \code{\link{changeSupport}} methods. Instead of having
  the core splitting algorithm in it, it uses the
  \code{\link{split}} methods. Be aware that default
  parameters values between the two families
  (\sQuote{changeSupport} and \sQuote{tapply}) are not
  necessarily the same.

  Users are encouraged to use \sQuote{tapply} instead of
  \sQuote{changeSupport} since new versions of
  \sQuote{changeSupport} are only wrappers to tapply.
}
\section{signature(TimeIntervalDataFrame, TimeIntervalDataFrame)}{
  split \code{\link{TimeIntervalDataFrame}} over another
  \code{\link{TimeIntervalDataFrame}} and then apply a
  function over each elements of the list.
}

\section{signature(TimeIntervalDataFrame, POSIXctp)}{
  split a \code{\link{TimeIntervalDataFrame}} against
  regular time intervals with a period defined by INDEX (a
  \code{\link{POSIXctp}}). Then a function is applied over
  each elements of the list.
}

\section{signature(TimeIntervalDataFrame, POSIXcti)}{
  split \code{\link{TimeIntervalDataFrame}} against
  specified intervals (\code{\link{POSIXcti}}). and then
  apply a function over each elements of the list.
}
\seealso{
  \code{\link[base]{tapply}},
  \code{\link{TimeIntervalDataFrame-class}},
  \code{\link{TimeInstantDataFrame-class}},
  \code{\link{SubtimeDataFrame-class}},
  \code{\link{changeSupport}},
  \code{\link{POSIXcti-class}},
  \code{\link{POSIXst-class}}, \code{\link{POSIXctp-class}}
}

