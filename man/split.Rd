\name{split}
\alias{split}
\alias{split,ANY,POSIXst-method}
\alias{split.POSIXcti}
\alias{split.POSIXctp}
\alias{split.POSIXst}
\alias{split.SubtimeDataFrame}
\alias{split.TimeInstantDataFrame}
\alias{split.TimeIntervalDataFrame}
\alias{split,TimeIntervalDataFrame,POSIXcti-method}
\alias{split,TimeIntervalDataFrame,POSIXctp-method}
\alias{split,TimeIntervalDataFrame,TimeIntervalDataFrame-method}
\title{Divide into Groups and Reassemble (Time*DataFrame objects)}
\usage{
  \method{split}{POSIXctp} (x, f, drop = FALSE, ...)

  \method{split}{POSIXst} (x, f, drop = FALSE, ...)

  \method{split}{POSIXcti} (x, f, drop = FALSE, ...)

  \method{split}{SubtimeDataFrame} (x, f, drop = FALSE, ...)

  \method{split}{TimeInstantDataFrame} (x, f, drop = FALSE, ...)

  \method{split}{TimeIntervalDataFrame} (x, f, drop = FALSE, ...)

  \S4method{split}{ANY,POSIXst}(x, f, drop = FALSE, ...)

  \S4method{split}{TimeIntervalDataFrame,POSIXcti}(x, f, ..., split.x=FALSE)

  \S4method{split}{TimeIntervalDataFrame,POSIXctp}(x, f, ...,  split.x=FALSE)

  \S4method{split}{TimeIntervalDataFrame,TimeIntervalDataFrame}(x, f, ..., split.x=FALSE, keep.f=TRUE)

}
\arguments{
  \item{\dots}{further potential arguments passed to
  methods.}

  \item{split.x}{logical indicating if data in \sQuote{x}
  that are over several intervals of 'f' must be 'cut' to
  fit to new intervals (TRUE) or ignored (FALSE).}

  \item{keep.f}{logical indicating if f values must be kept
  on the resulting list.}

  \item{x}{vector or data frame containing values to be
  divided into groups.}

  \item{f}{a \sQuote{factor} in the sense that
  \code{\link{as.factor}(f)} defines the grouping, or a
  list of such factors in which case their interaction is
  used for the grouping.}

  \item{drop}{logical indicating if levels that do not
  occur should be dropped (if \code{f} is a \code{factor}
  or a list).}
}
\value{
  The value returned from ‘split’ is a list of vectors
  containing the values for the groups.  The components of
  the list are named by the levels of ‘f’ (after converting
  to a factor).
}
\description{
  \sQuote{split} divides the data in the vector \sQuote{x}
  into the groups defined by \sQuote{f}.  The replacement
  forms replace values corresponding to such a division.
  Here are listed \sQuote{split} methods defined for Time
  objects defined in the timetools package
  (\code{\link{TimeIntervalDataFrame}},
  \code{\link{TimeInstantDataFrame}},
  \code{\link{POSIXst}}, \code{\link{POSIXcti}}, etc.). See
  sections below for complete list of methods.
}
\details{
  For each new split method defined in
  \code{\link{timetools}} a short description is given
  there. If the method is an S3 method, the class of the
  first argument only is given ; otherwise the complete
  signature is given (for S4 methods).
}
\section{POSIXctp}{
  split POSIXctp objects.
}

\section{POSIXst}{
  split POSIXst objects.
}

\section{POSIXcti}{
  split POSIXcti objects.
}

\section{SubtimeDataFrame}{
  split SubtimeDataFrame objects.
}

\section{TimeInstantDataFrame}{
  split TimeInstantDataFrame objects.
}

\section{TimeIntervalDataFrame}{
  split TimeIntervalDataFrame objects.
}

\section{signature(ANY, POSIXst)}{
  split any objects over \code{\link{POSIXst}}.
}

\section{signature(TimeIntervalDataFrame, TimeIntervalDataFrame)}{
  split \code{\link{TimeIntervalDataFrame}} over another
  \code{\link{TimeIntervalDataFrame}}. This methode
  actually act more or less like the function
  \code{\link{changeSupport}} with the split.from argument
  set to TRUE. The difference is the output : there the
  result is a list of TimeIntervalDataFrame whereas the
  result of changeSupport is a TimeIntervalDataFrame.
}

\section{signature(TimeIntervalDataFrame, POSIXctp)}{
  split a \code{\link{TimeIntervalDataFrame}} against
  regular time intervals with a period defined by f (a
  \code{\link{POSIXctp}}).
}

\section{signature(TimeIntervalDataFrame, POSIXcti)}{
  split \code{\link{TimeIntervalDataFrame}} against
  specified intervals f (\code{\link{POSIXcti}}).
}
\seealso{
  \code{\link[base]{split}},
  \code{\link{TimeIntervalDataFrame-class}},
  \code{\link{TimeInstantDataFrame-class}},
  \code{\link{SubtimeDataFrame-class}},
  \code{\link{changeSupport}},
  \code{\link{POSIXcti-class}},
  \code{\link{POSIXst-class}}, \code{\link{POSIXctp-class}}
}

