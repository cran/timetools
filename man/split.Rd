\name{split}
\alias{split}
\alias{split,ANY,POSIXst-method}
\alias{split,ANY,POSIXctp-method}
\alias{split,ANY,POSIXcti-method}
%\alias{split.SubtimeDataFrame}
%\alias{split.TimeInstantDataFrame}
%\alias{split.TimeIntervalDataFrame}
\alias{split,TimeIntervalDataFrame,POSIXcti-method}
\alias{split,TimeIntervalDataFrame,POSIXctp-method}
\alias{split,TimeIntervalDataFrame,TimeIntervalDataFrame-method}
\title{Divide into Groups and Reassemble (Time*DataFrame objects and POSIXct*)}
\usage{
  \S4method{split}{ANY,POSIXctp}(x, f, drop = FALSE, ...)
  \S4method{split}{ANY,POSIXcti}(x, f, drop = FALSE, ...)
  \S4method{split}{ANY,POSIXst}(x, f, drop = FALSE, ...)

  \S4method{split}{TimeIntervalDataFrame,TimeIntervalDataFrame}(x, f, ..., split.x=FALSE, keep.f=TRUE)
  \S4method{split}{TimeIntervalDataFrame,POSIXcti}(x, f, ..., split.x=FALSE)
  \S4method{split}{TimeIntervalDataFrame,POSIXctp}(x, f, ...,  split.x=FALSE)
}
\arguments{
  \item{x}{data frame containing values to be divided into groups.
	  TimeIntervalDataFrame as data.frame.}

  \item{f}{Can be of different kind. Is used to defined the grouping.
  See details below.}

  \item{drop}{logical indicating if levels that do not
  occur should be dropped (if \code{f} is a \code{factor}
  or a list).}

  \item{\dots}{further potential arguments passed to
  methods.}

  \item{split.x}{logical indicating if data in \code{x}
  that are over several intervals or not completely included in
  one interval of \code{f} must be 'cut' to
  fit to new intervals (TRUE) or ignored (FALSE).}

  \item{keep.f}{logical indicating if \code{f} values must be kept
  on the resulting list.}
}
\value{
  The value returned from ‘split’ is a list of vectors
  containing the values for the groups.  The components of
  the list are named by the levels of \code{f} (after converting
  to a factor). The class of each element of the list is the one 
  of the initial \code{x} structure.
}
\description{
  \sQuote{split} divides the data in the vector \sQuote{x}
  into the groups defined by \sQuote{f}.  The replacement
  forms replace values corresponding to such a division.
  Here are listed \sQuote{split} methods defined for Time
  objects defined in the timetools package
  \code{\link{POSIXst}}, \code{\link{POSIXcti}}, etc.). See
  sections below for complete list of methods.
}
\details{
  For each new split method defined in
  \code{\link{timetools}} a short description is given
  there.
}

\section{Split over POSIX*t* objects}{
  Since POSIXst, POSIXctp and POSIXcti objects are similar to vector
  it must be possible to split other type of objects against those ones.

  \describe{
    \item{split(x, '\link{POSIXctp}', drop=FALSE, ...)}{'POSIXctp' is first
    cast as character (using format) and then the split is done.}
    \item{split(x, '\link{POSIXcti}', drop=FALSE, ...)}{'POSIXcti' is first
    cast as character (using format) and then the split is done.
    The \code{...} argument is used to specify the format if needed.}
    \item{split(x, '\link{POSIXst}', drop=FALSE, ...)}{'POSIXst' is first
    cast as numeric and then the split is done.}
  }
}
\section{split('TimeIntervalDataFrame', 'TimeIntervalDataFrame', ..., split.x = FALSE, keep.f = TRUE)}{
  Split a TimeIntervalDataFrame into another TimeIntervalDataFrame.

  The method takes each time interval of the first TimeIntervalDataFrame
  (TitDF) and searches with which time intervals of the second it
  intersects.

  Each time interval of the first TItDF can intersect with
  none, one or several time intervals of the second TItDF. The arguments
  'split.x' is defined to tell the method what to do : \itemize{
    \item{if the time interval in the first TItDF (ti1) doesn't match
      any in the second TItDF, nothing to do}
    \item{if it (ti1) matches one in the second TItDF (ti2) and is
      included inside it, it (ti1) is entirely taken in the final result}
    \item{if it (ti1) intersects one and only one (ti2) inside the second
      TItDF, (ti1) is truncated to be included inside (ti2) if 'split.x' is
      TRUE and (ti1) is removed if 'split.x' is FALSE}
    \item{if it (ti1) is over several time intervals of the second TItDF
      (ti2.a, ti2.b, etc.) :\itemize{
        \item{if 'split.x' is TRUE, (ti1) is truncated into each ti2.x to be
          included inside each one}
        \item{if 'split.x' is FALSE, (ti1) is removed.}
      }
    }
  }
}

\section{split('TimeIntervalDataFrame', 'POSIXctp', ..., split.x = FALSE)}{
  Split a TimeIntervalDataFrame into a time period (of length 1).
  A TimeIntervalDataFrame is created (cf TimeIntervalDataFrame
  constructor) and the the above method is called.
}

\section{split('TimeIntervalDataFrame', 'POSIXcti', ..., split.x = FALSE)}{
  Split a TimeIntervalDataFrame into time intervals (POSIXcti).
  It is exactly the same as splitting a TimeIntervalDataFrame into
  another except that 'f' has not data.

  So a TimeIntervalDataFrame is created according to 'f' and the 
  the above method is called.
}

\seealso{
  \code{\link[base]{split}},
  \code{\link{TimeIntervalDataFrame-class}},
  \code{\link{POSIXcti}},
  \code{\link{POSIXst-class}}, \code{\link{POSIXctp-class}}
}

