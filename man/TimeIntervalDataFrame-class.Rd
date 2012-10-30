\name{TimeIntervalDataFrame-class}
\Rdversion{1.1}
\docType{class}
\alias{TimeIntervalDataFrame-class}
%\alias{TimeIntervalDataFrame}
%\alias{RegularTimeIntervalDataFrame}
\alias{$,TimeIntervalDataFrame-method}
\alias{$<-,TimeIntervalDataFrame-method}
\alias{[[,TimeIntervalDataFrame-method}
%\alias{changeSupport,TimeIntervalDataFrame,TimeIntervalDataFrame,numeric,ANY,ANY,ANY,ANY-method}
%\alias{changeSupport,TimeIntervalDataFrame,character,numeric,ANY,ANY,missing,missing-method}
%\alias{changeSupport,TimeIntervalDataFrame,POSIXctp,numeric,ANY,ANY,missing,missing-method}
%\alias{continuous,TimeIntervalDataFrame-method}
%\alias{continuous<-,TimeIntervalDataFrame-method}
\alias{dim,TimeIntervalDataFrame-method}
%\alias{homogeneous,TimeIntervalDataFrame-method}
%\alias{interval,TimeIntervalDataFrame-method}
\alias{names,TimeIntervalDataFrame-method}
\alias{names<-,TimeIntervalDataFrame-method}
\alias{ncol,TimeIntervalDataFrame-method}
\alias{nrow,TimeIntervalDataFrame-method}
\alias{length,TimeIntervalDataFrame-method}
\alias{lapply,TimeIntervalDataFrame-method}
%\alias{overlapping,TimeIntervalDataFrame-method}
%\alias{period,TimeIntervalDataFrame-method}
%\alias{regular,TimeIntervalDataFrame-method}
\alias{show,TimeIntervalDataFrame-method}
%\alias{timezone,TimeIntervalDataFrame-method}
%\alias{timezone<-,TimeIntervalDataFrame-method}
%\alias{when,TimeIntervalDataFrame-method}

\alias{[.TimeIntervalDataFrame}
\alias{[<-.TimeIntervalDataFrame}
\alias{[[<-.TimeIntervalDataFrame}
%\alias{as.TimeInstantDataFrame.TimeIntervalDataFrame}
\alias{as.data.frame.TimeIntervalDataFrame}
\alias{end.TimeIntervalDataFrame}
\alias{head.TimeIntervalDataFrame}
\alias{merge.TimeIntervalDataFrame}
\alias{plot.TimeIntervalDataFrame}
\alias{points.TimeIntervalDataFrame}
\alias{lines.TimeIntervalDataFrame}
\alias{print.TimeIntervalDataFrame}
\alias{rbind.TimeIntervalDataFrame}
\alias{row.names.TimeIntervalDataFrame}
\alias{row.names<-.TimeIntervalDataFrame}
\alias{split.TimeIntervalDataFrame}
\alias{start.TimeIntervalDataFrame}
\alias{summary.TimeIntervalDataFrame}
\alias{tail.TimeIntervalDataFrame}

\title{Class \code{"TimeIntervalDataFrame"}}
\description{
  Class to hold time data that are NOT 'punctual'.
}
\details{
  Formally, the class consists of a \code{\link[base]{data.frame}}
  and, for each row, two \code{\link[base]{POSIXct}} that can
  be summarize as time interval with the \code{\link{POSIXcti}} class.
  This allows to manipulate at once time data without any 
  restriction on time representation : data can occur at different time, 
  data can be discontinuous, data can be heterogeneous (not lasting
  for a unique period), data can overlay each other, etc.
  There are several methods to test/deal/ensure that
  these properties are respected or not, see below.

  To list all methods available for TimeIntervalDataFrame, type in the
  R console :

  \code{methods(class='TimeIntervalDataFrame') # for S3 methods}

  \code{showMethods(class='TimeIntervalDataFrame') # for S4 methods}

  To get back to this documentation from the R console :
  
  \code{class?TimeIntervalDataFrame}

  The construction of the class allows to manipulate objects as if they
  were data.frame (see \sQuote{Access to data} and \sQuote{Access to data properties}).
  Several functions are also available to access to time properties (see \sQuote{Access
  to time properties}).
 
  Methods are also available to facilitate the representations of instances of that class :
  see \sQuote{graphic representation} and \sQuote{text representation}.

  Finally, some specific methods allow to easily deal with agregation of data over time
  properties (day, hour, week, special or specific time intervals).
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \itemize{
    \item \code{new("TimeIntervalDataFrame", ...)} \dots argument must be replaced
    by named arguments coresponding to slots of a TimeIntervalDataFrame (see below).
    See also \code{\link[methods]{new}}.
    \item \code{TimeIntervalDataFrame (start, end=NULL, timezone='UTC', data=NULL, ...)}
    Arguments of the function correspond to object slots. See \code{\link{TimeIntervalDataFrame}}
    for details.
    \item \code{RegularTimeIntervalDataFrame (from, to, by, period, timezone='UTC', data=NULL)}
    Wrapper to construct TimeIntervalDataFrame with specific properties (see
    \code{\link{RegularTimeIntervalDataFrame}}).
    \item \code{as.TimeIntervalDataFrame (from, ...)} Converting object to TimeIntervalDataFrame.
  }
}
\section{Slots}{
  \describe{
    \item{\code{start}:}{Object of class \code{"POSIXct"} 
       corresponding to the start of each row of
       the \code{data.frame}.}
    \item{\code{end}:}{Object of class \code{"POSIXct"}
       corresponding to the end of each row of
       the \code{data.frame}.}
    \item{\code{timezone}:}{Object of class \code{"character"}
      indicating the timezone of data both for 
      representation and calculation.}
    \item{\code{data}:}{Object of class \code{"data.frame"} data contained
      by the object.}
  }
}
\section{Access to and manipulate data}{
  \describe{
    \item{$}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{$<-}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{[}{\code{x, i, j, drop=FALSE}: ... }
    \item{[<-}{\code{x, i, j, value}: ... }
    \item{[[}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{[[<-}{\code{x, i, j, value}: ... }
    \item{rbind}{\code{...}: ... }
    \item{merge}{\code{x, y, by, all=TRUE, tz=UTC, ...}: ... }
    \item{lapply}{\code{signature(X = "TimeIntervalDataFrame", FUN = 'ANY')}: ... }
    \item{split}{\code{x, f, drop=FALSE, ...}: ... }
  }
}
\section{Access to data properties}{
  \describe{
    \item{dim}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{names}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{names<-}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{ncol}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{nrow}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{length}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{row}{\code{x}: ... }
    \item{row<-}{\code{x, value}: ... }
  }
}
\section{Access to time properties}{
  \describe{
    \item{continuous}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{continuous<-}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{overlapping}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{homogeneous}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{interval}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{timezone}{\code{signature(object = "TimeIntervalDataFrame")}: ... }
    \item{timezone<-}{\code{signature(object = "TimeIntervalDataFrame")}: ... }
    \item{when}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{period}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{regular}{\code{signature(x = "TimeIntervalDataFrame")}: ... }
    \item{start}{\code{x, ...}: ... }
    \item{end}{\code{x, ...}: ... }
    \item{subtime}{\code{x, representation, cursor, ...}: ... }
  }
}
\section{Graphic representation}{
  \describe{
    \item{plot}{\code{x, y=NULL, cursor=NULL, type=p, lty=1:6, lwd=1, pch=1:25, col=NULL, xlim=NULL, ylim=NULL, log, main, sub, xlab, ylab, ann=par("ann"), axes=TRUE, asp=NA, ...}: plot what it is asked for (see \code{\link[graphics]{plot}}). Returns invisibly params used for the plot. }
    \item{points}{\code{x, y=NULL, cursor=NULL, type=p, lty=1:6, lwd=1, pch=1:25, col=NULL, ...}: plot what it is asked for (see \code{\link[graphics]{points}}). Returns invisibly params used for the plot. }
    \item{lines}{\code{x, y=NULL, cursor=NULL, type=p, lty=1:6, lwd=1, pch=1:25, col=NULL, ...}: plot what it is asked for (see \code{\link[graphics]{lines}}). Returns invisibly params used for the plot. }
  }
}
\section{Text representation}{
  \describe{
    \item{show}{\code{signature(object = "TimeIntervalDataFrame")}: ... }
    \item{print}{\code{x, tz=NULL, ...}: ... }
    \item{summary}{\code{object, ...}: ... }
    \item{head}{\code{x, tz, ...}: ... }
    \item{tail}{\code{x, tz, ...}: ... }
  }
}
\section{Specific methods}{
  \describe{
    \item{changeSupport}{\code{signature(from="TimeIntervalDataFrame", to="TimeIntervalDataFrame", min.coverage="numeric", FUN="ANY", weights.arg="ANY", split.from="ANY", merge.from="ANY")}: ... }
    \item{changeSupport}{\code{signature(from="TimeIntervalDataFrame", to="character", min.coverage="numeric", FUN="ANY", weights.arg="ANY", split.from="missing", merge.from="missing")}: ... }
    \item{changeSupport}{\code{signature(from="TimeIntervalDataFrame", to="period", min.coverage="numeric", FUN="ANY", weights.arg="ANY", split.from="missing", merge.from="missing")}: ... }
  }
}
\section{Converting TimeIntervalDataFrame to other classes}{
  \describe{
    \item{as.TimeInstantDataFrame}{\code{from, cursor=NULL, ...}: ... }
    \item{as.SubtimeDataFrame}{\code{from, representation, cursor=NULL, FUN=mean, ...}: ... }
    \item{as.data.frame}{\code{x, row.names=NULL, optional=FALSE, include.dates=FALSE, ...}: ... }
  }
}
\seealso{
  \code{\link{TimeInstantDataFrame}}, \code{\link{SubtimeDataFrame}},
  \code{\link{POSIXcti}}, \code{\link{POSIXctp}}
}
\examples{
showClass("TimeIntervalDataFrame")
}
\keyword{classes}
