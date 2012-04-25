\name{TimeInstantDataFrame-class}
\Rdversion{1.1}
\docType{class}
\alias{TimeInstantDataFrame-class}
%\alias{TimeInstantDataFrame}
%\alias{RegularTimeInstantDataFrame}
\alias{$,TimeInstantDataFrame-method}
\alias{$<-,TimeInstantDataFrame-method}
\alias{[[,TimeInstantDataFrame-method}
\alias{dim,TimeInstantDataFrame-method}
\alias{length,TimeInstantDataFrame-method}
\alias{names,TimeInstantDataFrame-method}
\alias{names<-,TimeInstantDataFrame-method}
\alias{ncol,TimeInstantDataFrame-method}
\alias{nrow,TimeInstantDataFrame-method}
%\alias{regular,TimeInstantDataFrame-method}
\alias{show,TimeInstantDataFrame-method}
\alias{lapply,TimeInstantDataFrame-method}
%\alias{timezone,TimeInstantDataFrame-method}
%\alias{timezone<-,TimeInstantDataFrame-method}
%\alias{when,TimeInstantDataFrame-method}

\alias{[.TimeInstantDataFrame}
\alias{[<-.TimeInstantDataFrame}
\alias{[[<-.TimeInstantDataFrame}
%\alias{as.TimeIntervalDataFrame.TimeInstantDataFrame}
\alias{as.data.frame.TimeInstantDataFrame}
\alias{head.TimeInstantDataFrame}
\alias{merge.TimeInstantDataFrame}
\alias{plot.TimeInstantDataFrame}
\alias{points.TimeInstantDataFrame}
\alias{lines.TimeInstantDataFrame}
\alias{print.TimeInstantDataFrame}
\alias{rbind.TimeInstantDataFrame}
\alias{row.names.TimeInstantDataFrame}
\alias{row.names<-.TimeInstantDataFrame}
\alias{split.TimeInstantDataFrame}
\alias{summary.TimeInstantDataFrame}
\alias{tail.TimeInstantDataFrame}

\title{Class \code{"TimeInstantDataFrame"}}
\description{
  Class to hold time data that ARE not 'punctual'.
}
\details{
  Formally, the class consists of a \code{\link[base]{data.frame}}
  and, for each row, a \code{\link[base]{POSIXct}}.
  This class is provided to deal with punctual time data. Many 
  of such classes are defined in other packages. This one is defined
  mainly to provide a \sQuote{punctual} class compatible
  with \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
  and \code{\link[=SubtimeDataFrame-class]{SubtimeDataFrame}}.

  To list all methods available for TimeInstantDataFrame, type in the
  R console :

  \code{methods(class='TimeInstantDataFrame') # for S3 methods}

  \code{showMethods(class='TimeInstantDataFrame') # for S4 methods}

  To get back to this documentation from the R console :
  
  \code{class?TimeInstantDataFrame}

  The construction of the class allows to manipulate objects as if they
  were data.frame (see \sQuote{Access to data} and \sQuote{Access to data properties}).
  Several functions are also available to access to time properties (see \sQuote{Access
  to time properties}).
 
  Methods are also available to facilitate the representations of instances of that class :
  see \sQuote{graphic representation} and \sQuote{text representation}.

  Finally, some specific methods allow to easily deal with agregation of data over time
  properties (day, hour, week, special or specific time).
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \itemize{
    \item \code{new("TimeInstantDataFrame", ...)} \dots argument must be replaced
    by named arguments coresponding to slots of a TimeInstantDataFrame (see below).
    See also \code{\link[methods]{new}}.
    \item \code{TimeInstantDataFrame (when, timezone='UTC', data=NULL, ...)}
    Arguments of the function correspond to object slots. See \code{\link{TimeInstantDataFrame}}
    for details.
    \item \code{RegularTimeInstantDataFrame (from, to, by, timezone='UTC', data=NULL)}
    Wrapper to construct TimeInstantDataFrame with specific properties (see
    \code{\link{RegularTimeInstantDataFrame}}).
    \item \code{as.TimeInstantDataFrame (from, ...)} Converting object to
    TimeInstantDataFrame.
  }
}
\section{Slots}{
  \describe{
    \item{\code{instant}:}{Object of class \code{"POSIXct"}  
       corresponding to the instant of each row of
       the \code{data.frame}.}
    \item{\code{timezone}:}{Object of class \code{"character"} 
      indicating the timezone of data both for 
      representation and calculation.}
    \item{\code{data}:}{Object of class \code{"data.frame"}  data contained
      by the object.}
  }
}
\section{Access to and manipulate data}{
  \describe{
    \item{$}{\code{signature(x = "TimeInstantDataFrame")}: ... }
    \item{$<-}{\code{signature(x = "TimeInstantDataFrame")}: ... }
    \item{[}{\code{x, i, j, drop=FALSE}: ... }
    \item{[<-}{\code{x, i, j, value}: ... }
    \item{[[}{\code{signature(x = "TimeInstantDataFrame")}: ... }
    \item{[[<-}{\code{x, i, j, value}: ... }
    \item{merge}{\code{x, y, by, all=TRUE, tz=UTC, ...}: ... }
    \item{lapply}{\code{signature(X = "TimeInstantDataFrame", FUN = 'ANY')}: ... }
    \item{rbind}{\code{...}: ... }
    \item{split}{\code{x, f, drop=FALSE, ...}: ... }
  }
}
\section{Access to data properties}{
  \describe{
    \item{dim}{\code{signature(x = "TimeInstantDataFrame")}: ... }
    \item{length}{\code{signature(x = "TimeInstantDataFrame")}: ... }
    \item{names}{\code{signature(x = "TimeInstantDataFrame")}: ... }
    \item{names<-}{\code{signature(x = "TimeInstantDataFrame")}: ... }
    \item{ncol}{\code{signature(x = "TimeInstantDataFrame")}: ... }
    \item{nrow}{\code{signature(x = "TimeInstantDataFrame")}: ... }
    \item{row}{\code{x}: ... }
    \item{row<-}{\code{x, value}: ... }
  }
}
\section{Access to time properties}{
  \describe{
    \item{regular}{\code{signature(x = "TimeInstantDataFrame")}: ... }
    \item{timezone}{\code{signature(object = "TimeInstantDataFrame")}: ... }
    \item{timezone<-}{\code{signature(object = "TimeInstantDataFrame")}: ... }
    \item{when}{\code{signature(x = "TimeInstantDataFrame")}: ... }
  }
}
\section{Graphic representation}{
  \describe{
    \item{plot}{\code{x, y=NULL, type=p, lty=1:6, lwd=1, pch=1:25, col=NULL, xlim=NULL, ylim=NULL, log, main, sub, xlab, ylab, ann=par("ann"), axes=TRUE, asp=NA, ...}: plot what it is asked for (see \code{\link[graphics]{plot}}). Returns invisibly params used for the plot. }
    \item{points}{\code{x, y=NULL, type=p, lty=1:6, lwd=1, pch=1:25, col=NULL, ...}:plot what it is asked for (see \code{\link[graphics]{points}}). Returns invisibly params used for the plot. }
    \item{lines}{\code{x, y=NULL, type=p, lty=1:6, lwd=1, pch=1:25, col=NULL, ...}: plot what it is asked for (see \code{\link[graphics]{lines}}). Returns invisibly params used for the plot. }
  }
}
\section{Text representation}{
  \describe{
    \item{show}{\code{signature(object = "TimeInstantDataFrame")}: ... }
    \item{print}{\code{x, tz=NULL, ...}: ... }
    \item{summary}{\code{object, ...}: ... }
    \item{head}{\code{x, tz, ...}: ... }
    \item{tail}{\code{x, tz, ...}: ... }
  }
}
\section{Specific methods}{
  Actually, there is no specific methods for this class.
}
\section{Converting TimeInstantDataFrame to other classes}{
  \describe{
    \item{as.TimeIntevalDataFrame}{\code{from, period, ...}: ... }
    \item{as.SubtimeDataFrame}{\code{from, representation, FUN=mean, ...}: ... }
    \item{as.data.frame}{\code{x, row.names=NULL, optional=FALSE, include.dates=FALSE, ...}: ... }
  }
}
\seealso{
  \code{\link{TimeIntervalDataFrame}}, \code{\link{SubtimeDataFrame}}
}
\examples{
showClass("TimeInstantDataFrame")
}
\keyword{classes}
