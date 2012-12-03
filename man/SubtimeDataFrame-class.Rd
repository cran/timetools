\name{SubtimeDataFrame-class}
\Rdversion{1.1}
\docType{class}
\alias{SubtimeDataFrame-class}
%\alias{SubtimeDataFrame}
\alias{$,SubtimeDataFrame-method}
\alias{$<-,SubtimeDataFrame-method}
\alias{[[,SubtimeDataFrame-method}
\alias{dim,SubtimeDataFrame-method}
\alias{lapply,SubtimeDataFrame-method}
\alias{length,SubtimeDataFrame-method}
\alias{names,SubtimeDataFrame-method}
\alias{names<-,SubtimeDataFrame-method}
\alias{ncol,SubtimeDataFrame-method}
\alias{nrow,SubtimeDataFrame-method}
\alias{show,SubtimeDataFrame-method}
%\alias{timezone,SubtimeDataFrame-method}
%\alias{when,SubtimeDataFrame-method}

\alias{[.SubtimeDataFrame}
\alias{[<-.SubtimeDataFrame}
\alias{[[<-.SubtimeDataFrame}
\alias{as.data.frame.SubtimeDataFrame}
\alias{head.SubtimeDataFrame}
\alias{merge.SubtimeDataFrame}
\alias{plot.SubtimeDataFrame}
\alias{points.SubtimeDataFrame}
\alias{lines.SubtimeDataFrame}
\alias{print.SubtimeDataFrame}
%\alias{rbind.SubtimeDataFrame}
\alias{row.names.SubtimeDataFrame}
\alias{row.names<-.SubtimeDataFrame}
\alias{split.SubtimeDataFrame}
\alias{summary.SubtimeDataFrame}
\alias{tail.SubtimeDataFrame}

\title{Class \code{"SubtimeDataFrame"}}
\description{
  Class to hold subtime data such a day of week, month of year, etc.
}
\details{
  Formally, the class consists of a \code{\link[base]{data.frame}}
  and, for each row, a \code{\link{POSIXst}} (or subtime).
  This class is provided to deal with subtime data. This class
  is compatible
  with \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
  and \code{\link[=TimeIntervalDataFrame-class]{TimeInstantDataFrame}}.

  To list all methods available for SubtimeDataFrame, type in the
  R console :

  \code{methods(class='SubtimeDataFrame') # for S3 methods}

  \code{showMethods(class='SubtimeDataFrame') # for S4 methods}

  To get back to this documentation from the R console :
  
  \code{class?SubtimeDataFrame}

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
    \item \code{new("SubtimeDataFrame", ...)} \dots argument must be replaced
    by named arguments coresponding to slots of a SubtimeDataFrame (see below).
    See also \code{\link[methods]{new}}.
    \item \code{SubtimeDataFrame (when, data=NULL, ...)}
    Arguments of the function correspond to object slots. See \code{\link{SubtimeDataFrame}}
    for details.
    \item \code{as.SubtimeDataFrame (from, representation, cursor=NULL, FUN=mean, ...)}
    Converting object to SubtimeDataFrame.
  }
}
\section{Slots}{
  \describe{
    \item{\code{when}:}{Object of class \code{"POSIXst"}  
       corresponding to the instant of each row of
       the \code{data.frame}.}
    \item{\code{data}:}{Object of class \code{"data.frame"}  data contained
      by the object.}
  }
}
\section{Access to and manipulate data}{
  \describe{
    \item{$}{\code{signature(x = "SubtimeDataFrame")}: ... }
    \item{$<-}{\code{signature(x = "SubtimeDataFrame")}: ... }
    \item{[}{\code{x, i, j, drop=FALSE}: ... }
    \item{[<-}{\code{x, i, j, value}: ... }
    \item{[[}{\code{signature(x = "SubtimeDataFrame")}: ... }
    \item{[[<-}{\code{x, i, j, value}: ... }
    \item{merge}{\code{x, y, by, all=TRUE, tz=UTC, ...}: ... }
    \item{lapply}{\code{signature(X = "SubtimeDataFrame", FUN = 'ANY')}: ... }
    % \item{rbind}{\code{...}: ... }
    \item{split}{\code{x, f, drop=FALSE, ...}: ... }
  }
}
\section{Access to data properties}{
  \describe{
    \item{dim}{\code{signature(x = "SubtimeDataFrame")}: ... }
    \item{length}{\code{signature(x = "SubtimeDataFrame")}: ... }
    \item{names}{\code{signature(x = "SubtimeDataFrame")}: ... }
    \item{names<-}{\code{signature(x = "SubtimeDataFrame")}: ... }
    \item{ncol}{\code{signature(x = "SubtimeDataFrame")}: ... }
    \item{nrow}{\code{signature(x = "SubtimeDataFrame")}: ... }
    \item{row}{\code{x}: ... }
    \item{row<-}{\code{x, value}: ... }
  }
}
\section{Access to time properties}{
  \describe{
    \item{timezone}{\code{signature(object = "SubtimeDataFrame")}: ... }
    \item{when}{\code{signature(x = "SubtimeDataFrame")}: ... }
  }
}
\section{Graphic representation}{
  \describe{
    \item{plot}{\code{x, y=NULL, type=p, lty=1:6, lwd=1, pch=1:25, col=NULL, xlim=NULL, ylim=NULL, log, main, sub, xlab, ylab, ann=par("ann"), axes=TRUE, asp=NA, as.is=TRUE, format=NULL, ...}: plot what it is asked for (see \code{\link[graphics]{plot}}). Returns invisibly params used for the plot. }
    \item{points}{\code{x, y=NULL, type=p, lty=1:6, lwd=1, pch=1:25, col=NULL, as.is=TRUE, format=NULL, ...}: plot what it is asked for (see \code{\link[graphics]{points}}). Returns invisibly params used for the plot. }
    \item{lines}{\code{x, y=NULL, type=p, lty=1:6, lwd=1, pch=1:25, col=NULL, as.is=TRUE, format=NULL, ...}: plot what it is asked for (see \code{\link[graphics]{lines}}). Returns invisibly params used for the plot. }
  }
}
\section{Text representation}{
  \describe{
    \item{show}{\code{signature(object = "SubtimeDataFrame")}: ... }
    \item{print}{\code{x, tz=NULL, ...}: ... }
    \item{summary}{\code{object, ...}: ... }
    \item{head}{\code{x, tz, ...}: ... }
    \item{tail}{\code{x, tz, ...}: ... }
  }
}
\section{Specific methods}{
  Actually, there is no specific methods for this class.
}
\section{Converting SubtimeDataFrame to other classes}{
  \describe{
    \item{as.data.frame}{\code{x, row.names=NULL, optional=FALSE, include.dates=FALSE, ...}: ... }
  }
}
\seealso{
  \code{\link{TimeInstantDataFrame}}, \code{\link{TimeIntervalDataFrame}},
  \code{\link{POSIXst}}
}
\examples{
showClass("SubtimeDataFrame")
}
\keyword{classes}
