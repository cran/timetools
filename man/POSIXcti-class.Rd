\name{POSIXcti-class}
\Rdversion{1.1}
\docType{class}
\alias{POSIXcti-class}
\alias{POSIXcti}
\alias{length,POSIXcti-method}
\alias{show,POSIXcti-method}
%\alias{duration,POSIXcti-method}

\alias{start.POSIXcti}
\alias{end.POSIXcti}
\alias{[.POSIXcti}
\alias{[<-.POSIXcti}
%\alias{format.POSIXcti}
\alias{print.POSIXcti}
\alias{summary.POSIXcti}
\alias{head.POSIXcti}
\alias{tail.POSIXcti}
\alias{c.POSIXcti}
\alias{unique.POSIXcti}
%\alias{split.POSIXcti}
%\alias{%included%.POSIXcti}
%\alias{%intersect%.POSIXcti}
%\alias{as.POSIXcti.logical}
\alias{match,POSIXcti,POSIXcti-method}
\alias{\%in\%,POSIXcti,POSIXcti-method}

\title{Class \code{"POSIXcti"}}
\description{
  Class that defines 'time interval' objects. A time interval
  is defined by two dates : its beginning and its ending.
}
\details{
  Objects of this class are used to represent time intervals. Start
  and end of an interval or \sQuote{POSIXcti} (the \sQuote{i} stands for interval)
  are \code{\link[base]{POSIXct}} objects.

  This class facilitate the use of \code{\link{TimeIntervalDataFrame}} objects.

  To list all methods available for POSIXcti, type in the
  R console :

  \code{methods(class='POSIXcti') # for S3 methods}

  \code{showMethods(class='POSIXcti') # for S4 methods}

  To get back to this documentation from the R console :
  
  \code{class?POSIXcti}

}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \itemize{
    \item \code{new("POSIXcti", ...)} \dots argument must be replaced
    by named arguments coresponding to slots of a POSIXcti (see below).
    See also \code{\link[methods]{new}}.
    \item \code{POSIXcti (start, end, timezone='UTC')}
    Arguments of the function correspond to object slots. See \code{\link{POSIXcti}}
    for details.
    \item \code{as.POSIXcti (from, ...)} Converting object to
    POSIXcti.
  }
}
\section{Slots}{
  \describe{
    \item{\code{start}:}{Object of class \code{"POSIXct"}  
       corresponding to the beginning of the interval.}
    \item{\code{duration}:}{integer indicating in seconds 
       the duration of intervals.}
  }
}
\section{Manipulate POSIXcti}{
  \describe{
    \item{[}{\code{x, i, ...}: ... }
    \item{[<-}{\code{x, i, value}: ... }
    \item{c}{\code{...}: ... }
    \item{split}{\code{x, f, drop=FALSE, ...}: ... }
  }
}
\section{Access to POSIXcti properties}{
  \describe{
    \item{length}{\code{signature(x = "POSIXctp")}: ... }
    \item{duration}{\code{signature(x = "POSIXctp")}: ... }
    \item{start}{\code{x, ...}: ... }
    \item{end}{\code{x, ...}: ... }
  }
}
\section{Math and comparison functions}{
  \describe{
    \item{Ops}{\code{e1, e2}: comparison between two time intervals }
    \item{'+'}{\code{signature(e1 = "POSIXcti", e2="POSIXctp")}: ... }
    \item{'+'}{\code{signature(e1 = "POSIXctp", e2="POSIXcti")}: ... }
    \item{'-'}{\code{signature(e1 = "POSIXcti", e2="POSIXctp")}: ... }
    \item{match}{\code{x, table, nomatch = NA_integer_, incomparables=NULL}: ... }
    \item{\%in\%}{\code{x, table}: ... }
  }
}
\section{Text representation}{
  \describe{
    \item{print}{\code{x, ...}: ... }
    \item{format}{\code{x, format = "\%Y-\%m-\%d \%H:\%M:\%S", ...}: ... }
    \item{show}{\code{signature(object='POSIXcti')}: ... }
    \item{summary}{\code{object, ...}: ... }
    \item{head}{\code{x, ...}: ... }
    \item{tail}{\code{x, ...}: ... }
  }
}
\seealso{
  \code{\link[base]{POSIXct}}, \code{\link{POSIXcti}}, \code{\link{TimeIntervalDataFrame}},
  \code{\link{POSIXst}}
}
\examples{
showClass("POSIXcti")
}
\keyword{classes}
