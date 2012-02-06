\name{POSIXctp-class}
\Rdversion{1.1}
\docType{class}
\alias{POSIXctp-class}
\alias{POSIXctp}
\alias{length,POSIXctp-method}
\alias{show,POSIXctp-method}
%\alias{unit,POSIXctp-method}
%\alias{unit<-,POSIXctp-method}
%\alias{duration,POSIXctp-method}

\alias{[.POSIXctp}
\alias{[<-.POSIXctp}
\alias{format.POSIXctp}
\alias{print.POSIXctp}
\alias{c.POSIXctp}
%\alias{as.POSIXctp.logical}

\title{Class \code{"POSIXctp"}}
\description{
  Class that defines 'time period' objects such as 'one month', 'two months', 
  'three hours', 'four minutes', etc.
}
\details{
  Objects of this class are used to represent time periods such
  as \sQuote{one hour}, \sQuote{two seconds}, \sQuote{three years}, etc.
  Partial time period are not allowed (\sQuote{1.5 hours} will be coerced
  to an integer value using \code{\link[base]{as.integer}}).

  POSIXctp (\sQuote{p} stands for\sQuote{period}) has only one unit.
  So \sQuote{one hour and 2 seconds} is not defined.

  To list all methods available for POSIXctp, type in the
  R console :

  \code{methods(class='POSIXctp') # for S3 methods}

  \code{showMethods(class='POSIXctp') # for S4 methods}

  To get back to this documentation from the R console :
  
  \code{class?POSIXctp}

}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \itemize{
    \item \code{new("POSIXctp", ...)} \dots argument must be replaced
    by named arguments coresponding to slots of a POSIXctp (see below).
    See also \code{\link[methods]{new}}.
    \item \code{POSIXctp (duration, unit)}
    Arguments of the function correspond to object slots. See \code{\link{POSIXctp}}
    for details.
    \item \code{as.POSIXctp (from, ...)} Converting object to
    POSIXctp.
  }
}
\section{Slots}{
  \describe{
    \item{\code{duration}:}{integer corresponding to the length of the period.}
    \item{\code{unit}:}{factor indicating the time unit
      of the period. See \code{\link{POSIXt.units}} to know available
      units.}
  }
}
\section{Manipulate POSIXctp}{
  \describe{
    \item{[}{\code{x, i, ...}: ... }
    \item{[<-}{\code{x, i, value}: ... }
    \item{c}{\code{...}: ... }
  }
}
\section{Access to POSIXctp properties}{
  \describe{
    \item{length}{\code{signature(x = "POSIXctp")}: ... }
    \item{duration}{\code{signature(x = "POSIXctp")}: ... }
    \item{unit}{\code{signature(x = "POSIXctp")}: ... }
    \item{unit<-}{\code{signature(object = "POSIXctp")}: ... }
  }
}
\section{Math and comparison functions}{
  \describe{
    \item{Ops}{\code{e1, e2}: comparison between two time periods }
    \item{*}{\code{signature(e1 = "POSIXctp", e2="POSIXctp")}: ... }
    \item{*}{\code{signature(e1 = "numeric", e2="POSIXctp")}: ... }
    \item{+}{\code{signature(e1 = "POSIXct", e2="POSIXctp")}: ... }
    \item{+}{\code{signature(e1 = "POSIXcti", e2="POSIXctp")}: ... }
    \item{+}{\code{signature(e1 = "POSIXctp", e2="POSIXct")}: ... }
    \item{+}{\code{signature(e1 = "POSIXctp", e2="POSIXcti")}: ... }
    \item{+}{\code{signature(e1 = "POSIXctp", e2="POSIXctp")}: ... }
    \item{-}{\code{signature(e1 = "POSIXct", e2="POSIXctp")}: ... }
    \item{-}{\code{signature(e1 = "POSIXcti", e2="POSIXctp")}: ... }
  }
}
\section{Text representation}{
  \describe{
    \item{print}{\code{x, ...}: ... }
    \item{format}{\code{x, ...}: ... }
    \item{show}{\code{signature(object='POSIXctp')}: ... }
  }
}
\seealso{
  \code{\link[base]{POSIXct}}, \code{\link{POSIXcti}}
}
\examples{
showClass("POSIXctp")
}
\keyword{classes}
