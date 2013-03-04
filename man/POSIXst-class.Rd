\name{POSIXst-class}
\docType{class}
\alias{POSIXst-class}
% \alias{POSIXst}

% \alias{when,TimeIntervalDataFrame-method}

% \alias{unit.POSIXst}
% \alias{of.POSIXst}
% \alias{timezone.POSIXst}

\alias{show,POSIXst-method}
\alias{length,POSIXst-method}

\alias{print.POSIXst}
\alias{summary.POSIXst}
\alias{head.POSIXst}
\alias{tail.POSIXst}
\alias{[.POSIXst}
\alias{[<-.POSIXst}
\alias{c.POSIXst}
\alias{unique.POSIXst}
% \alias{split.POSIXst}
% \alias{split,ANY,POSIXst-method}
\alias{as.numeric,POSIXst-method}
\alias{match,POSIXst,POSIXst-method}
\alias{match,POSIXst,ANY-method}
\alias{\%in\%,POSIXst,ANY-method}

\title{Class \code{"POSIXst"}}
\description{
  Class to define POSIXst object such as hours of day, seconds of year, etc.
}
\details{
  Objects of this class are used to represent subtimes. A subtime (or a
  \sQuote{POSIXst}, \sQuote{st} stand for Sub and Time)  is a subdivision  of
  time :
  \itemize{
  \item second of a minute ;
  \item second of an hour ;
  \item second of a day ;
  \item second of a week ;
  \item second of a month ;
  \item second of a year ;
  \item minute of an hour ;
  \item minute of a day ;
  \item \dots
  \item minute of a year ;
  \item \dots
  \item month of year ;
  \item year AD (after death).
  }

  A POSIXst is a kind of time object composed of 2 units and
  a positionnal integer. 
  The main unit can be accessed via the \sQuote{of} function ;
  the sub unit can be accessed via the \sQuote{unit} function.
  The positionnal integer correspond to the value of the subtime object.
  Consequently, a subtime \code{st} is the \code{value}th \code{unit(st)}
  of \code{of(st)}.

  The range of valid values for each kind of POSIXst is defined accordingly
  to the \code{\link[base]{DateTimeClasses}} definitions. For instance 
  valid values for seconds of hour are 0 to 61, valid values for 
  day of week are 0 to 6, etc.

  To define POSIXst objects see the \code{\link{POSIXst}} constructor.
  It allows to define a POSIXst object from scratch or to use a 
  \code{\link[base]{POSIXct}} object.

  Moreover, a POSIXst object has a \sQuote{timezone} slots. This is defined
  for compatibility reason with POSIXct object and also with
  \code{\link{TimeInstantDataFrame}},  \code{\link{TimeIntervalDataFrame}} and 
  \code{\link{SubtimeDataFrame}}.

  To list all methods available for POSIXst, type in the
  R console :

  \code{methods(class='POSIXst') # for S3 methods}

  \code{showMethods(class='POSIXst') # for S4 methods}

  To get back to this documentation from the R console :
  
  \code{class?POSIXst}

}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \itemize{
    \item \code{new("POSIXst", ...)} \dots argument must be replaced
    by named arguments coresponding to slots of a POSIXst (see below).
    See also \code{\link[methods]{new}}.
    \item \code{POSIXst (x, unit, of, ...)} Create a POSIXst from various type
    of object. See \code{\link{POSIXst}}.
  }
}
\section{Slots}{
  \describe{
    \item{\code{subtime}:}{Object of class \code{"integer"} 
       corresponding to the actual value of each subtime.}
    \item{\code{unit}:}{factor representing a time unit. It 
	    represents the subdvision of time (in 'second of year'
	    it corresponds to 'second'). See \code{\link{POSIXt.units}}.}
    \item{\code{of}:}{factor representing a time unit. It 
	    represents the main time unit (in 'second of year'
	    it corresponds to 'year'). See \code{\link{POSIXt.units}}.}
    \item{\code{timezone}:}{Object of class \code{"character"}
      indicating the timezone of the POSIXst object.}
  }
}
\section{Manipulate POSIXctp}{
  \describe{
    \item{[}{\code{x, i, ...}: ... }
    \item{[<-}{\code{x, i, value}: ... }
    \item{c}{\code{...}: ... }
    \item{split}{\code{x, f, drop=FALSE, ...}: ... }
    \item{Ops}{\code{e1, e2}: comparison between two subtimes }
    \item{as.numeric}{\code{x, ...}: ... }
    \item{match}{\code{x, table, nomatch = NA_integer_, incomparables=NULL}: ... }
    \item{\%in\%}{\code{x, table}: ... }
  }
}
\section{Access to POSIXst properties}{
  \describe{
	  \item{unit}{\code{x, ...}: Get the 'unit' of the object. }
	  \item{of}{\code{x, ...}: .Get the 'of' of the object. }
	  \item{timezone}{\code{x, ...}: Get the 'timezone' of the object. }
	  \item{length}{\code{signature(object = "POSIXst")}: ... }
  }
}
\section{Text representation}{
  \describe{
	  \item{format}{\code{x, format, ...}: see \code{\link{format.POSIXst}} }
	  \item{show}{\code{signature(object = "POSIXst")}: ... }
	  \item{print}{\code{x, ...}: ... }
	  \item{summary}{\code{object, ...}: ... }
	  \item{head}{\code{x, ...}: ... }
	  \item{tail}{\code{x, ...}: ... }
  }
}
\seealso{
  \code{\link[base]{POSIXct}}, \code{\link{POSIXcti}}, \code{\link{POSIXctp}},
  \code{\link{TimeIntervalDataFrame}},
}
\examples{
showClass("POSIXst")
}
\keyword{classes}
