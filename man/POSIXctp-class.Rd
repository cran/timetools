\name{POSIXctp}
\docType{class}
\alias{POSIXctp-class}
\alias{POSIXctp}
\alias{as.POSIXctp}
\alias{as.POSIXctp.logical}
\alias{[.POSIXctp}
\alias{[<-.POSIXctp}
\alias{c.POSIXctp}
\alias{split.POSIXctp}
\alias{rep.POSIXctp}
\alias{unique.POSIXctp}
\alias{length,POSIXctp-method}
\alias{unit,POSIXctp-method}
\alias{unit<-,POSIXctp-method}
\alias{duration,POSIXctp-method}
\alias{print.POSIXctp}
\alias{format.POSIXctp}
\alias{show,POSIXctp-method}
\alias{summary.POSIXctp}
\alias{head.POSIXctp}
\alias{tail.POSIXctp}

\alias{match,POSIXctp,POSIXctp-method}
\alias{match,POSIXctp,ANY-method}
\alias{\%in\%,POSIXctp,ANY-method}

\alias{Ops.POSIXctp}
\alias{<.POSIXctp}
\alias{<=.POSIXctp}
\alias{==.POSIXctp}
\alias{!=.POSIXctp}
\alias{>=.POSIXctp}
\alias{>.POSIXctp}

\alias{*,numeric,POSIXctp-method}
\alias{*,POSIXctp,numeric-method}
\alias{+,POSIXctp,POSIXctp-method}
\alias{-,POSIXctp,POSIXctp-method}
\alias{+,POSIXct,POSIXctp-method}
\alias{+,POSIXctp,POSIXct-method}
\alias{-,POSIXct,POSIXctp-method}
% documented in the POSIXcti doc
%\alias{+,POSIXcti,POSIXctp-method}
%\alias{+,POSIXctp,POSIXcti-method}
%\alias{-,POSIXcti,POSIXctp-method}
% documented in the POSIXst doc
%\alias{+,POSIXst,POSIXctp-method}
%\alias{+,POSIXctp,POSIXst-method}
%\alias{-,POSIXst,POSIXctp-method}

\alias{as.numeric,POSIXctp-method}

\title{Class \code{"POSIXctp"}}
\description{
  Class that defines 'periods of time' objects such as 'one month', 'two months', 
  'three hours', 'four minutes', etc.
}
\usage{
POSIXctp(duration, unit)
as.POSIXctp(from, \dots)

  \method{[}{POSIXctp}(x, i, \dots)
  \method{[}{POSIXctp}(x, i) <- value
  \method{c}{POSIXctp}(\dots)
  \method{split}{POSIXctp}(x, f, drop=FALSE, \dots)
  \method{rep}{POSIXctp}(x, \dots)
  \method{unique}{POSIXctp}(x, incomparables=FALSE, \dots)

  \S4method{unit}{POSIXctp}(x, \dots)
  \S4method{unit}{POSIXctp}(object) <- value
  \S4method{duration}{POSIXctp}(x, \dots)

  \S4method{length}{POSIXctp}(x)

  \method{print}{POSIXctp}(x, \dots)
  \method{format}{POSIXctp}(x, \dots)
  \method{summary}{POSIXctp}(object, \dots)
  \method{head}{POSIXctp}(x, \dots)
  \method{tail}{POSIXctp}(x, \dots)
  \S4method{show}{POSIXctp}(object)

  \S4method{match}{POSIXctp,POSIXctp}(x, table, nomatch = NA_integer_, incomparables=NULL)
  \S4method{match}{POSIXctp,ANY}(x, table, nomatch = NA_integer_, incomparables=NULL)
  \S4method{\%in\%}{POSIXctp,ANY}(x, table)

  \method{Ops}{POSIXctp}(e1, e2)

  \S4method{*}{numeric,POSIXctp}(e1, e2)
  \S4method{*}{POSIXctp,numeric}(e1, e2)
  \S4method{+}{POSIXctp,POSIXctp}(e1, e2)
  \S4method{-}{POSIXctp,POSIXctp}(e1, e2)

  \S4method{+}{POSIXct,POSIXctp}(e1, e2)
  \S4method{+}{POSIXctp,POSIXct}(e1, e2)
  \S4method{-}{POSIXct,POSIXctp}(e1, e2)

  \S4method{as.numeric}{POSIXctp}(x, \dots)
}

\arguments{
  \item{duration}{A vector \code{integer} indicating the duration of period (2 for 
	  2 months, 1 for 1 year, etc). If a vector of \code{numeric} is given, it 
  	  will coerced to an \code{integer}. Can be missing, see details below.}
  \item{unit}{A vector of factors defined by \code{\link{POSIXt.units}}() or
	  a vector of \code{character} corresponding to the previous factors.
  	  See details below.}

  \item{from}{Object to convert to a period of time (actually works only for
	  \code{NA}).}

  \item{x}{\code{POSIXctp} object on which the method has to be applied.}
  \item{i}{index (\code{logical} or \code{numeric}) of the POSIXctp objects.}
  \item{value}{New \code{POSIXctp} object.}
  \item{f}{\code{\link[base]{factor}} used to group the \code{POSIXctp} elements.}
  \item{drop}{Argument specific to the split method. See \code{link[base]{split}}
	  documentation.}

  \item{object}{\code{POSIXctp} object on which the method has to be applied.}

  \item{table}{The values (\code{POSIXctp}) to be matched against. See 
  	  \code{\link[base]{match}} for further details.}
  \item{nomatch}{The value to be returned i nthe case when no match is found.
  	  See  \code{\link[base]{match}} for further details.}
  \item{incomparables}{A vector of values that cannot be matched.
  	  See  \code{\link[base]{match}} for further details.}

  \item{e1}{\code{POSIXctp}, \code{numeric} or \code{\link{POSIXct}}. See details.}
  \item{e2}{\code{POSIXctp}, \code{numeric} or \code{\link{POSIXct}}. See details.}

  \item{\dots}{More arguments.}
}

\section{Objects from the Class}{
  Objects of this class are used to represent periods of times such
  as \sQuote{one hour}, \sQuote{two seconds}, \sQuote{three years}, etc.
  Partial periods of time are not allowed (\sQuote{1.5 hours} will be coerced
  to an integer value using \code{\link[base]{as.integer}}).

  POSIXctp (\sQuote{p} stands for\sQuote{period}) has only one unit.
  So \sQuote{one hour and 2 seconds} is not defined.

  One object is actually a  vector of periods of time and so can have a
  length of one for a single period of time or a length of \sQuote{n}
  for \sQuote{n} periods of time.
}
\section{Slots}{
  \describe{
    \item{\code{duration}:}{integer corresponding to the length of the period.}
    \item{\code{unit}:}{factor indicating the time unit
      of the period. See \code{\link{POSIXt.units}} to know available
      units.}
  }
}
\section{Accessing to POSIXctp properties}{
  A POSIXctp has several properties. Because a POSIXctp is a vector of 
  periods of time, the class has a \code{\link[base]{length}} function. Other
  properties are time properties : \code{\link{unit}}
  and \code{\link{duration}} allow to access to the corresponding properties.
  The \code{duration} of a period of time is an integer corresponding of the
  time that the period last, in its time unit. The unit of a period of time 
  is an ordered factor as the one defined by \code{\link{POSIXt.units}}.

  A \code{POSIXctp} can be converted to another time unit base (for instance
  2 hours make 120 minutes). For that purpose the function \code{unit<-} is 
  defined. The conversion will be effective only if the new unit can be exactly
  defined as a multiple of the old one
  (\sQuote{hour} to  \sQuote{second}, ok ; \sQuote{year} to \sQuote{month}, ok
  ; \sQuote{month} to \sQuote{minute} , NOT ok ; etc.
  When conversion can not be done, the result has its unit unchanged.
}
\section{Manipulating POSIXctp}{
  Manipulating \code{POSIXctp} means acting on \code{POSIXctp}s like on classical
  vectors. Methods available for this task allow to extract or replace parts of
  a \code{POSIXctp} (with the usual '[' operator), and to
  concatenate (\code{\link[base]{c}}) or split \code{POSIXctp}
  (\code{\link[base]{split}}). A \code{\link{unique}} and a \code{\link[base]{rep}}
  method are defined to uniquify or repeat elements of a \code{POSIXctp}.

  \code{\link[base]{match}} and \code{\link[base]{\%in\%}} methods have also been
  defined to find \code{POSIXctp} objects among others.
}
\section{Text representation}{
  To represent a \code{POSIXctp} available functions are \code{\link[base]{print}},
  \code{\link[base]{summary}}, \code{\link[utils]{head}},
  \code{\link[utils]{tail}}, \code{\link[methods]{show}} and \code{format}.

  The five first functions work the same way that their generic definition.

  \code{POSIXctp} are formatted by pasting their duration (\code{integer})
  with their unit (and with an \sQuote{s} if relevant).
}
\section{Testing two POSIXctp}{
  To test two POSIXctp the different operators of comparison are used. 
  If the \code{POSIXctp} compared
  have a different length, the shorter is recycled over the longer so the 
  resulting vector (a logical vector) has length equal to the longer object.

  Comparisons are made element by element. For a single comparison, first 
  elements are converted into the same unit. If this is not possible,
  FALSE is returned if the test is \sQuote{==}, TRUE if the test is \sQuote{!=},
  NA otherwise (elements can not be compared) ; else duration of elements are
  compared and the result of this comparison is returned.
}
\section{Mathematical operations on POSIXctp}{
  \code{POSIXctp} can be added (or subtracted) to different type of objects :
  to other \code{POSIXctp}, to \code{POSIXct}, to \code{POSIXcti}  and to
  \code{POSIXst}.  \code{POSIXctp} can also be multiplied by \code{numeric}.

  For all operations, if the two arguments
  have a different length, the shorter is recycled over the longer so the 
  resulting vector (a logical vector) has length equal to the longer object.

  Basic mathematical operation for \code{POSIXctp} are (negative periods of
  time can be defined !!) :
  \itemize{
    \item integer * POSIXctp
    \item POSIXctp * integer
    \item POSIXctp + POSIXctp
    \item POSIXctp - POSIXctp
  }
  When \code{POSIXctp}s do not have the same unit, an attempt is made to convert
  one to the unit of  the other, if it successes the operation is done otherwise
  NA is returned.

  Mathematical operations with \code{POSIXct}, \code{POSIXcti} and \code{POSIXst}
  are actually time lagging. A \code{POSIXct} to which a \code{POSIXctp} is 
  added is lagged by the time periods indicated ; for a  \code{POSIXcti}, start
  and end are lagged by the time periods. 
  For \code{POSIXst}, units of the \code{POSIXst} must be identical : the object
  is then lagged by the time periods (if the result is higher than the maximum
  the result is recycled at the beginning. For instance :
  \code{saturday + 2 days = monday}).

  \itemize{
    \item POSIXct + POSIXctp 
    \item POSIXctp + POSIXct
    \item POSIXct - POSIXctp
    \item .
    \item POSIXcti + POSIXctp 
    \item POSIXctp + POSIXcti
    \item POSIXcti - POSIXctp
    \item .
    \item POSIXst + POSIXctp 
    \item POSIXctp + POSIXst
    \item POSIXst - POSIXctp
  }
}
\section{Changing class}{
  \code{POSIXctp} can be converted to \code{numeric} with the \code{as.numeric}
  method. The duration of the object is returned.
}

\seealso{
  \code{\link[base]{POSIXct}}, \code{\link{POSIXcti}}, 
  \code{\link{POSIXst}}
}
\examples{
showClass("POSIXctp")
}
\keyword{classes}
