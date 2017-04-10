\name{POSIXcti}
\docType{class}
\alias{POSIXcti-class}
\alias{POSIXcti}
\alias{length,POSIXcti-method}
\alias{show,POSIXcti-method}
\alias{[.POSIXcti}
\alias{[<-.POSIXcti}
\alias{print.POSIXcti}
\alias{summary.POSIXcti}
\alias{head.POSIXcti}
\alias{tail.POSIXcti}
\alias{c.POSIXcti}
\alias{unique.POSIXcti}
\alias{match,POSIXcti,POSIXcti-method}
\alias{\%in\%,POSIXcti,POSIXcti-method}
\alias{split.POSIXcti}
\alias{rep.POSIXcti}
\alias{as.POSIXcti}
\alias{as.POSIXcti.logical}

\alias{duration,POSIXcti-method}
\alias{format.POSIXcti}
% TODO faire un Rd pour start/end
\alias{start.POSIXcti}
\alias{end.POSIXcti}
\alias{\%included\%.POSIXcti}
\alias{\%intersect\%.POSIXcti}

\alias{Ops.POSIXcti}
\alias{<.POSIXcti}
\alias{<=.POSIXcti}
\alias{==.POSIXcti}
\alias{!=.POSIXcti}
\alias{>=.POSIXcti}
\alias{>.POSIXcti}

\alias{+,POSIXcti,POSIXctp-method}
\alias{+,POSIXctp,POSIXcti-method}
\alias{-,POSIXcti,POSIXctp-method}

\title{Class \code{"POSIXcti"}}
\description{
  S4 class that defines 'time interval' objects.
}
\usage{
POSIXcti(start, end, timezone='UTC', \dots)
as.POSIXcti(from, \dots)

  \method{[}{POSIXcti}(x, i, \dots)
  \method{[}{POSIXcti}(x, i) <- value
  \method{c}{POSIXcti}(\dots)
  \method{split}{POSIXcti}(x, f, drop=FALSE, \dots)
  \method{rep}{POSIXcti}(x, \dots)
  \method{unique}{POSIXcti}(x, incomparables=FALSE, \dots)
  \method{\%intersect\%}{POSIXcti}(i1, i2)

  \method{start}{POSIXcti}(x, \dots)
  \method{end}{POSIXcti}(x, \dots)
  \S4method{duration}{POSIXcti}(x, \dots)

  \S4method{length}{POSIXcti}(x)

  \method{print}{POSIXcti}(x, \dots)
  \method{format}{POSIXcti}(x, format = "\%Y-\%m-\%d \%H:\%M:\%S", \dots)
  \method{summary}{POSIXcti}(object, \dots)
  \method{head}{POSIXcti}(x, \dots)
  \method{tail}{POSIXcti}(x, \dots)
  \S4method{show}{POSIXcti}(object)

  \S4method{match}{POSIXcti,POSIXcti}(x, table, nomatch = NA_integer_, incomparables=NULL)
  \S4method{\%in\%}{POSIXcti,POSIXcti}(x, table)

  \method{Ops}{POSIXcti}(e1, e2)

  \method{\%included\%}{POSIXcti}(i1, i2)

  \S4method{+}{POSIXcti,POSIXctp}(e1, e2)
  \S4method{+}{POSIXctp,POSIXcti}(e1, e2)
  \S4method{-}{POSIXcti,POSIXctp}(e1, e2)
}
\arguments{
  \item{start}{\code{\link{POSIXct}} object indicating the beginning of the time
	  interval.}
  \item{end}{\code{\link{POSIXct}} object indicating the end of the time interval.}
  \item{timezone}{\code{character} indicating the timezone in which the time
	  interval is set. See \code{\link[base]{timezone}}.}

  \item{from}{Object to convert to a time interval (actually works only for
	  \code{NA}).}

  \item{x}{\code{POSIXcti} object on which the method has to be applied.}
  \item{i}{index (\code{logical} or \code{numeric}) of the time interval objects.}
  \item{value}{New \code{POSIXcti} object.}
  \item{f}{\code{\link[base]{factor}} used to group the \code{POSIXcti} elements.}
  \item{drop}{Argument specific to the split method. See \code{link[base]{split}}
	  documentation.}

  \item{format}{\code{character} indicating the format to use to represent
	  the time interval. See sectopn \sQuote{Text representation} below
	  for further details.}

  \item{object}{\code{POSIXcti} object on which the method has to be applied.}

  \item{table}{The values (\code{POSIXcti} vector) to be matched against. See 
  	  \code{\link[base]{match}} for further details.}
  \item{nomatch}{The value to be returned i nthe case when no match is found.
  	  See  \code{\link[base]{match}} for further details.}
  \item{incomparables}{A vector of values that cannot be matched.
  	  See  \code{\link[base]{match}} for further details.}

  \item{e1}{For (<, <=, !=, ==, >=, >) \code{POSIXcti} to compare ; otherwise
  	  a \code{POSIXcti} to shift by a time period (\code{\link{POSIXctp}}) or
  	  a \code{\link{POSIXctp}} by which a \code{POSIXcti} has to be shift.}
  \item{e2}{For (<, <=, !=, ==, >=, >) \code{POSIXcti} to compare ; otherwise
  	  a \code{POSIXcti} to shift by a time period (\code{\link{POSIXctp}}) or
  	  a \code{\link{POSIXctp}} by which a \code{POSIXcti} has to be shift.}

  \item{i1}{\code{POSIXcti} to test/intersect. See \code{\link{\%included\%}} and 
	  \code{\link{\%intersect\%}} for further details.}
  \item{i2}{\code{POSIXcti} to test/intersect. See \code{\link{\%included\%}} and 
	  \code{\link{\%intersect\%}} for further details.}

  \item{\dots}{More arguments.}
}

\section{Objects from the Class}{
  Objects of this class represent time intervals. One object is actually a
  vector of time intervals and so can have a length of one for a single
  time interval or a length of \sQuote{n} for \sQuote{n} time intervals.
}
\section{Slots}{
  \describe{
    \item{\code{start}:}{Object of class \code{"POSIXct"} corresponding to the
	    beginning of the interval.}
    \item{\code{duration}:}{integer indicating in seconds the duration of
	    intervals.}
  }
}
\section{Accessing to POSIXcti properties}{
  A POSIXcti has several properties. Because a POSIXcti is a vector of 
  time intervals, the class has a \code{\link[base]{length}} function. Other
  properties are time properties : \code{\link{start}}, \code{\link{end}}
  and \code{\link{duration}} allow to access to the corresponding properties.
  The \code{duration} of a time interval is the number of seconds for which
  the interval last.
}
\section{Manipulating POSIXcti}{
  Manipulating \code{POSIXcti} means acting on \code{POSIXcti}s like on classical
  vectors. Methods available for this task allow to extract or replace parts of
  a \code{POSIXcti} (with the usual '[' operator), and to
  concatenate (\code{\link[base]{c}}) or split \code{POSIXcti}
  (\code{\link[base]{split}}). A \code{\link{unique}} and a \code{\link[base]{rep}}
  method are defined to uniquify or repeat elements of a \code{POSIXcti}.

  \code{\link[base]{match}} and \code{\link[base]{\%in\%}} methods have also been
  defined to find \code{POSIXctp} objects among others.

  Last, the \code{\link{\%intersect\%}} method allow to intersect two
  \code{POSIXcti}.
}
\section{Text representation}{
  To represent a \code{POSIXcti} available functions are \code{\link[base]{print}},
  \code{\link[base]{summary}}, \code{\link[utils]{head}},
  \code{\link[utils]{tail}}, \code{\link[methods]{show}} and \code{format}.

  The five first functions work the same way that their generic definition.

  \code{POSIXcti} are formatted by pasting the character strings 
  representing both start and end of each intervals. 
  Start and end's format can be specified with the \code{format}
  argument according to the basic \link[base]{format.POSIXct} function.
}
\section{Testing two POSIXcti}{
  To test two POSIXcti the differents operators of comparison are used. One 
  more is defined : \code{\link{\%included\%}}. If the \code{POSIXcti} compared
  have a different length, the shorter is recycled over the longer so the 
  resulting vector (a logical vector) has length equal to the longer object.

  Comparisons are made element by element. The result for a single comparison
  is given there :
  \describe{
    \item{e1 < e2}{TRUE if \code{end(e1) <= start(e2)}.}
    \item{e1 <= e2}{TRUE if \code{start(e1) <= start(e2) & end(e1) <= end(e2)}.}
    \item{e1 != e2}{TRUE if \code{start(e1) != start(e2) | duration(e1) != duration(e2)}.}
    \item{e1 == e2}{TRUE if \code{start(e1) == start(e2) & duration(e1) == duration(e2)}.}
    \item{e1 >= e2}{TRUE if \code{start(e1) >= start(e2) & end(e1) >= end(e2)}.}
    \item{e1 > e2}{TRUE if \code{end(e1) >= start(e2)}.}
    \item{i1 \%included\% i2}{TRUE if \code{start(i1) >= start(i2) & end(i1) <= end(i2)}.}
  }
}
\section{Mathematical operations on POSIXcti}{
  \sQuote{Mathematical} operations are actually \sQuote{time lagging} for 
  \code{POSIXcti}. A 
  \code{\link[=POSIXctp]{time period}} is added/removed to both start and end
  of intervals. The available operations are :
  \itemize{
    \item POSIXcti + POSIXctp
    \item POSIXctp + POISXcti
    \item POSIXcti - POSIXctp
  }
}
\seealso{
  \code{\link[base]{POSIXct}}, \code{\link{TimeIntervalDataFrame}},
  \code{\link{POSIXst}}, \code{\link{POSIXctp}}
}
\examples{
  # time interval : january of year 2013
  jan <- POSIXcti('2013-01-01', '2013-02-01')
  jan

  # the complete year
  y2013 <- POSIXcti('2013-01-01', '2014-01-01')
  y2013

  # is jan in 2013 ?
  jan \%included\% y2013

  # intersection
  jan \%intersect\% y2013

}
\keyword{classes}
