\name{POSIXst}
\docType{class}
\alias{POSIXst-class}

\alias{POSIXst}
\alias{POSIXst.default}
\alias{POSIXst.integer}
\alias{POSIXst.numeric}
\alias{POSIXst.POSIXct}
\alias{POSIXst.POSIXlt}
\alias{POSIXst.TimeInstantDataFrame}
\alias{POSIXst.TimeIntervalDataFrame}

\alias{day}
\alias{day,ANY-method}
\alias{day-methods}
\alias{hour}
\alias{hour,ANY-method}
\alias{hour-methods}
\alias{minute}
\alias{minute,ANY-method}
\alias{minute-methods}
\alias{month}
\alias{month,ANY-method}
\alias{month-methods}
\alias{second}
\alias{second,ANY-method}
\alias{second-methods}
\alias{year}
\alias{year,ANY-method}
\alias{year-methods}

\alias{[.POSIXst}
\alias{[<-.POSIXst}
\alias{c.POSIXst}
\alias{split.POSIXst}
\alias{rep.POSIXst}
\alias{seq.POSIXst}
\alias{unique.POSIXst}
\alias{duplicated.POSIXst}
\alias{unit.POSIXst}
\alias{of.POSIXst}
\alias{timezone.POSIXst}
\alias{length,POSIXst-method}

\alias{print.POSIXst}
\alias{format.POSIXst}
\alias{summary.POSIXst}
\alias{head.POSIXst}
\alias{tail.POSIXst}
\alias{show,POSIXst-method}
\alias{match,POSIXst,POSIXst-method}
\alias{match,POSIXst,ANY-method}
\alias{\%in\%,POSIXst,ANY-method}

\alias{Ops.POSIXst}
\alias{<.POSIXst}
\alias{<=.POSIXst}
\alias{==.POSIXst}
\alias{!=.POSIXst}
\alias{>=.POSIXst}
\alias{>.POSIXst}

\alias{-,POSIXst,POSIXst-method}
\alias{+,POSIXst,POSIXctp-method}
\alias{+,POSIXctp,POSIXst-method}
\alias{-,POSIXst,POSIXctp-method}

\alias{as.numeric,POSIXst-method}

\title{Class \code{"POSIXst"}}
\description{
  Class to define POSIXst object such as hours of day, seconds of year, etc.
}
\usage{

POSIXst(x, unit, of = NULL, tz = "UTC", ...)
  \method{POSIXst}{default} (x, unit, of = NULL, tz = "UTC", ...)
  \method{POSIXst}{integer} (x, unit, of = NULL, tz = "UTC", ...)
  \method{POSIXst}{numeric} (x, unit, of = NULL, tz = "UTC", ...)
  \method{POSIXst}{POSIXct} (x, unit, of = NULL, tz = attributes(x)$tzone, ...)
  \method{POSIXst}{POSIXlt} (x, unit, of = NULL, tz = attributes(x)$tzone, ...)
  \method{POSIXst}{TimeInstantDataFrame} (x, unit, of = NULL, tz = timezone(x), ...)
  \method{POSIXst}{TimeIntervalDataFrame} (x, unit, of = NULL, tz = timezone(x), ..., cursor = NULL)

year(x, ...)
month(x, ...)
day(x, of, ...)
hour(x, of, ...)
minute(x, of, ...)
second(x, of, ...)

  \method{[}{POSIXst}(x, i)
  \method{[}{POSIXst}(x, i) <- value
  \method{c}{POSIXst}(\dots)
  \method{split}{POSIXst}(x, f, drop=FALSE, \dots)
  \method{rep}{POSIXst}(x, \dots)
  \method{seq}{POSIXst}(from, to, \dots)
  \method{unique}{POSIXst}(x, incomparables=FALSE, \dots)
  \method{duplicated}{POSIXst}(x, incomparables=FALSE, \dots)

  \method{unit}{POSIXst}(x, \dots)
  \method{of}{POSIXst}(x, \dots)
  \method{timezone}{POSIXst}(object)

  \S4method{length}{POSIXst}(x)

  \method{print}{POSIXst}(x, \dots)
  \method{format}{POSIXst}(x, format, \dots)
  \method{summary}{POSIXst}(object, \dots)
  \method{head}{POSIXst}(x, \dots)
  \method{tail}{POSIXst}(x, \dots)
  \S4method{show}{POSIXst}(object)

  \S4method{match}{POSIXst,POSIXst}(x, table, nomatch = NA_integer_, incomparables=NULL)
  \S4method{match}{POSIXst,ANY}(x, table, nomatch = NA_integer_, incomparables=NULL)
  \S4method{\%in\%}{POSIXst,ANY}(x, table)

  \method{Ops}{POSIXst}(e1, e2)

  \S4method{+}{POSIXst,POSIXctp}(e1, e2)
  \S4method{+}{POSIXctp,POSIXst}(e1, e2)
  \S4method{-}{POSIXst,POSIXctp}(e1, e2)

  \S4method{-}{POSIXst,POSIXst}(e1, e2)

  \S4method{as.numeric}{POSIXst}(x, \dots)

}
\arguments{

  \item{x}{object to convert into POSIXst or \code{POSIXst} object on which the
	  method has to be applied.}
  \item{unit}{indicates the subtime part to extract ('year', 'month', 'day',
	  'hour', 'minute', 'second')}
  \item{of}{used to specify the main period from wich the is to extract
	  ('year', 'month', day', 'hour', 'minute'). Not used for
	  \sQuote{unit in c('year', 'month')}.}
  \item{tz}{if needed, specifies th timezone of POSIXst}

  \item{cursor}{for TimeIntervalDataFrame, if not NULL, the object is first
	  coerced to a TimeInstantDataFrame using the
	  \code{\link{as.TimeInstantDataFrame}} method.}

  \item{i}{index (\code{logical} or \code{numeric}) of the POSIXst objects.}
  \item{value}{New \code{POSIXst} object.}
  \item{f}{\code{\link[base]{factor}} used to group the \code{POSIXst} elements.}
  \item{drop}{Argument specific to the split method. See \code{link[base]{split}}
	  documentation.}

  \item{object}{\code{POSIXst} object on which the method has to be applied.}

  \item{format}{Character string to precise the desired format. See section
  \sQuote{Text representation} below for details.}

  \item{table}{The values (\code{POSIXst}) to be matched against. See 
  	  \code{\link[base]{match}} for further details.}
  \item{nomatch}{The value to be returned i nthe case when no match is found.
  	  See  \code{\link[base]{match}} for further details.}
  \item{incomparables}{A vector of values that cannot be matched.
  	  See  \code{\link[base]{match}} for further details.}

  \item{e1}{\code{POSIXst} or \code{\link{POSIXctp}}. See details.}
  \item{e2}{\code{POSIXst} or \code{\link{POSIXctp}}. See details.}

  \item{from, to}{starting and end values to sequence, see \code{\link[base]{seq}}}

  \item{\dots}{More arguments.}
}
\section{Objects from the Class}{
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

  To define POSIXst objects see POSIXst section below.

  One object is actually a  vector of subtimes and so can have a length of one
  for a single subtime or a length of \sQuote{n} for \sQuote{n} subtimes.

  Last, a POSIXst object has a \sQuote{timezone} slots. This is defined
  for compatibility reason with POSIXct object and also with
  \code{\link{TimeInstantDataFrame}},  \code{\link{TimeIntervalDataFrame}} and 
  \code{\link{SubtimeDataFrame}}.
}
\section{Slots}{
  \describe{
    \item{\code{subtime}:}{Object of class \code{"integer"} corresponding to the
	    actual value of each subtime.}
    \item{\code{unit}:}{factor representing a time unit. It represents the
	    subdvision of time (in 'second of year' it corresponds to 'second').
	    See \code{\link{POSIXt.units}}.}
    \item{\code{of}:}{factor representing a time unit. It represents the main
	    time unit (in 'second of year' it corresponds to 'year'). See
	    \code{\link{POSIXt.units}}.}
    \item{\code{timezone}:}{Object of class \code{"character"} indicating the
    timezone of the POSIXst object.}
  }
}

\section{POSIXst}{
  \code{POSIXst} objects can be created from various other class objects.
  For this purpose the \code{POSIXst} method has been defined. 

  First, if \sQuote{x} is missing, an empty factor with the appropriated levels
  (according to \sQuote{unit and of}) is returned.

  A \code{POSIXst} can be created from an integer or a numeric. If so,
  \sQuote{unit}
  and \sQuote{of} msute be supplied (see arguments section above). The \sQuote{tz}
  argument can be supplied (numeric will be converted to an integer). In both 
  cases,  values of the integer/numeric must be in the right range 
  (see \code{\link[base]{DateTimeClasses}}.


  A \code{POSIXst} can be created from a \code{\link{POSIXct}} or
  \code{\link{POSIXlt}} object. In this case, the subtime (POSIxst) is extracted
  in the units indicated by \sQuote{unit} and \sQuote{of} arguments. The 
  \sQuote{tz} argument indicates the timezone of the resulting object (it doesn't
  do any conversion on the POSIX{l,c}t objects).

  Finally, \code{POSIXst} can be extracted from \code{\link{TimeInstantDataFrame}}
  and  \code{\link{TimeIntervalDataFrame}}. For the first type of object, the 
  method is applied to the time instants (which are \code{POSIXct}). For the 
  latter, because a time interval can contains several POSIXst of one kind (for
  instance a day contains all 'hours of day'), the result of this method  
  TimeIntervalDataFrame is a list of POSIXst. Each element of the list contains
  the POSIXsts asked for corresponding to each row of the TimeIntervalDataFrame
  object. If \sQuote{cursor} is supplied, the TimeIntervalDataFrame is first
  converted to a TimeInstantDataFrame (see \code{\link{as.TimeInstantDataFrame}}
  for details).
}

\section{year(...), month(...), day(...), hour(...), minute(...) and second(...)}{
  \code{year}, \code{month}, \code{day}, \code{hour}, \code{minute} and
  \code{second} are methods defined to extract the adequat information from a
  time object. These functions are wrappers to \code{POSIXst}.
  
  Each of these methods call POSIXst replacing the \code{unit} argument with its
  own name :
  \code{minute(x, of='day')} will call
  \code{POSIXst(x, unit='minute', of='day')}
}
\section{Accessing to POSIXst properties}{
  A POSIXst has several properties. Because a POSIXst is a vector of 
  subtimes, the class has a \code{\link[base]{length}} function. Other
  properties are time properties : \code{\link{unit}}, \code{\link{of}}
  and \code{\link{timezone}} allow to access to the corresponding properties.
  The \sQuote{unit} and \sQuote{of} of a subtime 
  is an ordered factor as the one defined by \code{\link{POSIXt.units}}.

  For more informations on timezone, see the
  \link[base:timezone]{page of the manual}.
}
\section{Manipulating POSIXst}{
  Manipulating \code{POSIXst} means acting on \code{POSIXst}s like on classical
  vectors. Methods available for this task allow to extract or replace parts of
  a \code{POSIXst} (with the usual '[' operator), and to
  concatenate (\code{\link[base]{c}}) or split \code{POSIXst}
  (\code{\link[base]{split}}). A \code{\link{unique}}, a  \code{\link{duplicated}}, a \code{\link[base]{rep}}
  and a \code{\link[base]{seq}}
  methods are defined to uniquify, repeat or sequence elements of a \code{POSIXst}.

  \code{\link[base]{match}} and \code{\link[base]{\%in\%}} methods have also been
  defined to find \code{POSIXst} objects among others.
}
\section{Text representation}{
  To represent a \code{POSIXst} available functions are \code{\link[base]{print}},
  \code{\link[base]{summary}}, \code{\link[utils]{head}},
  \code{\link[utils]{tail}}, \code{\link[methods]{show}} and \code{format}.

  The five first functions work the same way that their generic definition.

  'POSIXst' are formatted according to the \code{format} argument which must
  respect the following rules.
  \itemize{
	  \item \%v value
	  \item \%s subtime unit (slot \sQuote{unit})
	  \item \%m main unit (slot \sQuote{of})
	  \item \%a Abbreviated weekday name in the current locale.
	  \item \%A Full weekday name in the current locale.
	  \item \%b Abbreviated month name in the current locale.
	  \item \%B Full month name in the current locale.
	  \item \%r timezone
	  \item \%p place of subtime (ie the string part of 1st, 2nd, 10th, etc.)
  }
}
\section{Testing two POSIXst}{
  To test two POSIXst the differents operators of comparison are used. 
  If the \code{POSIXst} compared
  have a different length, the shorter is recycled over the longer so the 
  resulting vector (a logical vector) has length equal to the longer object.

  Comparisons are made element by element. Two \code{POSIXst} with a different
  \sQuote{unit} or a different \sQuote{of} are differents (TRUE if test is
  \sQuote{!=}, FALSE if \sQuote{==} NA otherwise). If they have identical
  \sQuote{unit} and \sQuote{of} the comparison is made over subtime slots.
}
\section{Mathematical operations on POSIXst}{
  \code{POSIXst} can be added and substracted to \code{\link{POSIXctp}}.
  \code{POSIXst} can also be substracted (and only substracted) to \code{POSIXst}.

  For all operations, if the two arguments
  have a different length, the shorter is recycled over the longer so the 
  resulting vector (a logical vector) has length equal to the longer object.

  Mathematical operations with \code{POSIXctp} are actually time lagging. Units
  of the \code{POSIXctp} must be identical to the \code{POSIXst}'s : the object
  is then lagged by the time periods (if the result is higher than the maximum
  the result is recycled at the begining. For instance :
  \code{saturday + 2 days = monday}).

  \itemize{
    \item POSIXst + POSIXctp 
    \item POSIXctp + POISXst
    \item POSIXst - POSIXctp
  }

  Substracting a \code{POSIXst} to another result in a \code{\link{POSIXctp}}.
  For instance \code{wednesday - monday = 2 days}.
}
\section{Changing class}{
  \code{POSIXst} can be converted to \code{numeric} with the \code{as.numeric}
  method. The subtime slot of the object is returned.
}
\seealso{
  \code{\link[base]{POSIXct}}, \code{\link{POSIXcti}}, \code{\link{POSIXctp}},
  \code{\link{TimeIntervalDataFrame}},
}
\examples{
showClass("POSIXst")
}
\keyword{classes}
