\name{TimeIntervalDataFrame}
\docType{class}
\alias{TimeIntervalDataFrame-class}
\alias{TimeIntervalDataFrame}
\alias{RegularTimeIntervalDataFrame}

\alias{$,TimeIntervalDataFrame-method}
\alias{$<-,TimeIntervalDataFrame-method}
\alias{[.TimeIntervalDataFrame}
\alias{[<-.TimeIntervalDataFrame}
\alias{[[,TimeIntervalDataFrame-method}
\alias{[[<-.TimeIntervalDataFrame}

\alias{rbind.TimeIntervalDataFrame}
\alias{merge.TimeIntervalDataFrame}
\alias{split.TimeIntervalDataFrame}
%\alias{split,TimeIntervalDataFrame,POSIXcti-method}
%\alias{split,TimeIntervalDataFrame,POSIXctp-method}
%\alias{split,TimeIntervalDataFrame,TimeIntervalDataFrame-method}
\alias{lapply,TimeIntervalDataFrame-method}

\alias{tapply,TimeIntervalDataFrame-method}
\alias{changeSupport,TimeIntervalDataFrame-method}

\alias{continuous,TimeIntervalDataFrame-method}
\alias{continuous<-,TimeIntervalDataFrame-method}
\alias{homogeneous,TimeIntervalDataFrame-method}
\alias{period,TimeIntervalDataFrame-method}
\alias{overlapping,TimeIntervalDataFrame-method}
\alias{regular,TimeIntervalDataFrame-method}
\alias{timezone,TimeIntervalDataFrame-method}
\alias{timezone<-,TimeIntervalDataFrame-method}
\alias{start.TimeIntervalDataFrame}
\alias{end.TimeIntervalDataFrame}
\alias{when,TimeIntervalDataFrame-method}
\alias{interval,TimeIntervalDataFrame-method}

\alias{dim,TimeIntervalDataFrame-method}
\alias{length,TimeIntervalDataFrame-method}
\alias{names,TimeIntervalDataFrame-method}
\alias{names<-,TimeIntervalDataFrame-method}
\alias{ncol,TimeIntervalDataFrame-method}
\alias{nrow,TimeIntervalDataFrame-method}
\alias{row.names.TimeIntervalDataFrame}
\alias{row.names<-.TimeIntervalDataFrame}

\alias{print.TimeIntervalDataFrame}
\alias{summary.TimeIntervalDataFrame}
\alias{head.TimeIntervalDataFrame}
\alias{tail.TimeIntervalDataFrame}
\alias{show,TimeIntervalDataFrame-method}

\alias{plot.TimeIntervalDataFrame}
\alias{points.TimeIntervalDataFrame}
\alias{lines.TimeIntervalDataFrame}
\alias{barplot.TimeIntervalDataFrame}

\alias{as.TimeIntervalDataFrame}
\alias{as.TimeIntervalDataFrame.TimeInstantDataFrame}

%\alias{as.TimeInstantDataFrame.TimeIntervalDataFrame}
%\alias{as.SubtimeDataFrame.TimeIntervalDataFrame}
%\alias{as.data.frame.TimeIntervalDataFrame}

\title{Class \code{"TimeIntervalDataFrame"}}
\description{
  Class to hold time data that are NOT 'punctual'.
}
\usage{
  TimeIntervalDataFrame(start, end = NULL,
    timezone = "UTC", data = NULL, period = NULL, \dots)

  as.TimeIntervalDataFrame(from, \dots)
  \method{as.TimeIntervalDataFrame}{TimeInstantDataFrame}(from, period, \dots)

  RegularTimeIntervalDataFrame(from, to, by, period, timezone = "UTC", data = NULL)

  \S4method{$}{TimeIntervalDataFrame}(x, name)
  \S4method{$}{TimeIntervalDataFrame}(x, name) <- value
  \method{[}{TimeIntervalDataFrame}(x, i, j, drop=FALSE)
  \method{[}{TimeIntervalDataFrame}(x, i, j) <- value
  \S4method{[[}{TimeIntervalDataFrame}(x ,i, j, \dots)
  \method{[[}{TimeIntervalDataFrame}(x, i, j) <- value

  \method{rbind}{TimeIntervalDataFrame}(\dots)
  \method{merge}{TimeIntervalDataFrame}(x, y, by, all=TRUE, tz='UTC', \dots)
  \method{split}{TimeIntervalDataFrame}(x, f, drop=FALSE, \dots)
  \S4method{lapply}{TimeIntervalDataFrame}(X, FUN, \dots)

  \S4method{tapply}{TimeIntervalDataFrame}(X, INDEX, FUN, ...,
       min.coverage=1, weights.arg=NULL, merge.X=TRUE, split.X=FALSE,
       keep.INDEX=TRUE, simplify=TRUE)
  \S4method{changeSupport}{TimeIntervalDataFrame}(from, to,
       min.coverage, FUN=NULL, weights.arg=NULL,
       split.from=FALSE, merge.from=TRUE, ...)

  \S4method{continuous}{TimeIntervalDataFrame}(x, \dots)
  \S4method{continuous}{TimeIntervalDataFrame}(x) <- value
  \S4method{homogeneous}{TimeIntervalDataFrame}(x, \dots)
  \S4method{period}{TimeIntervalDataFrame}(x, \dots)
  \S4method{overlapping}{TimeIntervalDataFrame}(x, \dots)
  \S4method{regular}{TimeIntervalDataFrame}(x, \dots)
  \S4method{timezone}{TimeIntervalDataFrame}(object)
  \S4method{timezone}{TimeIntervalDataFrame}(object) <- value
  \method{start}{TimeIntervalDataFrame}(x, \dots)
  \method{end}{TimeIntervalDataFrame}(x, \dots)
  \S4method{when}{TimeIntervalDataFrame}(x, \dots)
  \S4method{interval}{TimeIntervalDataFrame}(x, \dots)

  \S4method{dim}{TimeIntervalDataFrame}(x)
  \S4method{length}{TimeIntervalDataFrame}(x)
  \S4method{names}{TimeIntervalDataFrame}(x)
  \S4method{names}{TimeIntervalDataFrame}(x) <- value
  \S4method{ncol}{TimeIntervalDataFrame}(x)
  \S4method{nrow}{TimeIntervalDataFrame}(x)
  \method{row.names}{TimeIntervalDataFrame}(x)
  \method{row.names}{TimeIntervalDataFrame}(x) <- value

  \method{print}{TimeIntervalDataFrame}(x, tz=NULL, \dots)
  \method{summary}{TimeIntervalDataFrame}(object, \dots)
  \method{head}{TimeIntervalDataFrame}(x, tz, \dots)
  \method{tail}{TimeIntervalDataFrame}(x, tz, \dots)
  \S4method{show}{TimeIntervalDataFrame}(object)

  \method{plot}{TimeIntervalDataFrame}(x, y=NULL, cursor=NULL,
    type='p', lty=1:6, lwd=1, pch=1:25, col=NULL,
    xlim=NULL, ylim=NULL, log='', main='', sub='', xlab='', ylab='',
    ann=par('ann'), axes=TRUE, asp=NA, \dots)
  \method{points}{TimeIntervalDataFrame}(x, y=NULL, cursor=NULL, type='p',
    lty=1:6, lwd=1, pch=1:25, col=NULL, \dots)
  \method{lines}{TimeIntervalDataFrame}(x, y=NULL, cursor=NULL, type='l',
    lty=1:6, lwd=1, pch=1:25, col=NULL, \dots)
  \method{barplot}{TimeIntervalDataFrame}(height, format='', \dots)
}
\arguments{
  \item{start}{POSIXct or character representing a time with a valid format
    (see \code{\link[base:as.POSIXct]{POSIXct}}). It gives the begining
    of each interval.}
  \item{end}{POSIXct or character representing a time with a valid format
    (see \code{\link[base:as.POSIXct]{POSIXct}}). It gives the end
    of each interval. If NULL, see \sQuote{Details}.}
  \item{timezone}{character representing a valid timezone (see
    \code{\link[base]{timezone}}).}
  \item{data}{a data.frame with as much rows as the length
    of \sQuote{start} and end, or with one row less than the
    length of \sQuote{start} if \sQuote{end} is \code{NULL}.
    Can be \code{NULL} (hence the data.frame has zero column
    and as much rows as needed).}

  \item{period}{
    \describe{
      \item{TimeIntervalDataFrame}{
        if not NULL, a \code{\link{POSIXctp}} or a
    	character that can be converted to a
    	\code{\link{POSIXctp}} (see argument \sQuote{unit} of
      	POSIXctp function). See Details to know how to use this
      	argument.}
      \item{as.TimeIntervalDataFrame}{
        \code{\link{POSIXctp}} object indicating
      	the period to add to \sQuote{when} slot of \code{from} to
      	determine the end of the new period (the \sQuote{when} is
      	used for the start of period)}
      \item{RegularTimeIntervalDataFrame}{
        a \code{\link{POSIXctp}} object indicating the period of each
    	interval. If missing, it is given the value of \code{by}.}
  }}

  \item{from}{\describe{
    \item{as.TimeIntervalDataFrame}{object to convert to a TimeIntervalDataFrame}
    \item{RegularTimeIntervalDataFrame}{
      POSIXct or character representing a time with a valid format (see
      \code{\link[base:as.POSIXct]{POSIXct}}). It represents the start of the
      object.}
    \item{changeSupport}{see \code{\link{changeSupport}}}
  }}
  \item{to}{\describe{
    \item{RegularTimeIntervalDataFrame}{
      POSIXct or character representing a time with a valid format (see
      \code{\link[base:as.POSIXct]{POSIXct}}). It represents the end of the
      object. If missing, its value is deduced from \sQuote{from},
      \sQuote{by} and \sQuote{data}.}
    \item{changeSupport}{see \code{\link{changeSupport}}}
  }}
  \item{by}{\describe{
    \item{RegularTimeIntervalDataFrame}{a \code{\link{POSIXctp}} object
      indicating the increment to use between the start of each interval.}
    \item{merge}{specifications of the columns used for merging.}
  }}

  \item{x}{TimeIntervalDataFrame object (to modify, to extract or to test)}
  \item{i}{indices specifying elements to extract or replace. (See
	  \code{\link[base]{Extract}})}
  \item{j}{indices specifying elements to extract or replace. (See
	  \code{\link[base]{Extract}})}
  \item{name}{A litteral character string or a name. (See
	  \code{\link[base]{Extract}})}
  \item{drop}{Used for compatibility with data.frame methods.}
  \item{value}{New value for the object designated (data.frame, names, row.names,
	  etc.).}

  \item{y}{TimeIntervalDataFrame to merge with x}
  \item{all}{logical; see \code{\link[base]{merge}}}
  \item{tz}{character representing a valid timezone (see
	  \code{\link[base]{timezone}}).}

  \item{f}{a 'factor' in the sense that ‘as.factor(f)’ defines the
            grouping, or a list of such factors in which case their
            interaction is used for the grouping. See \code{\link{split}}.}

  \item{X}{
    \describe{
      \item{lapply}{a TimeIntervalDataFrame on which the FUN must be applied.}
      \item{tapply}{see \code{\link{tapply}} for details}
  }}
  \item{FUN}{
    \describe{
      \item{lapply}{function to apply over each columns of X.}
      \item{tapply,changeSupport}{see \code{\link{tapply}} for details}
  }}
  \item{INDEX,min.coverage,weights.arg, merge.X,split.X,keep.INDEX,simplify,split.from,merge.from}{
    see \code{\link{tapply}} and/or \code{\link{changeSupport}}
    for details.}

  \item{object}{TimeIntervalDataFrame object (to modify, to extract or to test)}

  \item{type}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{lty}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{lwd}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{pch}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{col}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{xlim}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{ylim}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{log}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{main}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{sub}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{xlab}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{ylab}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{ann}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{axes}{plotting argument, see \code{\link[graphics]{plot.default}}}
  \item{asp}{plotting argument, see \code{\link[graphics]{plot.default}}}

  \item{cursor}{To convert TimeIntervalDataFrame to a TimeInstantDataFrame
	  before plotting (see \code{\link{TimeInstantDataFrame}},
	  it indicates where the TimeInstant
	  must be taken. If \code{0}, start of each intervals is taken as
	  instant ; if \code{1} end of each intervals is taken as instant.
	  Any other value will determine a weigthed instant between start and
	  end (actually, value higher than 1 or lower than 0 will give instant
	  outside this range).}

  \item{height}{plotting argument, see \code{\link[graphics]{barplot}}}
  \item{format}{plotting argument, see \code{\link[graphics]{barplot}}}

  \item{\dots}{More arguments.}
}

\section{Objects from the Class}{
  Formally, the class consists of a \code{\link[base]{data.frame}}
  and, for each row, two \code{\link[base]{POSIXct}} that can
  be summarize as time interval with the \code{\link{POSIXcti}} class.
  This allows to manipulate at once time data without any 
  restriction on time representation : data can occur at different time, 
  data can be discontinuous, data can be heterogeneous (not lasting
  for a unique period), data can overlay each other, etc.
  There are several methods to test/deal/ensure that
  these properties are respected or not, see below.

  The construction of the class allows to manipulate objects as if they
  were data.frame (see \sQuote{Access to data} and \sQuote{Access to
  data properties}). Several functions are also available to access to
  time properties (see \sQuote{Access to time properties}).
 
  Methods are also available to facilitate the representations of
  instances of that class : see \sQuote{graphic representation} and
  \sQuote{text representation}.

  Finally, some specific methods allow to easily deal with agregation of
  data over time properties (day, hour, week, special or specific time
  intervals).

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
\section{TimeIntervalDataFrame constructors}{
  Objects can be created by calls of the form
  \itemize{
    \item \code{new("TimeIntervalDataFrame", ...)} \dots argument must be
    replaced by named arguments coresponding to slots of a
    TimeIntervalDataFrame (see below). See also \code{\link[methods]{new}}.

    \item{\code{TimeIntervalDataFrame (start, end=NULL, timezone='UTC', data=NULL, ...)}
    Arguments of the function correspond to object slots.
    If both \code{start} and \code{end} are given, they must
    have the same length. They are used to define the
    intervals of the object. If \code{data} is also given, it
    must have a number of rows identical to the length of
    \code{start} and \code{end}.

    If only \code{start} is given, a continuous (see
    \code{\link{continuous}}) TimeIntervalDataFrame is built.
    The first element of \code{start} is the start of the
    first interval, the second element is the end of the
    first interval and the start of the second interval. The
    last element of \code{start} is only the end of the last
    interval. This is why \code{data}, if given, must be one
    row shorter than \code{start}.

    If period is given it must be a \code{\link{POSIXctp}}
    object (or a valid character) and \sQuote{start} and
    \sQuote{end} must have length equal to 1. In that case, a
    TimeIntervalDataFrame will be created with start date
    equal to start \sQuote{floored} by the unit of
    \sQuote{period}, end date \sQuote{ceiled} by the unit of
    \sQuote{period} and with enough intervals of
    \sQuote{period} length to fit. If \sQuote{data} given, it
    must have a number of rows equal to the number of
    intervals calculated.}

    \item \code{RegularTimeIntervalDataFrame (from, to, by, period, timezone='UTC', data=NULL)}
    Wrapper to construct TimeIntervalDataFrame with specific properties (see
    details of each argument).

    \item \code{as.TimeIntervalDataFrame (from, ...)}
    Converting object to TimeIntervalDataFrame.
  }
}

\section{Accessing to and manipulating data}{
  The TimeIntervalDataFrame class is defined to works like the data.frame class
  with the difference that a time interval (\code{\link{POSIXcti}}) is
  attached to each rows of the data.frame. Thus to access and manipulate
  data of a TimeIntervalDataFrame the following methods are defined : 
  '$', '$<-', '[', '[<-', '[[', '[[<-'. See \code{\link[base]{Extract}} for 
  details.

  Other methods have been defined to allow some operations on
  TimeIntervalDataFrame :
  \itemize{
    \item rbind and merge to join two (or more)
  TimeIntervalDataFrame (see \code{\link[base]{rbind}} and
  \code{\link[base]{merge}}),
    \item a TimeIntervalDataFrame can be splitted exactly the same way that
    a data.frame can (see \code{\link[base]{split}} in the base
    package) and some more possibilities have been defined (see \code{\link{split}}
    in the timetools package),
    \item a function can be applied over each column of a TimeIntervalDataFrame
    via the lapply function provided that the function return one value (in
    this case the resulting value has only one row),
    or as much values as the number of rows of the object (in this case the
    TimeIntervalDataFrame given in argument in returned with the new values
    calculated),
    \item 'tapply' can split a TimeIntervalDataFrame and then apply a function
    over each group (see \code{\link{tapply}}),
    \item 'changeSupport' act like the 'tapply' function but with
    a different default behaviour (see \code{\link{changeSupport}}).
  }

  Because a TimeIntervalDataFrame works more or less like a data.frame, 
  the following methods work on a TimeIntervalDataFrame :
  \code{\link[base]{dim}},
  \code{\link[base]{length}},
  \code{\link[base]{names}},
  \code{\link[base]{names<-}},
  \code{\link[base]{ncol}},
  \code{\link[base]{nrow}},
  \code{\link[base]{row.names}},
  \code{\link[base]{row.names<-}}.
}

\section{Access/modify to time properties}{
  A TimeIntervamDataFrame can be tested for a few time properties :
  \describe{
    \item{continuous}{see \code{\link{continuous}} ;}
    \item{homogeneous}{see \code{\link{homogeneous}} ;}
    \item{period}{see \code{\link{period}} ;}
    \item{overlapping}{see \code{\link{overlapping}} ;}
    \item{regular}{TRUE if all time intervals are equally spaced ;}
    \item{timezone}{gives or sets the timezone of the TimeIntervalDataFrame ;}
    \item{start}{returns a POSIXct object with the start time of each 
	    intervals ;}
    \item{end}{returns a POSIXct object with the end time of each intervals ;}
    \item{when}{returns a POSIXcti, i.e. the intervals of the object ;}
    \item{interval}{returns a POSIXcti, i.e. the intervals of the object.}
  }
}
\section{Graphic representation}{
  To plot a \code{TimeIntervalDataFrame} available functions are
  \code{\link[graphics]{plot}}, \code{\link[graphics]{lines}},
  \code{\link[graphics]{points}} and \code{\link[graphics]{barplot}}.

  These functions works more or less like their generic definition.
}
\section{Text representation}{
  To represent a \code{TimeIntervalDataFrame} available functions are
  \code{\link[base:print.data.frame]{print}},
  \code{\link[base:data.frame]{summary}}, \code{\link[utils]{head}},
  \code{\link[utils]{tail}} and \code{\link[methods]{show}}.
}
\seealso{
  \code{\link{TimeInstantDataFrame}}, \code{\link{SubtimeDataFrame}},
  \code{\link{POSIXcti}}, \code{\link{POSIXctp}}
}
\examples{
showClass("TimeIntervalDataFrame")
}
\keyword{classes}

