\name{TimeInstantDataFrame}
\docType{class}
\alias{TimeInstantDataFrame-class}
\alias{TimeInstantDataFrame}
\alias{RegularTimeInstantDataFrame}

\alias{$,TimeInstantDataFrame-method}
\alias{$<-,TimeInstantDataFrame-method}
\alias{[.TimeInstantDataFrame}
\alias{[<-.TimeInstantDataFrame}
\alias{[[,TimeInstantDataFrame-method}
\alias{[[<-.TimeInstantDataFrame}

\alias{Ops,TimeInstantDataFrame,numeric-method}
\alias{Ops,numeric,TimeInstantDataFrame-method}

\alias{rbind.TimeInstantDataFrame}
\alias{merge.TimeInstantDataFrame}
\alias{split.TimeInstantDataFrame}
\alias{lapply,TimeInstantDataFrame-method}

\alias{regular,TimeInstantDataFrame-method}
\alias{timezone,TimeInstantDataFrame-method}
\alias{timezone<-,TimeInstantDataFrame-method}
\alias{when,TimeInstantDataFrame-method}

\alias{dim,TimeInstantDataFrame-method}
\alias{length,TimeInstantDataFrame-method}
\alias{names,TimeInstantDataFrame-method}
\alias{names<-,TimeInstantDataFrame-method}
\alias{ncol,TimeInstantDataFrame-method}
\alias{nrow,TimeInstantDataFrame-method}
\alias{row.names.TimeInstantDataFrame}
\alias{row.names<-.TimeInstantDataFrame}

\alias{print.TimeInstantDataFrame}
\alias{summary.TimeInstantDataFrame}
\alias{head.TimeInstantDataFrame}
\alias{tail.TimeInstantDataFrame}
\alias{show,TimeInstantDataFrame-method}

\alias{plot.TimeInstantDataFrame}
\alias{points.TimeInstantDataFrame}
\alias{lines.TimeInstantDataFrame}
\alias{barplot.TimeInstantDataFrame}

\alias{as.TimeInstantDataFrame}
\alias{as.TimeInstantDataFrame.TimeIntervalDataFrame}

%\alias{as.TimeIntervalDataFrame.TimeInstantDataFrame}
%\alias{as.SubtimeDataFrame.TimeInstantDataFrame}
%\alias{as.data.frame.TimeInstantDataFrame}

\title{Class \code{"TimeInstantDataFrame"}}
\description{
  Class to hold time data that ARE 'instantaneous'.
}
\usage{
  TimeInstantDataFrame(when, timezone = "UTC", data = NULL, \dots)

  as.TimeInstantDataFrame(from, \dots)
  \method{as.TimeInstantDataFrame}{TimeIntervalDataFrame}(from, cursor = NULL, \dots)

  RegularTimeInstantDataFrame(from, to, by, timezone = "UTC", data = NULL)

  \S4method{$}{TimeInstantDataFrame}(x, name)
  \S4method{$}{TimeInstantDataFrame}(x, name) <- value
  \method{[}{TimeInstantDataFrame}(x, i, j, drop=FALSE)
  \method{[}{TimeInstantDataFrame}(x, i, j) <- value
  \S4method{[[}{TimeInstantDataFrame}(x ,i, j, \dots)
  \method{[[}{TimeInstantDataFrame}(x, i, j) <- value

  \method{rbind}{TimeInstantDataFrame}(\dots)
  \method{merge}{TimeInstantDataFrame}(x, y, by, all=TRUE, tz='UTC', \dots)
  \method{split}{TimeInstantDataFrame}(x, f, drop=FALSE, \dots)
  \S4method{lapply}{TimeInstantDataFrame}(X, FUN, \dots)

  \S4method{regular}{TimeInstantDataFrame}(x, \dots)
  \S4method{timezone}{TimeInstantDataFrame}(object)
  \S4method{timezone}{TimeInstantDataFrame}(object) <- value
  \S4method{when}{TimeInstantDataFrame}(x, \dots)

  \S4method{dim}{TimeInstantDataFrame}(x)
  \S4method{length}{TimeInstantDataFrame}(x)
  \S4method{names}{TimeInstantDataFrame}(x)
  \S4method{names}{TimeInstantDataFrame}(x) <- value
  \S4method{ncol}{TimeInstantDataFrame}(x)
  \S4method{nrow}{TimeInstantDataFrame}(x)
  \method{row.names}{TimeInstantDataFrame}(x)
  \method{row.names}{TimeInstantDataFrame}(x) <- value

  \method{print}{TimeInstantDataFrame}(x, tz=NULL, \dots)
  \method{summary}{TimeInstantDataFrame}(object, \dots)
  \method{head}{TimeInstantDataFrame}(x, tz, \dots)
  \method{tail}{TimeInstantDataFrame}(x, tz, \dots)
  \S4method{show}{TimeInstantDataFrame}(object)

  \method{plot}{TimeInstantDataFrame}(x, y=NULL, type="p",
	lty=1:6, lwd=1, pch=1:25, col=NULL,
	xlim=NULL, ylim=NULL, log, main, sub, xlab, ylab,
	ann=par("ann"), axes=TRUE, asp=NA, \dots)
  \method{points}{TimeInstantDataFrame}(x, y=NULL, type="p",
	lty=1:6, lwd=1, pch=1:25, col=NULL, \dots)
  \method{lines}{TimeInstantDataFrame}(x, y=NULL, type="l",
	lty=1:6, lwd=1, pch=1:25, col=NULL, \dots)
  \method{barplot}{TimeInstantDataFrame}(height, format='', \dots)
}
\arguments{
  \item{when}{POSIXct or character representing a time with a valid format (see
	  \code{\link[base:as.POSIXct]{POSIXct}}). It gives the instant of each
	  row.}
  \item{timezone}{character representing a valid timezone (see
	  \code{\link[base]{timezone}}).}
  \item{data}{a data.frame with as much rows as needed for the created object. Can
	  be \code{NULL} (hence the data.frame has zero column and as much rows
	  as needed).}

  \item{from}{\describe{
    \item{as.TimeInstantDataFrame}{object to convert to a TimeInstantDataFrame}
    \item{RegularTimeInstantDataFrame}{
      POSIXct or character representing a time with a valid format (see
      \code{\link[base:as.POSIXct]{POSIXct}}). It represents the start of the
      object.}
  }}
  \item{cursor}{To convert TimeIntervalDataFrame,
	  it indicates where the TimeInstant
	  must be taken. If \code{0}, start of each intervals is taken as
	  instant ; if \code{1} end of each intervals is taken as instant.
	  Any other value will determine a weigthed instant between start and
	  end (actually, value higher than 1 or lower than 0 will give instant
	  outside this range).}

  \item{to}{POSIXct or character representing a time with a valid format (see
	  \code{\link[base:as.POSIXct]{POSIXct}}). It represents the end of the
	  object. If missing, its value is deduced from \sQuote{from},
	  \sQuote{by} and \sQuote{data}.}
  \item{by}{\describe{
    \item{RegularTimeInstantDataFrame}{a \code{\link{POSIXctp}} object indicating
	    the increment to use between instants of the object.}
    \item{merge}{specifications of the columns used for merging.}
  }}

  \item{x}{TimeInstantDataFrame object (to modify, to extract or to test)}
  \item{i}{indices specifying elements to extract or replace. (See
	  \code{\link[base]{Extract}})}
  \item{j}{indices specifying elements to extract or replace. (See
	  \code{\link[base]{Extract}})}
  \item{name}{A litteral character string or a name. (See
	  \code{\link[base]{Extract}})}
  \item{drop}{Used for compatibility with data.frame methods.}
  \item{value}{New value for the object designated (data.frame, names, row.names,
	  etc.).}

  \item{y}{TimeInstantDataFrame to merge with x}
  \item{all}{logical; see \code{\link[base]{merge}}}
  \item{tz}{character representing a valid timezone (see
	  \code{\link[base]{timezone}}).}

  \item{f}{a 'factor' in the sense that ‘as.factor(f)’ defines the
            grouping, or a list of such factors in which case their
            interaction is used for the grouping. See \code{\link{split}}.}

  \item{X}{a TimeInstantDataFrame on which the FUN must be applied.}
  \item{FUN}{function to apply over eahc columns of X.}

  \item{object}{TimeInstantDataFrame object (to modify, to extract or to test)}

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

  \item{height}{plotting argument, see \code{\link[graphics]{barplot}}}
  \item{format}{plotting argument, see \code{\link[graphics]{barplot}}}

  \item{\dots}{More arguments.}
}
\section{Objects from the Class}{
  Formally, the class consists of a \code{\link[base]{data.frame}}
  and, for each row, a \code{\link[base]{POSIXct}}.
  This class is provided to deal with punctual time data. Many 
  of such classes are defined in other packages. This one is defined
  mainly to provide a \sQuote{punctual} class compatible
  with \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
  and \code{\link[=SubtimeDataFrame-class]{SubtimeDataFrame}}.

  The construction of the class allows to manipulate objects as if they
  were data.frame (see \sQuote{Access to data} and \sQuote{Access to data
  properties}).
 
}
\section{Slots}{
  \describe{
    \item{\code{instant}:}{Object of class \code{"POSIXct"} corresponding to
 	    the instant of each row of the \code{data.frame}.}
    \item{\code{timezone}:}{Object of class \code{"character"} indicating the
	    timezone of data both for representation and calculation.}
    \item{\code{data}:}{Object of class \code{"data.frame"}  data contained by
	    the object.}
  }
}
\section{TimeInstantDataFrame constructors}{
  Objects can be created by calls of the form
  \itemize{
    \item \code{new("TimeInstantDataFrame", ...)} \dots argument must be replaced
    by named arguments coresponding to slots of a TimeInstantDataFrame (see below).
    See also \code{\link[methods]{new}}.
    \item \code{TimeInstantDataFrame (when, timezone='UTC', data=NULL, ...)}
    Arguments of the function correspond to object slots.
    \item \code{RegularTimeInstantDataFrame (from, to, by, timezone='UTC', data=NULL)}, Wrapper to construct TimeInstantDataFrame with specific properties. Instants
    of the TimeInstantDataFrame go from 'from' to 'to' regularly spaced by 
    'by', which is a \code{\link{POSIXctp}} or an object which can be coerced to.
    \item \code{as.TimeInstantDataFrame (from, ...)} Converting object to
    TimeInstantDataFrame.
  }
}
\section{Math}{
  Every functions defined in the Ops group (see \code{\link{Ops}})
  can be used width a TimeInstantDataFrame and numeric :
  \itemize{
  \item{\code{tidf * 2}}
  \item{\code{2 * tidf}}
  \item{\code{2:10 == tidf}}
  \item{\code{2^tidf}}
  \item{\code{tidf^2}}
  }
}
\section{Accessing to and manipulating data}{
  The TimeInstantDataFrame class is defined to works like the data.frame class
  with the difference that a time instant (\code{\link[base]{POSIXct}}) is
  attached to each rows of the data.frame. Thus to access and manipulate
  data of a TimeInstantDataFrame the following methods are defined : 
  '$', '$<-', '[', '[<-', '[[', '[[<-'. See \code{\link[base]{Extract}} for 
  details.

  With \sQuote{[} operator, a selection by dates is also available. If
  \sQuote{i} and or \sQuote{j} are POSIXt or strings that can be 
  converted to POSIXct (see below), they are considered as the 
  minimal and maximal time limits : all datas between those are selected.
  A string that can be converted to a POSIXct is (in this case only) a
  string composed of 3 parts separated by white space : 
  'YYYY-MM-DD HH:MM:SS tz'. The second and third parts are options, thus
  accepted format are :\itemize{
    \item 'YYYY-MM-DD'
    \item 'YYYY-MM-DD tz'
    \item 'YYYY-MM-DD HH:MM:SS'
    \item 'YYYY-MM-DD HH:MM:SS tz'
  }. If timezone is not given, it is assumed to be the same as the one
  of the object on which the selection is done.

  Other methods have been defined to allow some operations over
  TimeInstantDataFrame :
  \itemize{
    \item rbind and merge to join two (or more)
  TimeInstantDataFrame (see \code{\link[base]{rbind}} and
  \code{\link[base]{merge}}),
    \item a TimeInstantDataFrame can be splitted exactly the same way that
    a data.frame can (see \code{\link[base]{split}} in the base
  package),
    \item a function can be applied over each column of a TimeInstantDataFrame
    via the lapply function. If the function returns one value,
     the resulting value is a \code{\link{TimeIntervalDataFrame}}
    beginning at the first instant of the object and ending at the latest one ;
	else if the function returns as much values as the number of rows of
	the object, the TimeInstantDataFrame given in argument is returned with
	the new calculated values ; on others cases, a non-TimeInstantDataFrame
    object is returned.
  }

  Because a TimeInstantDataFrame works more or less like a data.frame, 
  the following methods work on a TimeInstantDataFrame :
  \code{\link[base]{dim}},
  \code{\link[base]{length}},
  \code{\link[base]{names}},
  \code{\link[base]{names<-}},
  \code{\link[base]{ncol}},
  \code{\link[base]{nrow}},
  \code{\link[base]{row.names}},
  \code{\link[base]{row.names<-}}.
}
\section{Access to time properties}{
  A TimeInstantDataFrame can be tested for a few time properties :
  \describe{
    \item{regular}{TRUE if all time instants are equally spaced ;}
    \item{timezone}{gives or sets the timezone of the TimeInstantDataFrame ;}
    \item{when}{returns a POSIXct object with the time instant of the 
	    TimeInstantDataFrame.}
  }
}
\section{Graphic representation}{
  To plot a \code{TimeInstantDataFrame} available functions are
  \code{\link[graphics]{plot}}, \code{\link[graphics]{lines}},
  \code{\link[graphics]{points}} and \code{\link[graphics]{barplot}}.

  These functions works more or less like their generic definition.
}
\section{Text representation}{
  To represent a \code{TimeInstantDataFrame} available functions are
  \code{\link[base:print.data.frame]{print}},
  \code{\link[base:data.frame]{summary}}, \code{\link[utils]{head}},
  \code{\link[utils]{tail}} and \code{\link[methods]{show}}.
}
\seealso{
  \code{\link{TimeIntervalDataFrame}}, \code{\link{SubtimeDataFrame}}
}
\examples{
showClass("TimeInstantDataFrame")
}
\keyword{classes}
