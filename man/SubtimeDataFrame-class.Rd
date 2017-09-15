\name{SubtimeDataFrame}
\docType{class}
\alias{SubtimeDataFrame-class}
\alias{SubtimeDataFrame}
\alias{as.SubtimeDataFrame}

\alias{$,SubtimeDataFrame-method}
\alias{$<-,SubtimeDataFrame-method}
\alias{[.SubtimeDataFrame}
\alias{[<-.SubtimeDataFrame}
\alias{[[,SubtimeDataFrame-method}
\alias{[[<-.SubtimeDataFrame}

%\alias{rbind.SubtimeDataFrame} % n'existe pas
\alias{split.SubtimeDataFrame}
\alias{merge.SubtimeDataFrame}
\alias{lapply,SubtimeDataFrame-method}

\alias{timezone,SubtimeDataFrame-method}
\alias{when,SubtimeDataFrame-method}
\alias{unit.SubtimeDataFrame}
\alias{of.SubtimeDataFrame}

\alias{dim,SubtimeDataFrame-method}
\alias{length,SubtimeDataFrame-method}
\alias{names,SubtimeDataFrame-method}
\alias{names<-,SubtimeDataFrame-method}
\alias{ncol,SubtimeDataFrame-method}
\alias{nrow,SubtimeDataFrame-method}
\alias{row.names.SubtimeDataFrame}
\alias{row.names<-.SubtimeDataFrame}

\alias{print.SubtimeDataFrame}
\alias{summary.SubtimeDataFrame}
\alias{head.SubtimeDataFrame}
\alias{tail.SubtimeDataFrame}
\alias{show,SubtimeDataFrame-method}

\alias{plot.SubtimeDataFrame}
\alias{points.SubtimeDataFrame}
\alias{lines.SubtimeDataFrame}
\alias{barplot.SubtimeDataFrame}

\alias{as.SubtimeDataFrame}
\alias{as.SubtimeDataFrame.TimeInstantDataFrame}
\alias{as.SubtimeDataFrame.TimeIntervalDataFrame}

%\alias{as.data.frame.SubtimeDataFrame}

\title{Class \code{"SubtimeDataFrame"}}
\description{
  Class to hold subtime data such a day of week, month of year, etc.
}
\usage{
SubtimeDataFrame(when, data = NULL, \dots)

as.SubtimeDataFrame(x, unit, of, ...)
  \method{as.SubtimeDataFrame}{TimeInstantDataFrame}(x, unit, of, FUN=NULL, ...)
  \method{as.SubtimeDataFrame}{TimeIntervalDataFrame}(x, unit, of, FUN=NULL, cursor=NULL, ...)

  \S4method{$}{SubtimeDataFrame}(x, name)
  \S4method{$}{SubtimeDataFrame}(x, name) <- value
  \method{[}{SubtimeDataFrame}(x, i, j, drop=FALSE)
  \method{[}{SubtimeDataFrame}(x, i, j) <- value
  \S4method{[[}{SubtimeDataFrame}(x ,i, j, \dots)
  \method{[[}{SubtimeDataFrame}(x, i, j) <- value

  \method{merge}{SubtimeDataFrame}(x, y, by, all=TRUE, sort=FALSE, \dots)
  \method{split}{SubtimeDataFrame}(x, f, drop=FALSE, \dots)
  \S4method{lapply}{SubtimeDataFrame}(X, FUN, \dots)

  \S4method{timezone}{SubtimeDataFrame}(object)
  \S4method{when}{SubtimeDataFrame}(x, \dots)
  \method{unit}{SubtimeDataFrame}(x, \dots)
  \method{of}{SubtimeDataFrame}(x, \dots)

  \S4method{dim}{SubtimeDataFrame}(x)
  \S4method{length}{SubtimeDataFrame}(x)
  \S4method{names}{SubtimeDataFrame}(x)
  \S4method{names}{SubtimeDataFrame}(x) <- value
  \S4method{ncol}{SubtimeDataFrame}(x)
  \S4method{nrow}{SubtimeDataFrame}(x)
  \method{row.names}{SubtimeDataFrame}(x)
  \method{row.names}{SubtimeDataFrame}(x) <- value

  \method{print}{SubtimeDataFrame}(x, \dots)
  \method{summary}{SubtimeDataFrame}(object, \dots)
  \method{head}{SubtimeDataFrame}(x, \dots)
  \method{tail}{SubtimeDataFrame}(x, \dots)
  \S4method{show}{SubtimeDataFrame}(object)

  \method{plot}{SubtimeDataFrame}(
    x, y=NULL, type='p', lty=1:6, lwd=1, pch=1:25, col=NULL,
    xlim=NULL, ylim=NULL, log='', main='', sub='', xlab='', ylab='',
    ann=par('ann'), axes=TRUE, asp=NA, as.is=TRUE, format=NULL, \dots)
  \method{points}{SubtimeDataFrame}(
    x, y=NULL, type='p', lty=1:6, lwd=1, pch=1:25, col=NULL, as.is=TRUE, \dots)
  \method{lines}{SubtimeDataFrame}(
    x, y=NULL, type='l', lty=1:6, lwd=1, pch=1:25, col=NULL, as.is=TRUE, \dots)
  \method{barplot}{SubtimeDataFrame}(height, format='', \dots)
}
\arguments{
  \item{when}{\code{\link{POSIXst}}.}
  \item{data}{a data.frame with as much rows as needed for the created object. Can
	  be \code{NULL} (hence the data.frame has zero column and as much rows
	  as needed).}

  \item{x}{object to convert to a TimeInstantDataFrame or
    SubtimeDataFrame object (to modify, to extract or to test)}

  \item{unit}{indicates the subtime part to extract
  ('year', 'month', 'day', 'hour', 'minute', 'second').}

  \item{of}{used to specify the main period from wich the
  is to extract ('year', 'month', 'day', 'hour', 'minute').
  Not used for \sQuote{unit in c('year', 'month')}.}

  \item{FUN}{function to use for the agregation (if wanted,
  see \sQuote{details})}

  \item{cursor}{For TimeIntervalDataFrame, it indicates
  where the TimeInstant must be taken. If \code{0}, start
  of each intervals is taken as instant ; if \code{1} end
  of each intervals is taken as instant.  Any other value
  will determine a weigthed instant between start and end
  (actually, value higher than 1 or lower than 0 will give
  instant outside this range).}

  \item{i}{indices specifying elements to extract or replace. (See
	  \code{\link[base]{Extract}})}
  \item{j}{indices specifying elements to extract or replace. (See
	  \code{\link[base]{Extract}})}
  \item{name}{A litteral character string or a name. (See
	  \code{\link[base]{Extract}})}
  \item{drop}{Used for compatibility with data.frame methods.}
  \item{value}{New value for the object designated (data.frame, names, row.names,
	  etc.).}

  \item{y}{SubtimeDataFrame to merge with x}
  \item{all}{logical; see \code{\link[base]{merge}}}
  \item{by}{specifications of the columns used for merging.}
  \item{sort}{logical; if TRUE the resulting merged SubtimeDataFrame is 
  	  ordered according to 'when' values.}

  \item{f}{a 'factor' in the sense that ‘as.factor(f)’ defines the
            grouping, or a list of such factors in which case their
            interaction is used for the grouping. See \code{\link{split}}.}

  \item{X}{a SubtimeDataFrame on which the FUN must be applied.}

  \item{object}{SubtimeDataFrame object (to modify, to extract or to test)}

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

  \item{as.is}{should data be represented incrementally (the first row
  is given an x-value of 1, the second of 2, ... the last of n)(TRUE) or
  should data be grouped by their subtime (all value corresponding
  to monday are drawn at an x-value of 1) (FALSE) ?}

  \item{height}{plotting argument, see \code{\link[graphics]{barplot}}}
  \item{format}{for barplot see \code{\link[graphics]{barplot}}, 
  otherwise a string to format the x-labels according to the format
  method of POSIXst objects (see 'Text representation' of \code{\link{POSIXst}}).}

  \item{\dots}{More arguments.}
}
\section{Objects from the Class}{
  Formally, the class consists of a \code{\link[base]{data.frame}}
  and, for each row, a \code{\link{POSIXst}} (or subtime).
  This class is provided to deal with subtime data. This class
  is compatible
  with \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
  and \code{\link[=TimeIntervalDataFrame-class]{TimeInstantDataFrame}}.

  The construction of the class allows to manipulate objects as if they
  were data.frame (see \sQuote{Access to data} and \sQuote{Access to data properties}).
  Several functions are also available to access to time properties (see \sQuote{Access
  to time properties}).
 
  Methods are also available to facilitate the representations of instances of that class :
  see \sQuote{graphic representation} and \sQuote{text representation}.

  Finally, some specific methods allow to easily deal with agregation of data over time
  properties (day, hour, week, special or specific time).
}
\section{SubtimeDataFrame constructors}{
  Objects can be created by calls of the form
  \itemize{
    \item \code{new("SubtimeDataFrame", ...)} \dots argument must be replaced
    by named arguments coresponding to slots of a SubtimeDataFrame (see below).
    See also \code{\link[methods]{new}}.
    \item \code{SubtimeDataFrame (when, data=NULL, ...)}
    Arguments of the function correspond to object slots.
    \item{\code{as.SubtimeDataFrame (from, representation, cursor=NULL, FUN=mean, ...)}
    Converting object to SubtimeDataFrame.
    Conversion from a TimeIntervalDataFrame to a
    SubtimeDataFrame can be direct or after agregation.
    
    For a direct conversion (where date are only replaced by
    the desired subtime), FUN must be NULL.

    For an agregateed conversion, the function to use must be
    indicated by the FUN arg and all arguments to pass to
    this function can be given (namely).}
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
\section{Accessing to and manipulating data}{
  The SubtimeDataFrame class is defined to works like the data.frame class
  with the difference that a subtime (\code{\link{POSIXst}}) is
  attached to each rows of the data.frame. Thus to access and manipulate
  data of a TimeInstantDataFrame the following methods are defined : 
  '$', '$<-', '[', '[<-', '[[', '[[<-'. See \code{\link[base]{Extract}} for 
  details.

  Other methods have been defined to allow some operations over
  TimeInstantDataFrame :
  \itemize{
    \item  merge to join two (or more) SubtimeDataFrame (see 
    \code{\link[base]{merge}}),
    \item a SubtimeDataFrame can be splitted exactly the same way that
    a data.frame can (see \code{\link[base]{split}} in the base
    package),
    \item a function can be applied over each column of a TimeIntervalDataFrame
    via the lapply function provided that the function return one value (in
    this case the resulting value is a \code{\link{TimeIntervalDataFrame}}
    beginning at the first instant of the object and ending at the latest one),
    or as much values as the number of rows of the object (in this case the
    SubtimeDataFrame given in argument in returned with the new values
    calculated).
  }

  Because a SubtimeDataFrame works more or less like a data.frame, 
  the following methods work on a SubtimeDataFrame :
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
  A SubtimeDataFrame can be tested for a few time properties :
  \describe{
    \item{timezone}{gives or sets the timezone of the SubtimeDataFrame ;}
    \item{when}{returns a POSIXst object with the time instant of the 
	    SubtimeDataFrame.}
    \item{unit}{returns the unit of the time instant of the 
	    SubtimeDataFrame.}
    \item{of}{returns the 'of' of the time instant of the 
	    SubtimeDataFrame.}
  }
}
\section{Graphic representation}{
  To plot a \code{SubtimeDataFrame} available functions are
  \code{\link[graphics]{plot}}, \code{\link[graphics]{lines}},
  \code{\link[graphics]{points}} and \code{\link[graphics]{barplot}}.

  These functions works more or less like their generic definition.
}
\section{Text representation}{
  To represent a \code{SubtimeDataFrame} available functions are
  \code{\link[base:print.data.frame]{print}},
  \code{\link[base:data.frame]{summary}}, \code{\link[utils]{head}},
  \code{\link[utils]{tail}} and \code{\link[methods]{show}}.
}
\seealso{
  \code{\link{TimeInstantDataFrame}}, \code{\link{TimeIntervalDataFrame}},
  \code{\link{POSIXst}}
}
\examples{
showClass("SubtimeDataFrame")
}
\keyword{classes}
