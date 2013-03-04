#' @rdname changeSupport
changeSupport <- function(from,to,min.coverage,FUN=NULL,weights.arg=NULL,
			  split.from=FALSE,merge.from=TRUE, ...)
UseMethod ('changeSupport')
#' Function to change time support of TimeIntervalDataFrame
#'
#' Methods that allows to agregate AND disagregate
#' homogeneous AND heterogeneous time data.
#' 
#' Agregating homogeneous data is for example to calculate
#' daily means of time series from hourly time series.
#'
#' Agregating heterogeneous data is for example to calculate
#' annual means of time series from monthly time series (because each
#' month doesn(t have identical weight).
#'
#' In above cases, the \code{min.coverage} allows to control
#' if means should be calculated or not : for the monthly case,
#' if there are \code{NA} values and the time coverage of \sQuote{not NA}
#' values is lower \code{min.coverage} the result will be \code{NA} ;
#' if time coverage is higher than \code{min.coverage}, the 
#' annual mean will be \sQuote{estimated} by the mean of available
#' data.
#'
#' Disagregating data is more \sQuote{articficial} and is 
#' disabled by default (with the \code{split.from} argument).
#' This argument is also used to precise if one value can
#' be use for agregation in more than one interval in the resulting
#' TimeIntervalDataFrame (for sliding intervals for instance).
#' Here are some examples of time disagregration :
#' \itemize{
#' \item A weekly mean can be dispatched over the days of the week.
#'  By default, the value attribuated to each day is the value 
#' of the week, but this can be changed by using a special function
#' (\code{FUN} argument).
#' \item The value of a variable is known from monday at 15 hours to 
#' tuesday at 15 hours and from tuesday at 15 hours to wednesday at 15 hours.
#' To \sQuote{evaluate} the value of the variable for tuesday can be 
#' estimated by doing a weigthed mean between the two values. Weights are
#' determined by the intersection between each interval and tuesday.
#' Here weights will be \code{0.625} (15/24) and \code{0.375} (9/24)
#' (In this case, disagration is combined with a \sQuote{reagregation}).
#' }
#'
#' These are \sQuote{trivial} examples but many other usage can be found
#' for these methods. Other functions than \code{weighted.mean}
#' or \code{mean} can be used.
#' The Qair package (in its legislative part) gives several examples of usage
#' (this package is not availables on CRAN but see \sQuote{references}
#' to know where you can find it).
#'
#' @examples
#'ti3 <- TimeIntervalDataFrame (
#'        c('2010-01-01', '2010-01-02', '2010-01-04'), NULL,
#'        'UTC', data.frame(ex3=c(6, 1.5)))
#'
#'# weighted mean over a period of 3 days with at least 75% of
#'# coverage (NA is retunr if not)
#'ti3
#'d <- POSIXctp(unit='day')
#'changeSupport (ti3, 3L*d, 0.75)
#'
#'ti4 <- TimeIntervalDataFrame (
#'	c('2010-01-01', '2010-01-02', '2010-01-04',
#'	  '2010-01-07', '2010-01-09', '2010-01-10'), NULL,
#'          'UTC', data.frame(ex4=c(6, 1.5, 5, 3, NA)))
#'
#'# weighted mean over a period of 3 days with at least 75% of
#'# coverage (NA is retunr if not) or 50%
#'ti4
#'changeSupport (ti4, 3L*d, 0.75)
#'changeSupport (ti4, 3L*d, 0.5)
#'
#'# use of split.from
#'ti1 <- RegularTimeIntervalDataFrame('2011-01-01', '2011-02-01', 'hour')
#'ti1$value <- 1:nrow(ti1)
#'# we can calculate sliding mean over periods of 24 hours.
#'# first lets build the corresponding TimeIntervalDataFrame
#'ti2 <- RegularTimeIntervalDataFrame('2011-01-01', '2011-02-01', 'hour', 'day')
#'# if we try to 'project' ti1 over ti2 it won't work :
#'summary (changeSupport (ti1, ti2, 0))
#'# all data are NA because 'spliting' is not enabled. Let's enable it :
#'summary (changeSupport (ti1, ti2, 0, split.from=TRUE))
#'
#' @param from \code{\link{TimeIntervalDataFrame}} for wich
#' the time support is to change
#' @param to an object indicating the new support, see specific sections
#' @param min.coverage a numeric between 0 and 1 indicating the percentage
#'  of valid values over each interval to allow an aggregation. NA is 
#' returned if the percentage is not reach. In changeSupport, when values
#' are aggregated, intervals are not allowed to overlap.
#' When a function (FUN) has a na.rm argument, the na.rm=TRUE behaviour
#' is met if na.rm is set to TRUE and min.coverage to 0 (zero) ; the
#' na.rm=FALSE behaviour is met if na.rm is set to FALSE whatever is the value
#' of min.coverage.
#' @param FUN function use to agregate data of from. 
#' By default \code{\link[base]{mean}} if \sQuote{from} is 
#' \code{\link{homogeneous}}. \code{\link[stats]{weighted.mean}} otherwise.
#' @param weights.arg if FUN has a \sQuote{weight} argument, this parameter must
#' be a character naming the weight argument. For instance, if FUN is 
#' \code{\link[stats]{weighted.mean}}, then weights.arg is \code{'w'}.
#' @param \dots arguments for FUN or for other methods
#' @param split.from logical indicating if data in \sQuote{from} can be
#' used for several intervals of the new time support (see \sQuote{details}).
#' @param merge.from logical indicating if data in \sQuote{from} can be
#' merged over interval of the new time support.
#' 
#' @return \code{\link{TimeIntervalDataFrame}}
#' @references Qair-package : \url{http://sourceforge.net/projects/packagerqair/}
#'
#' @seealso \code{\link{TimeIntervalDataFrame}}, \code{\link{POSIXcti}}
setGeneric (name='changeSupport', def=function(from, to, min.coverage,
					       FUN=NULL, weights.arg=NULL,
					       split.from=FALSE, merge.from=TRUE,
					       ...)
	    standardGeneric('changeSupport') )
 
#' @rdname changeSupport
#' @aliases changeSupport,TimeIntervalDataFrame,TimeIntervalDataFrame,numeric-method
#' @section from=TimeIntervalDataFrame, to=TimeIntervalDataFrame:
#' \code{to} is a TimeIntervalDataFrame. The method will try to 
#' adapt data of \code{from} over interval of \code{to}.
#' The returned object is the \code{to} TimeIntervalDataFrame with
#' new columns corresponding of those of \code{from}.
#'
#' If merge.from is TRUE, values affected for each interval 
#' of \code{to} will be calculated with all data in the interval.
#' If split.from is TRUE, values partially in the interval will also
#' be used for calculation.
#'
#' If merge.from is FALSE, values affected for each interval of \code{to}
#' will be the one inside this interval. If several values are inside
#' the interval, \code{NA} will be affected.
#' If split.from is TRUE, a value partially inside the interval is
#' considered as being inside it. So if there is no other values
#' in the interval, this value will be affected, else \code{NA}
#' will be affected.

setMethod ('changeSupport',
	   signature(from='TimeIntervalDataFrame', to='TimeIntervalDataFrame',
		     min.coverage='numeric', FUN='ANY', weights.arg='ANY',
		     split.from='ANY', merge.from='ANY'),
	   definition=function (from, to, min.coverage, FUN=NULL, weights.arg=NULL,
				split.from=FALSE, merge.from=TRUE, ...)
{
	fun.args <- list (...)
	if (is.null (FUN) ) {
		if (length (fun.args) != 0 | !is.null(weights.arg) )
			warning ('Arguments passed to FUN are set to default.')
		if (homogeneous (from) ) {
			FUN <- mean
			fun.args <- list(na.rm = TRUE)
		} else {
			FUN <- weighted.mean
			weights.arg <- 'w'
			fun.args <- list(na.rm = TRUE)
		}
	}

	do.call(tapply, c(X=from, INDEX=to, FUN=FUN, fun.args, min.coverage=min.coverage,
		weights.arg=weights.arg, merge.X=merge.from, split.X=split.from,
		keep.INDEX=TRUE, simplify=TRUE))

} )

#' @rdname changeSupport
#' @aliases changeSupport,TimeIntervalDataFrame,character,numeric-method
#' @section from=TimeIntervalDataFrame, to=character:
#' \code{to} is one of 'year', 'month', 'day', 'hour', 'minute' or 'second'.
#' It defines the period (\code{\link{POSIXctp}}) to use to build
#' the new TimeIntervalDataFrame on which \code{from} will 
#' be agregated (or disagregated).
#'
#' So first, an \sQuote{empty} (no data) TimeIntervalDataFrame is created,
#' and then, the agregation is done accordingly to the  
#' \sQuote{from=TimeIntervalDataFrame, to=TimeIntervalDataFrame} section.
setMethod ('changeSupport',
	   signature(from='TimeIntervalDataFrame', to='character',
		     min.coverage='numeric', FUN='ANY', weights.arg='ANY', 
		     split.from='ANY', merge.from='ANY'),
	   definition= function (from, to, min.coverage, FUN=NULL,
				 weights.arg=NULL, split.from=FALSE,
				 merge.from=TRUE, ...)
{
	period <- POSIXctp(unit=to)
	return (changeSupport (from, period, min.coverage, FUN, weights.arg,
			       split.from, merge.from, ...) )
} )

#' @rdname changeSupport
#' @aliases changeSupport,TimeIntervalDataFrame,POSIXctp,numeric-method
#' @section from=TimeIntervalDataFrame, to=POSIXctp:
#' \code{to} is period (see \code{\link{POSIXctp}}).
#' It defines the base of the new TimeIntervalDataFrame on which
#' \code{from} will be agregated (or disagregated).
#'
#' So first, an \sQuote{empty} (no data) TimeIntervalDataFrame is created,
#' and then, the agregation is done accordingly to the  
#' \sQuote{from=TimeIntervalDataFrame, to=TimeIntervalDataFrame} section.
setMethod ('changeSupport',
	   signature(from='TimeIntervalDataFrame', to='POSIXctp',
		     min.coverage='numeric', FUN='ANY', weights.arg='ANY', 
		     split.from='ANY', merge.from='ANY'),
	   definition= function (from, to, min.coverage, FUN=NULL,
				 weights.arg=NULL, split.from=FALSE,
				 merge.from=TRUE, ...)
{
	# if from and to have same base, no calculus to do 

	if (homogeneous (from) &&
	    continuous(from)  &&
	    to == period (from) )
		return (from)

	s <- min(start( from ))
	e <- max(end( from ))
	tzone <- timezone( from )
	to <- TimeIntervalDataFrame(s, e, period=to, timezone=tzone)
	
	return( changeSupport(from, to, min.coverage, FUN, weights.arg,
	      		      split.from, merge.from, ...) )
} )

