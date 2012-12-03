#' Create an object of class POSIXst (subtime object) (S4 class)
#'
#' @param x object to which extract subtime
#' @param unit indicates the subtime part to extract ('year', 'month',
#'	'day', 'hour', 'minute', 'second')
#' @param of used to specify the main period 
#'	from wich the is to extract ('year', 'month', day', 'hour', 'minute').
#'	Not used for \sQuote{unit in c('year', 'month')}.
#' @param tz if needed, specifies th timezone of POSIXst
#' @param \dots more arguments to or from other methods
#'
#' @return  a factor which depends on the subtime asked for. See \sQuote{Details}.
#'
#' @seealso \code{\link[base]{DateTimeClasses}}, \code{\link[base]{timezone}},
#' \code{\link{time.properties}}
POSIXst <- function (x, unit, of=NULL, tz='UTC', ...) UseMethod('POSIXst')
#' @rdname POSIXst
#' @section default:
#' If \sQuote{x} is missing, an empty factor with the appropriated
#' levels (according to \sQuote{unit and of}) is returned.
#' 
#' @method POSIXst default
POSIXst.default <- function (x, unit, of=NULL, tz='UTC', ...)
{
	if (missing (x))
		return(POSIXst (as.POSIXlt(character(), tz),
				unit, of, tz, ...))
	stop(sprintf("'POSIXst' method not implemented for %s object", class(x)))
}
#' @rdname POSIXst
#' @section integer:
#' This is the default constructor.
#' 
#' @method POSIXst integer
POSIXst.integer <- function (x, unit, of=NULL, tz='UTC', ...)
{
	unit <- POSIXt.units(unit)
	if( unit == POSIXt.units('year') & is.null(of) )
		of <- POSIXt.units('AD') else
	if( unit == POSIXt.units('month') & is.null(of) )
		of <- POSIXt.units('year') else
		of <- POSIXt.units(of)
	new('POSIXst', subtime=x, unit=unit, of=of, timezone=tz)
}
#' @rdname POSIXst
#' @section numeric:
#' If \sQuote{x} is a numeric, its values must be in the right range
#' (see \code{\link[base]{DateTimeClasses}} and must be "like" an integer.
#' 
#' @method POSIXst numeric
POSIXst.numeric <- function (x, unit, of=NULL, tz='UTC', ...)
{
	x <- as.character(x)
	if( any(grepl('\\.', x)) ) stop("'x', must be a whole number)")
	x <- as.integer(x)
	POSIXst(x, unit, of, tz, ...)
}
#' @rdname POSIXst
#' @section POSIXct:
#' with the \sQuote{tz} argument, one can specify/change
#' the timezone of the resulting POSIXst
#' @method POSIXst POSIXct
POSIXst.POSIXct <- function (x, unit, of=NULL, tz=attributes(x)$tzone, ...)
{
	x <- as.POSIXlt(x)
	POSIXst (x, unit, of, tz[1], ...)
}
#' @rdname POSIXst
#' @section POSIXlt:
#' with the \sQuote{tz} argument, one can specify/change
#' the timezone of the resulting POSIXst
#' @method POSIXst POSIXlt
POSIXst.POSIXlt <- function (x, unit, of=NULL, tz=attributes(x)$tzone, ...)
{
	unit <- POSIXt.units(unit)
	if( unit == POSIXt.units('year') & is.null(of) )
		of <- POSIXt.units('AD') else
	if( unit == POSIXt.units('month') & is.null(of) )
		of <- POSIXt.units('year') else
		of <- POSIXt.units(of)

	x <- as.POSIXlt(as.POSIXct(x), tz[1])

	if (unit == POSIXt.units('year')) {
		res <- x$year+1900
	} else if (unit==POSIXt.units('month')) {
		res <- x$mon
	} else if (unit == POSIXt.units('day')) {
		res <- switch (as.character(of),
			year = x$yday,
			month = x$mday,
			week = x$wday,
			"'of' should be one of (year, month, week)")
	} else if (unit == POSIXt.units('hour')) {
		res <- switch (as.character(of),
			year	=  x$yday	* 24 + x$hour,
			month	= (x$mday-1)	* 24 + x$hour,
			week	=  x$wday	* 24 + x$hour,
			day	= 		       x$hour,
			"'of' should be one of (year, month, week, day)")
	} else if (unit == POSIXt.units('minute')) {
		res <- switch (as.character(of),
			year	=( x$yday	* 24 + x$hour) * 60 + x$min,
			month	=((x$mday-1)	* 24 + x$hour) * 60 + x$min,
			week	=( x$wday	* 24 + x$hour) * 60 + x$min,
			day	= (		       x$hour) * 60 + x$min,
			hour	=				      x$min,
			"'of' should be one of (year, month, week, day, hour)")
	} else if (unit == POSIXt.units('second')) {
		res <- switch (as.character(of),
			year	=( (x$yday	* 24 + x$hour) * 60 + x$min) * 60 + x$sec,
			month	=(((x$mday-1)	* 24 + x$hour) * 60 + x$min) * 60 + x$sec,
			week	=( (x$wday	* 24 + x$hour) * 60 + x$min) * 60 + x$sec,
			day	=((		       x$hour) * 60 + x$min) * 60 + x$sec,
			hour	=(				      x$min) * 60 + x$sec,
			minute	= 						    x$sec,
		"'of' should be one of (year, month, week, day, hour, minute)")
	}
	return( POSIXst(res, unit, of, tz) )
}

#' @rdname POSIXst
#' @section TimeInstantDataFrame:
#' This methode extract subtimes (\code{\link{POSIXst}}), from 
#' \code{\link{TimeInstantDataFrame}}.
#'
#' with the \sQuote{tz} argument, one can specify/change
#' the timezone of the resulting POSIXst
#' @method POSIXst TimeInstantDataFrame
POSIXst.TimeInstantDataFrame <- function (x, unit, of=NULL, tz=timezone(x), ...)
{
	POSIXst(force(when(x)), unit, of, tz, ...)
}

#' @rdname POSIXst
#' @param cursor for TimeIntervalDataFrame, if not NULL, the object
#' is first coerced to a TimeInstantDataFrame using the
#' \code{\link{as.TimeInstantDataFrame}} method.
#' @method POSIXst TimeIntervalDataFrame
#'
#' @section TimeIntervalDataFrame:
#' Because an time interval can contain several POSIXst of one kind
#' for instance a day contains all 'hours of day'), the result
#' of this function for TimeIntervalDataFrame is a list
#' of POSIXst. Each element of the list contains the POSIXsts asked
#' for corresponding to each row of the TimeIntervalDataFrame
#' object.
#'
POSIXst.TimeIntervalDataFrame <- function (x, unit, of=NULL, tz=timezone(x), ..., cursor=NULL)
{
	if( !is.null(cursor) )
		return( POSIXst(as.TimeInstantDataFrame(x, cursor), unit, of, tz) )

	unit <- POSIXt.units(unit)
	u <- as.character(unit)
	u <- switch(u,
		second='sec', minute='min', u)
	st <- mapply(seq.POSIXt, SIMPLIFY=FALSE,
		     start(x), end(x),
		     MoreArgs=list(by=u))

	st <- lapply(st, POSIXst, unit, of, tz)

	return( st )
}



