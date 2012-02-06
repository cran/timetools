#' Extract a specific subtime of of POSIXt object or define a subtime object
#'
#' This function allows to extract a subtime of a time
#' object or to build a subtime object (S3).
#'
#' \sQuote{subtime} extract one of those indicating in the \code{\link[base]{DateTimeClasses}}
#' documentation.
#' 
#' The result is an ordered factor containing the values corresponding
#' to the given argument \sQuote{x}. The levels of the factor are
#' all the available values for that subtime. To know
#' what are those values, see \code{\link[base]{DateTimeClasses}} documentation.
#' 
#' The factor is given an attributes 'timezone' which correspond of the
#' the timezone of the argument.
#'
#' @param x object to which extract subtime
#' @param representation character string indicating 
#' 	which subtime is to extract ('mday',
#'	'mon', 'wday', 'yday', 'sec', 'min', dhour').
#' @param \dots more arguments to or from other methods
#'
#' @return  a factor which depends on the subtime asked for. See \sQuote{Details}.
#'
#' @seealso \code{\link[base]{DateTimeClasses}}, \code{\link[base]{timezone}}
subtime <- function (x, representation, ...) UseMethod('subtime')
#' @rdname subtime
#' @section default:
#' If \sQuote{x} is missing, an empty factor with the appropriated
#' levels (according to \sQuote{representation}) is returned.
#' @method subtime default
#' @param timezone a character string to specify the timezone of subtime
subtime.default <- function (x, representation, timezone='UTC', ...)
{
	if (missing (x))
		return(subtime (as.POSIXlt(character(), timezone), representation, ...))
	subtime (as.POSIXct(x, timezone), representation, ...)
}
#' @rdname subtime
#' @section numeric:
#' If \sQuote{x} is a numeric, a appropriated subtime factor
#' is returned (in accordance with representation).
#' @method subtime numeric
subtime.numeric <- function (x, representation, timezone='UTC', ...)
{
	lvls <- levels (subtime(representation=representation))
	res <- factor (lvls[x], levels=lvls, ordered=TRUE)
	attributes(res)$timezone <- timezone
	class (res) <- c('subtime', 'factor')
	return (res)
}
#' @rdname subtime
#' @method subtime POSIXct
subtime.POSIXct <- function (x, representation, ...)
	subtime (as.POSIXlt(x), representation, ...)
#' @rdname subtime
#' @method subtime POSIXlt
subtime.POSIXlt <- function (x, representation, ...)
{
	if (representation == 'mday') {
		res <- factor (x$mday, 1:31, ordered=TRUE)
	} else if (representation == 'mon') {
		res <- factor (x$mon, 0:11,
					   format(as.POSIXct(sprintf('2011-%02i-10', 1:12)), '%B'),
					   ordered=TRUE)
	} else if (representation == 'wday') {
		res <- factor (x$wday, 0:6,
		               format(as.POSIXct(sprintf('2011-12-%02i', 25:31)), '%A'),
					   ordered=TRUE)
	} else if (representation == 'yday') {
		res <- factor (x$yday, 0:365, ordered=TRUE)
	} else if (representation == 'sec') {
		res <- factor (x$sec, 0:61, ordered=TRUE)
	} else if (representation == 'min') {
		res <- factor (x$min, 0:59, ordered=TRUE)
	} else if (representation == 'hour') {
		res <- factor (x$hour, 0:23, ordered=TRUE)
	}
	attributes(res)$timezone <- attributes(x)$tzone
	class (res) <- c('subtime', 'factor')
	return (res)
}

#' @rdname time.properties
#' @aliases timezone.subtime
#' @method timezone subtime
timezone.subtime <- function(object) attributes(object)$timezone

#' @rdname subtime
#' @method subtime TimeInstantDataFrame
subtime.TimeInstantDataFrame <- function (x, representation, ...)
	subtime(when(x), representation)

#' @rdname subtime
#' @method subtime TimeIntervalDataFrame
#' @param cursor indicates
#'	where the TimeInstant must be taken. If \code{0},
#' 	start of each intervals is taken as instant ;
#' 	if \code{1} end of each intervals is taken as instant.
#' 	Any other value will determine a weigthed instant 
#' 	between start and end (actually, value higher than 1 or 
#' 	lower than 0 will give instant outside this range).
subtime.TimeIntervalDataFrame <- function (x, representation, cursor=NULL, ...)
{
	if (is.null (cursor) ) cursor <- 0.5
	if (cursor > 1 || cursor < 0) warning ("For a standard use, cursor should be between 0 and 1.")
	instant <- mapply (function(x, y, wx, wy)
				weighted.mean (c(x, y), c(wx, wy), na.rm=TRUE),
			   start(x), end(x), 1-cursor, cursor)
	instant <- as.POSIXct(instant, origin=origin)
	subtime(instant, representation)
}
