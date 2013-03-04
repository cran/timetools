# definition de la classe
#------------------------
setClass ('POSIXctp',
	representation (duration='integer', unit='factor'),
	validity=function(object) {
		if (length (object@unit) != length (object@duration) ) {
			return ("Slots 'duration' and 'unit' must have same length.")
		} else if (!all (object@unit %in% POSIXt.units() ) ) {
			return (sprintf ('%s are not a valid units.', paste (as.character (object@unit), collapse=', ') ) )
		} else return (TRUE)
	})

# constructeur
#-------------
POSIXctp <- function (duration, unit)
{
	if (missing (duration)) duration <- 1L
	if( missing(unit) )
	{
		unit <- duration
		duration <- 1L
	}
	if (inherits(duration, 'numeric') & !inherits(duration, 'integer')) {
		#warning('duration is not an integer. It will be coerced to')
		duration <- as.integer (duration)
	}
	if( length(duration) > 1 & length(unit) == 1)
		unit <- rep( unit, length(duration) )
	if (inherits (unit, 'character'))
		unit <- POSIXt.units(unit)
	return (new('POSIXctp', duration=duration, unit=unit))
}

# acces aux proprietes
#---------------------
#' @rdname posix.properties
#' @aliases unit,POSIXctp-method
setMethod ('unit', 'POSIXctp', function(x, ...) (x@unit))
#' @rdname posix.properties
#' @param object POSIXctp to which the unit is to be changed
#' @param value a character or a \code{\link{POSIXt.units}} indicating 
#' the new units of object. The conversion will be effective only 
#' if it makes sense  (\sQuote{hour} to \sQuote{second}, ok ; 
#' \sQuote{year} to \sQuote{month}, ok ; \sQuote{month} to \sQuote{minute}
#' , NOT ok ; etc.
#' @aliases unit<-,POSIXctp-method
setMethod (f='unit<-', signature='POSIXctp',
		  definition=function(object, value) {
			  value <- POSIXt.units(value)
			  conversion <- array (
				c(1, 12, rep(NA, 5),
				  NA, 1, rep(NA, 5), 
				  rep(NA, 2), 1, 7, 7*24, 7*24*60, 7*24*60*60,
				  rep(NA, 3), 1, 24, 24*60, 24*60*60,
				  rep(NA, 4), 1, 60, 60*60,
				  rep(NA, 5), 1, 60, rep(NA, 6), 1),
				dim=c(7, 7),
				dimnames=list (to=c('year', 'month', 'week', 'day', 'hour', 'minute', 'second'),
					       from=c('year', 'month', 'week', 'day', 'hour', 'minute', 'second')))
			  conv <- conversion[cbind (as.character(value), as.character(unit(object)))]
			  if (any (is.na(conv)))
			      warning('some POSIXctp can not be converted due to incompatible units (year to hour for instance)')
			  POSIXctp (duration(object) * ifelse(is.na(conv), 1, conv),
				    ifelse(is.na(conv), as.character (unit(object)), as.character(value)))
} )
#' @rdname posix.properties
#' @aliases duration,POSIXctp-method
setMethod ('duration', 'POSIXctp', function(x, ...) x@duration)

format.POSIXctp <- function (x, ...) {
	sprintf ('%i %s%s', x@duration, as.character (x@unit), ifelse(x@duration>1, 's', ''))
}
print.POSIXctp <- function(x, ...) print (format (x) )
setMethod ('show', 'POSIXctp', function(object) show (format (object) ))

tail.POSIXctp <- function (x, ...) tail(format(x, ...))

head.POSIXctp <- function (x, ...) head(format(x, ...))

summary.POSIXctp <- function (object, ...)
	summary(format(object, ...))

setMethod ('length', 'POSIXctp', function(x)length (x@duration))

'[<-.POSIXctp' <- function (x, i, value) {
	d <- duration (x)
	d[i] <- duration (value)
	u <- unit (x)
	u[i] <- unit (value)
	new ('POSIXctp', duration=d, unit=u)
}
'[.POSIXctp' <- function (x, i, ...) new ('POSIXctp', duration=duration (x)[i], unit=unit (x)[i])

#' @rdname as.POSIXctp
#' @method as.POSIXctp logical
as.POSIXctp.logical <- function (from, ...)
	if (is.na(from))
		new ('POSIXctp', duration=as.integer (NA), unit=POSIXt.units('second') ) else
		stop ('Cannot coerce a logical to a POSIXctp.')

c.POSIXctp <- function(...){
	pers <- list(...)
	if (!all (sapply (pers, inherits, 'POSIXctp') ) )
		NextMethod('c')
	else
		new('POSIXctp', duration=unlist(lapply(pers, duration)), unit=unlist(lapply(pers, unit)))
}

Ops.POSIXctp <- function (e1, e2) {
	if (!inherits (e2, 'POSIXctp') ) return (NextMethod (.Generic) )
	if (!.Generic %in% c('==', '!=', '<=', '<', '>', '>='))# stop (sprintf ("%s not implemented for 'period' objects"), .Generic)
		#                 stop(gettextf("'%s' not defined for \"period\" objects", 
		#                                           .Generic), domain = NA)
		return (NextMethod(.Generic) )
	if (.Generic == '==') {
		if (any (unit(e1) > unit(e2)))
			suppressWarnings (unit(e1[unit(e1) > unit(e2)]) <- unit(e2)[unit(e1) > unit(e2)])
		if (any (unit(e2) > unit(e1)))
			suppressWarnings (unit(e2[unit(e2) > unit(e1)]) <- unit(e1)[unit(e2) > unit(e1)])
		return (duration(e1) == duration(e2) & unit(e1) == unit(e2))
	}
	if (.Generic == '!=')
		return (!e1 == e2)
	if (.Generic == '<') {
		res <- unit (e1) < unit(e2) | (unit(e1) == unit(e2) & duration(e1) < duration(e2) )
		return (res)
	}
	if (.Generic == '<=') {
		res <- unit (e1) < unit(e2) | (unit(e1) == unit(e2) & duration(e1) <= duration(e2) )
		return (res)
	}
	if (.Generic == '>')
		return (e2 < e1)
	if (.Generic == '>=')
		return (e2 <= e1)
}

setMethod('as.numeric', 'POSIXctp', function(x, ...) return( x@duration ))

setMethod('match', signature('POSIXctp', 'POSIXctp'),
	function(x, table, nomatch = NA_integer_, incomparables=NULL)
	{
		m <- match(x@duration, table@duration, nomatch, incomparables)
		m[unit(table)[m]!=unit(x)] <- nomatch
		m
	} )
setMethod('match', signature('POSIXctp', 'ANY'),
	function(x, table, nomatch = NA_integer_, incomparables=NULL)
	{
		if( length(unique(unit(x))) != 1 )
			stop("unit of table can't be guessed from x")
		table <- POSIXctp(table, unique(unit(x)))
		match(x, table, nomatch, incomparables)
	} )
setMethod('%in%', signature('POSIXctp', 'ANY'),
	function(x, table) match(x, table, 0L) > 0L)

unique.POSIXctp <- function(x, incomparables=FALSE, ...)
{
	u <- x[1]
	if(length(x) > 1)
	for( i in 2:length(x) ) {
		xi <- x[i]
		if(!any(duration(xi) == duration(u) &
			as.character(unit(xi)) == as.character(unit(u))))
			u <- c(u, xi)
	}
	return( u )
}
