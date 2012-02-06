# méthodes mixtes
#----------------
#' Operators to sum or substract POSIXctp from POSIXct or POSIXcti
#'
#' @name ops.posixctpi
#' @aliases Ops.POSIXcti Ops.POSIXctp +,POSIXct,POSIXctp-method
setMethod ('+', c('POSIXct', 'POSIXctp'),
	function (e1, e2) {
		if (length (e1) > length(e2)) e2 <- e2[rep(1:length(e2), length.out=length(e1))]
		if (length (e2) > length(e1)) e1 <- rep(e1, length.out=length(e2))
		e3 <- as.POSIXlt (e1)
		effective.unit <- sapply (FUN=switch, as.character (unit(e2) ),
			year = 'year', month = 'mon', day = 'mday',
			hour = 'hour', minute = 'min', second = 'sec',
			stop ('e2 is a corrupted POSIXctd') )
		for (u in unique (effective.unit) ) {
			index <- effective.unit == u
			e3[[u]][index] <- e3[[u]][index] + duration (e2)[index]
		}
		return (as.POSIXct (e3) )
	})
#' @rdname ops.posixctpi
#' @aliases +,POSIXctp,POSIXct-method
setMethod ('+', c('POSIXctp', 'POSIXct'), function (e1, e2) e2 + e1)
#' @rdname ops.posixctpi
#' @aliases -,POSIXct,POSIXctp-method
setMethod ('-', c('POSIXct', 'POSIXctp'),
	function (e1, e2) {
		e3 <- as.POSIXlt (e1)
		effective.unit <- sapply (FUN=switch, as.character (unit(e2) ),
			year = 'year', month = 'mon', day = 'mday',
			hour = 'hour', minute = 'min', second = 'sec',
			stop ('e2 is a corrupted POSIXctp') )
		for (u in unique (effective.unit) ) {
			index <- effective.unit == u
			e3[[u]][index] <- e3[[u]][index] - duration (e2)[index]
		}
		return (as.POSIXct (e3) )
	})

#' @rdname ops.posixctpi
#' @aliases +,POSIXcti,POSIXctp-method
setMethod ('+', c('POSIXcti', 'POSIXctp'),
	function (e1, e2) return (POSIXcti(start(e1) + e2, end(e1) + e2)))
#' @rdname ops.posixctpi
#' @aliases +,POSIXctp,POSIXcti-method
setMethod ('+', c('POSIXctp', 'POSIXcti'), function (e1, e2) e2 + e1)
#' @rdname ops.posixctpi
#' @aliases -,POSIXcti,POSIXctp-method
setMethod ('-', c('POSIXcti', 'POSIXctp'),
	function (e1, e2) return (POSIXcti(start(e1) - e2, end(e1) - e2)))
#' @rdname ops.posixctpi
#' @aliases *,numeric,POSIXctp-method
setMethod ('*', c('numeric', 'POSIXctp'),
	function (e1, e2)
	{
		duration <- e1 * duration(e2)
		unit <- as.character(unit(e2))
		if (length(duration) != length(unit))
			unit <- rep (unit, length.out=length(duration))
		return (POSIXctp(duration=duration, unit=unit))
	} )
#' @rdname ops.posixctpi
#' @aliases +,POSIXctp,POSIXctp-method
setMethod ('+', c('POSIXctp', 'POSIXctp'),
	function (e1, e2)
	{
		if (unit (e1) != unit(e2))
			stop ("'e1' and 'e2' must have the same unit.")
		unit <- as.character(unit(e2))
		return (POSIXctp(duration=duration(e1) + duration(e2), unit=unit))
	} )

