# méthodes mixtes
#----------------
#' Operators to sum or substract POSIXctp from POSIXct or POSIXcti
#'
#' @name ops.posixctpi
#' @aliases Ops.POSIXcti Ops.POSIXctp +,POSIXct,POSIXctp-method Ops.POSIXst
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
#' @rdname ops.posixctpi
#' @aliases -,POSIXst,POSIXst-method
setMethod ('-', c('POSIXst', 'POSIXst'),
	function (e1, e2) {
		if( unit(e1) != unit(e2) )
			stop( "e1 and e2 must have the same 'unit' to be substracted" )
		if( of(e1) != of(e2) )
			stop( "e1 and e2 must have the same 'of' to be substracted" )
		if( timezone(e1) != timezone(e2) )
			stop( "e1 and e2 must have the same 'timezone' to be substracted" )

		if (length (e1) > length(e2)) e2 <- e2[rep(1:length(e2),
							   length.out=length(e1))]
		if (length (e2) > length(e1)) e1 <- rep(e1, length.out=length(e2))

		POSIXctp(e1@subtime - e2@subtime, rep(unit(e1), length(e1)))
	} )
#' @rdname ops.posixctpi
#' @aliases +,POSIXctp,POSIXst-method
setMethod ('+', c('POSIXctp', 'POSIXst'), function (e1, e2) e2 + e1)
#' @rdname ops.posixctpi
#' @aliases +,POSIXst,POSIXctp-method
setMethod ('+', c('POSIXst', 'POSIXctp'),
	function (e1, e2) {
		if( unit(e1) != unit(e2) )
			stop( "e1 and e2 must have the same 'unit' to be added together" )
		if (length (e1) > length(e2)) e2 <- e2[rep(1:length(e2),
							   length.out=length(e1))]
		if (length (e2) > length(e1)) e1 <- rep(e1, length.out=length(e2))
		e3 <- e1@subtime + e2@duration
		uc <- as.character(unit(e1))
		oc <- as.character(of(e1))
		if( uc=='year' ) {
			val.min <- -Inf
			val.max <- +Inf
		} else if( uc=='month' ) {
			val.min <- 0
			val.max <- 11
		} else if( uc=='day' & oc=='month' ) {
			val.min <- 1
			val.max <- 31
		} else {
			val.min <- 0
			bases <- switch(oc, year=c(60, 60, 24, 366),
					    month=c(60, 60, 24, 31),
					    week=c(60, 60, 24, 7),
					    day=c(60, 60, 24),
					    hour=c(60, 60),
					    minute=c(60))
			sb <- switch(uc, second=1, minute=2, hour=3, 4)
			val.max <- prod(bases[sb:length(bases)])-1
		}
		if(((uc=='day' & oc=='month' ) | (uc=='second') |
		     (uc=='day' | oc=='year')) && any( e3 > val.max) )
			warning(sprintf('Number of %s in a %s can vary. In the calculation, %.0f is used as modulo', uc, oc, val.max))
		sup <- e3 > val.max
		if( any(sup) ) e3[sup] <- e3[sup] %% (val.max+1) + val.min
		POSIXst(e3, uc, oc)
	} )
#' @rdname ops.posixctpi
#' @aliases -,POSIXst,POSIXctp-method
setMethod ('-', c('POSIXst', 'POSIXctp'),
	function (e1, e2) {
		if( unit(e1) != unit(e2) )
			stop( "e1 and e2 must have the same 'unit' to be added together" )
		if (length (e1) > length(e2)) e2 <- e2[rep(1:length(e2),
							   length.out=length(e1))]
		if (length (e2) > length(e1)) e1 <- rep(e1, length.out=length(e2))
		e3 <- e1@subtime - e2@duration
		uc <- as.character(unit(e1))
		oc <- as.character(of(e1))
		if( uc=='year' ) {
			val.min <- -Inf
			val.max <- +Inf
		} else if( uc=='month' ) {
			val.min <- 0
			val.max <- 11
		} else if( uc=='day' & oc=='month' ) {
			val.min <- 1
			val.max <- 31
		} else {
			val.min <- 0
			bases <- switch(oc, year=c(60, 60, 24, 366),
					    month=c(60, 60, 24, 31),
					    week=c(60, 60, 24, 7),
					    day=c(60, 60, 24),
					    hour=c(60, 60),
					    minute=c(60))
			sb <- switch(uc, second=1, minute=2, hour=3, 4)
			val.max <- prod(bases[sb:length(bases)])-1
		}
		if(((uc=='day' & oc=='month' ) | (uc=='second') |
		     (uc=='day' | oc=='year')) && any( e3 < val.min) )
			warning(sprintf('Number of %s in a %s can vary. In the calculation, %.0f is used as modulo', uc, oc, val.max))
		inf <- e3 < val.min
		if( any(inf) ) e3[inf] <- (e3[inf]-val.min) %% (val.max+1)
		POSIXst(e3, uc, oc)
	} )
