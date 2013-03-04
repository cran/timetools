# definition de la classe
#------------------------
setClass (Class = 'TimeIntervalDataFrame', 
	  representation = representation (start='POSIXct', end='POSIXct',
					   timezone='character', data='data.frame'),
	  prototype = prototype (start=as.POSIXct(character(), tz='UTC'),
				 end=as.POSIXct(character(), tz='UTC'),
				 timezone='UTC',
				 data=data.frame()),
	  validity=function(object) {
		  if (length (object@start) != length (object@end))
			  stop ("'start' and 'end' must have the same length in a 'TimeIntervalDataFrame'.")
		  if (length (object@end) != nrow (object@data))
			  stop ("In a 'TimeIntervalDataFrame, 'data' must have a number of rows as long as 'start' and 'end'.")
		  return (TRUE)
	  } )

# constructeurs
#--------------
#' Create a TimeIntervalDataFrame from scratch
#' 
#' To see all methods related to this class, 
#' see \code{\link{TimeIntervalDataFrame-class}}
#'
#' If both \code{start} and \code{end} are given, they must have the same
#' length. They are used to define the intervals of the object. If \code{data}
#' is also given, it must have a number of rows identical to the length of
#' \code{start} and \code{end}.
#'
#' If only \code{start} is given, a continuous (see \code{\link{continuous}})
#' TimeIntervalDataFrame is built. The first element of \code{start} is the start
#' of the first interval, the second element is the end of the first interval 
#' and the start of the second interval. The last element of \code{start} is
#' only the end of the last interval. This is why \code{data}, if given,
#' must be one row shorter than \code{start}.
#'
#' If period is given it must be a \code{\link{POSIXctp}} object
#' (or a valid character) and \sQuote{start}
#' and \sQuote{end} must have length equal to 1. In that case, a
#' TimeIntervalDataFrame will be created with start date equal to start
#' \sQuote{floored} by the unit of \sQuote{period}, end date \sQuote{ceiled}
#' by the unit of \sQuote{period} and with enough intervals of \sQuote{period}
#' length to fit. If \sQuote{data} given, it must have a number of rows 
#' equal to the number of intervals calculated.
#' 
#' To access to the class documentation, type in the R console :
#'
#' \code{class?TimeIntervalDataFrame}
#'
#' @examples
#' TimeIntervalDataFrame (
#'	c('2010-01-01', '2010-02-01'), c('2010-02-01', '2010-02-02'),
#' 	'UTC', data.frame(ex=1:2) )
#'
#' TimeIntervalDataFrame (
#'	c('2010-01-01', '2010-02-01', '2010-02-02'), NULL,
#' 	'UTC', data.frame(ex=1:2) )
#'
#' TimeIntervalDataFrame(
#' 	as.POSIXct('2013-01-01'), as.POSIXct('2013-03-01'), 
#'	period=POSIXctp('day')
#' )
#' 
#' @param start POSIXct or character representing a time with a valid
#' format (see \code{\link[base:as.POSIXct]{POSIXct}}).
#' It gives the begining of each interval.
#' @param end  POSIXct or character representing a time with a valid
#' format (see \code{\link[base:as.POSIXct]{POSIXct}}).
#' It gives the end of each interval. If NULL, see \sQuote{Details}.
#' @param timezone character representing a valid timezone (see 
#' \code{\link[base]{timezone}}).
#' @param data a data.frame with as much rows as the length of \sQuote{start}
#' and end, or with one row less than the length of \sQuote{start} if \sQuote{end} is
#' \code{NULL}. Can be \code{NULL} (hence the data.frame has zero column and as much
#' rows as needed).
#' @param period if not NULL, a \code{\link{POSIXctp}} or a character that 
#' can be converted to a \code{\link{POSIXctp}} (see argument \sQuote{unit} of
#' POSIXctp function). See Details to know how to use this argument.
#' @param \dots arguments to or from other methods
#'
#' @return a \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}} object.
#' @seealso \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}},
#' \code{\link{RegularTimeIntervalDataFrame}}, \code{\link{timetools}}
TimeIntervalDataFrame <- function (start, end=NULL, timezone='UTC', data=NULL, period=NULL, ...) {
	# cas avec period

	if(!is.null(period)) {
		if(length(start) != 1 & length(end) != 1)
			stop("both 'start' and 'end' arguments must have a length of 1.")
			
		if (length (period) > 1) {
			warning ('Only the first given period is used as \'to\'.')
			period <- period[1]
		}

		if( is.character(start) ) start <- as.POSIXct (start, timezone)
		if( is.character(end) ) end <- as.POSIXct (end, timezone)
		if( is.character(period) ) period <- POSIXctp( period )
		tz <- if(is.null(timezone)) attributes(start)$tzone[1] else timezone

		# construction de ls structure qui va servir 

		u <- as.character (unit(period))
		if (u == 'second') {
			s <- trunc (start, 'secs')
		} else if (u == 'minute') {
			s <- trunc (start, 'mins')
		} else if (u == 'hour') {
			s <- trunc (start, 'hours')
		} else if (u == 'day') {
			s <- trunc (start, 'days')
		} else if (u == 'month') {
			s <- as.POSIXct(
				sprintf('%s-01', format(start, '%Y-%m')),
				tz)
		} else if (u == 'year') {
			s <- as.POSIXct(
				sprintf('%s-01-01', format(start, '%Y')),
				tz)
		}
		
		s <- as.POSIXct(s)
		
		if (u == 'year') {
			e <- end
			nb <- year(e) - year(s) +
			ifelse(second(e, of='year') == 0, 0, 1)
		} else if (u == 'month') {
			e <- end
			nb <- (year(e) - year(s))*12 +
				as.numeric(month(e)) -
				as.numeric(month(s)) +
			ifelse(second(e, of='month') == 0, 0, 1)
		} else {
			u <- switch (u, second='secs', minute='mins',
				     hour='hours', day='days')
			nb <- as.numeric (difftime(end, s, units=u))
			nb <- ceiling (nb/duration(period))
		}
		
		e <- s+as.numeric(nb) * period
		
		result <- RegularTimeIntervalDataFrame(
			s, e, by=period, timezone=tz)
		return( result )
	}

	# cas classique

	if (is.null (end) ) {
		end <- start[-1]
		start <- start[-length(start)]
	}
	if (is.character (start) ) start <- as.POSIXct (start, timezone)
	if (is.character (end) ) end <- as.POSIXct (end, timezone)
	if (is.null (data)) data <- data.frame (matrix (NA, ncol=0, nrow=length(start) ) )
	new ('TimeIntervalDataFrame', start=start, end=end,
	     timezone=timezone, data=data)
}

#' Create a regular TimeIntervalDataFrame from scratch
#' 
#' To see all methods related to this class, 
#' see \code{\link{TimeIntervalDataFrame-class}}
#'
#' To access to the class documentation, type in the R console :
#'
#' \code{class?TimeIntervalDataFrame}
#'
#' @param from POSIXct or character representing a time with a valid
#' format (see \code{\link[base:as.POSIXct]{POSIXct}}). It represents
#' the start of the object (and of its first interval).
#' @param to POSIXct or character representing a time with a valid
#' format (see \code{\link[base:as.POSIXct]{POSIXct}}). It represents
#' the end of the object : the end of the last interval is as near
#'  as possible from this date, but before it.
#' If missing, its value is deduced from \sQuote{from}, \sQuote{by} 
#' and \sQuote{data}.
#' @param by a \code{\link{POSIXctp}} object indicating the
#' increment to use between the start of each interval
#' @param period a \code{\link{POSIXctp}} object indicating the
#' period of each interval. If missing, it is given the value of \code{by}.
#' @param data a data.frame with a number of rows the fit with the 
#' number of intervals created by the function. If \code{NULL}, the 
#' data slot will be a data.frame with zero column.
#' @inheritParams TimeIntervalDataFrame
#'
#' @return a \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}} object.
#' @seealso \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}},
#' \code{\link{TimeIntervalDataFrame}}, \code{\link{timetools}}
RegularTimeIntervalDataFrame <- function (from, to, by, period, timezone='UTC', data=NULL) {
	if (is.character (from) ) from <- as.POSIXct (from, timezone)
	if (is.character (by) ) by <- POSIXctp(unit=by)
	if (missing (to))
		to <- from + (nrow(data) - 1) * by
	if (is.character (to) ) to <- as.POSIXct (to, timezone)
	if (!inherits (by, 'POSIXctp') )
		stop ("'by' should be coercible to a 'POSIXctp'.")
	if (missing (period) ) {
		period <- by
	} else if (is.character (period) ) {
		period <- POSIXctp(unit=period)
	}
	if (!inherits (period, 'POSIXctp') )
		stop ("'period' should be coercible to a 'POSIXctp'.")

	if (as.character(unit(by)) == 'year') {
		nb <- year(to) - year(from) +
			ifelse(second(to, of='year') == 0, 0, 1)
	} else if (as.character(unit(by)) == 'month') {
		nb <- (year(to) - year(from))*12 + as.numeric(month(to)) - as.numeric(month(from)) +
			ifelse(second(to, of='month') == 0, 0, 1)
	} else {
		u <- switch (as.character(unit(by)), second='secs', minute='mins',
						     hour='hours', day='days')
		nb <- as.numeric (difftime(to, from, units=u))
		nb <- ceiling (nb/duration(by))
	}
	start <- from + 0:(nb-1) * by
	end <- start + period
	tk <- !is.na(start) & !is.na(end) &
		((start >= from & start <= to) | (start >= from & end <= to))
	start <- start[tk]
	end <- end[tk]

	if (is.null (data)) data <- data.frame (matrix (NA, ncol=0, nrow=length(start) ) )
	new ('TimeIntervalDataFrame', start=start, end=end,
	     timezone=timezone, data=data)
}

# definition des accesseurs de l'objet
#-------------------------------------

start.TimeIntervalDataFrame <- function(x, ...) return(as.POSIXct(as.POSIXlt(x@start, timezone(x))))
end.TimeIntervalDataFrame <- function(x, ...) return(as.POSIXct(as.POSIXlt(x@end, timezone(x))))
#' @rdname time.properties
#' @aliases timezone,TimeIntervalDataFrame-method
setMethod (f='timezone', signature='TimeIntervalDataFrame',
	   definition=function(object) return(object@timezone[1]) )
#' @rdname time.properties
#' @aliases timezone<-,TimeIntervalDataFrame-method
setMethod (f='timezone<-', signature='TimeIntervalDataFrame',
		  definition=function(object, value) {
	start <- as.POSIXct (as.POSIXlt (object@start, value) )
	end <- as.POSIXct (as.POSIXlt (object@end, value) )
	new ('TimeIntervalDataFrame', start=start, end=end,
	     timezone=value, data=data.frame (object) )
} )
#' @rdname time.properties
#' @aliases interval,TimeIntervalDataFrame-method
setMethod (f='interval', signature='TimeIntervalDataFrame',
	   definition=function(x, ...) return(POSIXcti (start(x), end(x)) ) )
#' @rdname time.properties
#' @aliases when,TimeIntervalDataFrame-method
setMethod (f='when', signature='TimeIntervalDataFrame',
	   definition=function(x, ...) return(POSIXcti (start(x), end(x)) ) )
#' @rdname time.properties
#' @aliases period,TimeIntervalDataFrame-method
setMethod (f='period', signature='TimeIntervalDataFrame',
	   definition=function(x, ...) {
		   if (!homogeneous(x)) stop ("x should be homogeneous to have a 'period'")
		   if (!continuous(x)) stop ("x should be continuous to have a 'period'")
		   res <- difftime(end(x), start(x), units='secs')
		   res <- POSIXctp (unique(res), unit=rep ('second', length(unique(res))))
		   return (res)
	   } )

# mise en forme pour / et affichage
#----------------------------------
print.TimeIntervalDataFrame <- function (x, tz=NULL, ...) {
	if (is.null (tz) ) tz <- timezone(x)
	print(data.frame (start=format (start(x), tz=tz, usetz=TRUE),
			 end=format (end(x), tz=tz, usetz=TRUE),
			 x@data), ...)
}
setMethod ('show', 'TimeIntervalDataFrame',
	   function (object)
		   print (object, timezone(object)) )
tail.TimeIntervalDataFrame <- function (x, tz, ...) {
	if (missing (tz) ) tz <- x@timezone
	tail(data.frame (start=format (start(x), tz=tz, usetz=TRUE),
			 end=format (end(x), tz=tz, usetz=TRUE),
			 x@data), ...)
}
head.TimeIntervalDataFrame <- function (x, tz, ...) {
	if (missing (tz) ) tz <- x@timezone
	head(data.frame (start=format (start(x), tz=tz, usetz=TRUE),
			 end=format (end(x), tz=tz, usetz=TRUE),
			 x@data), ...)
}
summary.TimeIntervalDataFrame <- function (object, ...)
	summary(data.frame (start=start(object), end=end(object), object@data), ...)
# format

# defintion des accesseurs aux donnees
#-------------------------------------
'[.TimeIntervalDataFrame' <- function(x, i, j, drop=FALSE) {
	n.args <- nargs() - hasArg(drop)
	if (missing (j) & n.args==2) {
		j <- i
		i <- seq_len(nrow(x))
	}
	if(missing(i)) i <- seq_len(nrow(x))
	y <- new ('TimeIntervalDataFrame', 
	     start = start(x)[i, drop=drop],
	     end = end(x)[i, drop=drop],
	     timezone = x@timezone,
	     data = x@data[i, j, drop=drop])
	validObject(y)
	return(y)
}
setMethod (f='[[', signature='TimeIntervalDataFrame',
	   definition=function(x, i, ...) {
		   '[[.data.frame'(x@data, i, ...)
	   })
setMethod (f='$', signature='TimeIntervalDataFrame',
	   definition=function(x, name) {
		   do.call ('$', list(x=x@data, name=name))
	   })

'[<-.TimeIntervalDataFrame' <- function(x, i, j, value) {
	n.args <- nargs()
	if (missing (j) & n.args==3) {
		j <- i
		i <- seq_len(nrow(x))
	}
	if(missing(i)) i <- seq_len(nrow(x))
	# les ids servent a voir si la data.frame a evoluer
	#	en nombre de lignes
	ids <- sprintf ('ID%i', 1:nrow (x))
	rn <- row.names(x)
	tmp <- x@data
	row.names(tmp) <- ids
	tmp[i, j] <- value
	if(!all (available.rows <- row.names (tmp) %in% ids) ){
		x@start <- x@start[available.rows]
		x@end <- x@end[available.rows]
	}
	row.names(tmp) <- rn
	x@data <- tmp
	validObject(x)
	return(x)
}
'[[<-.TimeIntervalDataFrame' <- function(x, i, j, value) {
   # les ids servent a voir si la data.frame a evoluer
   #	en nombre de lignes
   ids <- sprintf ('ID%i', 1:nrow (x@data))
   rn <- row.names(x@data)
   tmp <- x@data
   row.names(tmp) <- ids
   if (missing (j) )
	   tmp[[i]] <- value else
	   tmp[[i,j]] <- value
   #    tmp <- '[[<-'(tmp, i, j, value)
   if(!all(available.rows <- row.names (tmp) %in% ids ) ){
	   x@start <- x@start[available.rows]
	   x@end <- x@end[available.rows]
   }
   row.names(tmp) <- rn
   x@data <- tmp
   validObject(x)
   return(x)
}
setMethod (f='$<-', signature='TimeIntervalDataFrame',
	   definition=function(x, name, value) {
		   x@data <- "$<-.data.frame"(x@data, name, value)
		   validObject(x)
		   return(x)
	   })

setMethod (f='dim', signature='TimeIntervalDataFrame',
	   definition=function(x) dim (x@data))
setMethod (f='length', signature='TimeIntervalDataFrame',
	   definition=function(x) length (x@data))
setMethod (f='nrow', signature='TimeIntervalDataFrame',
	   definition=function(x) nrow (x@data))
setMethod (f='ncol', signature='TimeIntervalDataFrame',
	   definition=function(x) ncol (x@data))
row.names.TimeIntervalDataFrame <- function(x) row.names (x@data)
'row.names<-.TimeIntervalDataFrame' <- function(x, value) {
		   row.names (x@data) <- value
		   x
	   }
setMethod (f='names', signature='TimeIntervalDataFrame',
	   definition=function(x) names (x@data))
setMethod (f='names<-', signature='TimeIntervalDataFrame',
	   definition=function(x, value) {
		   names (x@data) <- value
		   x
	   } )

# Math

# manipulation
#-------------
# fonction réalisée en S3 pour ne pas imposer de 'signature'
rbind.TimeIntervalDataFrame <- function (...)
{
	dots <- list (...)
	names(dots) <- NULL
	if (!all (sapply (dots, inherits, 'TimeIntervalDataFrame')))
		stop ("all arguments must be 'TimeIntervalDataFrame'")
	start <- as.POSIXct (unlist (lapply (dots, start) ), origin=timetools::origin)
	end <- as.POSIXct (unlist (lapply (dots, end) ), origin=timetools::origin)
	df <- do.call("rbind", lapply(dots, function(x) x@data) )
	tz <- timezone (dots[[1]])
	if (!all (tz == sapply (dots, timezone)))
		warning ("Not all timezone are identical. Timezone of the first object is used.")
	new('TimeIntervalDataFrame', start=start, end=end,
	     timezone=tz, data=df)
}
# cbind # a faire eventuellement entre un Time*DataFrame et une data.frame
merge.TimeIntervalDataFrame <- function(x, y, by, all=TRUE, tz='UTC', ...) {
	if (!inherits(y, 'TimeIntervalDataFrame'))
		stop ("'y' must be a 'TimeIntervalDataFrame'.")
	if (missing (by) ) by <- intersect (names (x), names(y))
	start.vec <- list (start(x), start(y))
	end.vec <- list (end(x), end(y))
	x.data <- data.frame (start=format (start(x),
					    format='%Y-%m-%d %H:%M:%S',
					    tz='UTC'),
			      end=format (end(x),
					  format='%Y-%m-%d %H:%M:%S',
					  tz='UTC'),
			      x@data)
	y.data <- data.frame (start=format (start(y),
					    format='%Y-%m-%d %H:%M:%S',
					    tz='UTC'),
			      end=format (end(y),
					  format='%Y-%m-%d %H:%M:%S',
					  tz='UTC'),
			      y@data)
	z <- merge (x.data, y.data, by=unique (c('start', 'end', by) ), all=all, ...)
	z <- new ('TimeIntervalDataFrame',
	     start=as.POSIXct(z$start, tz='UTC'),
	     end=as.POSIXct(z$end, tz='UTC'),
	     timezone='UTC',
	     data=z[setdiff(names(z), c('start', 'end'))])
	timezone(z) <- tz
	return (z)
}

setMethod ('lapply', signature('TimeIntervalDataFrame', 'ANY'),
	   function (X, FUN, ...)
	   {
		   res <- lapply (data.frame(X), FUN, ...)
		   if (all (sapply (res, length) == nrow(X))) {
			   X@data <- data.frame (res[names(X)])
		   } else if (all (sapply (res, length) == 1)) {
			   X <- new ('TimeIntervalDataFrame',
				     start=min(start(X)), end=max(end(X)),
				     timezone=timezone(X),
				     data=data.frame (res))
		   } else {
			   stop ("try to apply inadequate function over TimeIntervalDataFrame.")
		   }
		   return (X)
	   } )
# acces/modification de certaines propriétés
#-------------------------------------------
# tous les intervalles sont de même durée
#' @rdname time.properties
#' @aliases homogeneous,TimeIntervalDataFrame-method
setMethod (f='homogeneous', signature='TimeIntervalDataFrame',
	   definition=function(x, ..) {
		   len <- length(unique(difftime(end(x), start(x))))
		   return(len == 1)
	   } )
# tous les intervalles sont de même durée et espacés d'une même période
#' @rdname time.properties
#' @aliases regular,TimeIntervalDataFrame-method
setMethod (f='regular', signature='TimeIntervalDataFrame',
	   definition=function(x, ...) {
		   len <- length(unique(difftime(start(x)[-1], start(x)[-nrow(x)])))
		   return(homogeneous(x) & length(len) == 1)
	   } )
# entre le début du premier intervalle et la fin du dernier, il n'y a pas de 'trous' ET
#	il n'y a pas de superposition entre deux intervalles
#' @rdname time.properties
#' @aliases continuous,TimeIntervalDataFrame-method
setMethod (f='continuous', signature='TimeIntervalDataFrame',
	   definition=function(x, ...) {
		   start <- start(x)
		   end <- end(x)
		   ordre <- order (start, end)
		   start <- start[ordre]
		   end <- end[ordre]
		   return(all (start[-1] == end[-nrow(x)]) )
	   } )
#' @rdname time.properties
#' @aliases continuous<-,TimeIntervalDataFrame-method
setMethod (f='continuous<-', signature='TimeIntervalDataFrame',
	   definition=function(x, value) {
		   if (!value) return (x)
		   if (overlapping (x) ) stop ("Can't make continuous a 'TimeIntervalDataFrame' with overlapping intervals.")
		   if (continuous (x) ) return (x)
		   data <-  as.data.frame (matrix(NA, nrow=nrow(x)-1, ncol=ncol(x) ) )
		   names (data) <- names (x)
		   complementaire <- new ('TimeIntervalDataFrame',
					  start=end(x)[-nrow(x)], end=start(x)[-1],
					  timezone=x@timezone,
					  data= data)
		   return (merge (x, complementaire, tz=timezone(x) ) )
	   } )
# certains intervalles se superposent-ils ?
#' @rdname time.properties
#' @aliases overlapping,TimeIntervalDataFrame-method
setMethod (f='overlapping', signature='TimeIntervalDataFrame',
	   definition=function(x, ...) {
		   ol <- .C ('overlapping_timeintervaldf',
				  as.integer(start(x)), as.integer(end(x)), as.integer(nrow(x)),
				  ol=integer(1),
				  NAOK=FALSE, PACKAGE='timetools')$ol
		   return (ol == 1)
	   } )


# transformateur de classe
#-------------------------
setAs ('TimeIntervalDataFrame', 'data.frame',
       function(from) data.frame (start=start(from), end=end(from), from@data) )
as.data.frame.TimeIntervalDataFrame <- function (x, row.names=NULL, optional=FALSE, include.dates=FALSE, ...) {
	if (include.dates)
		return (data.frame (start=start(x), end=end(x), x@data) ) else
		return (x@data)
}

#' @rdname as.TimeInstantDataFrame
#' @usage
#'
#' \method{as.TimeInstantDataFrame}{TimeIntervalDataFrame}(from, cursor = NULL, ...)
#'
#' @param cursor For TimeIntervalDataFrame, it indicates
#'	where the TimeInstant must be taken. If \code{0},
#' 	start of each intervals is taken as instant ;
#' 	if \code{1} end of each intervals is taken as instant.
#' 	Any other value will determine a weigthed instant 
#' 	between start and end (actually, value higher than 1 or 
#' 	lower than 0 will give instant outside this range).
as.TimeInstantDataFrame.TimeIntervalDataFrame <- function(from, cursor=NULL, ...) {
	if (is.null (cursor) ) cursor <- 0.5
	if (cursor > 1 || cursor < 0) warning ("For a standard use, cursor should be between 0 and 1.")
	instant <- mapply (function(x, y, wx, wy)
				weighted.mean (c(x, y), c(wx, wy), na.rm=TRUE),
			   start(from), end(from), 1-cursor, cursor)
	instant <- as.POSIXct(instant, origin=timetools::origin)
	to <- new ('TimeInstantDataFrame', instant=instant,
		   timezone=timezone(from), data=from@data)
	validObject(to)
	return (to)
}

#' @rdname as.SubtimeDataFrame
#' @usage 
#' \method{as.SubtimeDataFrame}{TimeIntervalDataFrame}(x, unit, of, FUN=NULL, cursor=NULL, ...)
#'
#' @section TimeIntervalDataFrame:
#' If \sQuote{from} is a \code{\link{TimeIntervalDataFrame}},
#' data is first converted to a TimeInstantDataFrame (see \code{\link{as.TimeInstantDataFrame}}).
#' Then, this TimeInstantDataFrame is converted to a SubtimeDataFrame (see the appropriated section).
#' 
#' @inheritParams as.TimeInstantDataFrame.TimeIntervalDataFrame
as.SubtimeDataFrame.TimeIntervalDataFrame <-
	function(x, unit, of, FUN=NULL, cursor=NULL, ...)
	as.SubtimeDataFrame (as.TimeInstantDataFrame (x, cursor), unit, of, FUN, ...)

