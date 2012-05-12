# definition de la classe
#------------------------
setClass (Class = 'TimeInstantDataFrame', 
	  representation = representation (instant='POSIXct', timezone='character', data='data.frame'),
	  prototype = prototype (instant=as.POSIXct(character(), timezone='UTC'),
				 data=data.frame()),
	  validity=function(object) {
		  if (length (when (object)) != nrow (object))
			  stop ("In a 'TimeInstantDataFrame, 'data' must have a number of rows as long as 'instant'.")
		  return (TRUE)
	  })

# constructeurs
#--------------
#' Create a TimeInstantDataFrame from scratch
#' 
#' To see all methods related to this class, 
#' see \code{\link{TimeInstantDataFrame-class}}
#'
#' If both \code{when} and \code{data}
#' are given, \code{data} must have a number of rows identical to the length of
#' \code{when}.
#'
#' To access to the class documentation, type in the R console :
#'
#' \code{class?TimeInstantDataFrame}
#'
#' @examples
#' TimeInstantDataFrame (
#'	c('2010-01-01', '2010-02-01'),
#' 	'UTC', data.frame(ex=1:2) )
#'
#' TimeInstantDataFrame (c('2010-01-01', '2010-02-01', '2010-02-02'), 'UTC')
#' 
#' @param when POSIXct or character representing a time with a valid
#' format (see \code{\link[base:as.POSIXct]{POSIXct}}).
#' It gives the instant of each row.
#' @param timezone character representing a valid timezone (see 
#' \code{\link[base]{timezone}}).
#' @param data a data.frame with as much rows as the length of \sQuote{when}.
#' Can be \code{NULL} (hence the data.frame has zero column and as much
#' rows as needed).
#' @param \dots arguments to or from other methods
#'
#' @return a \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}} object.
#' @seealso \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}},
#' \code{\link{RegularTimeInstantDataFrame}}, \code{\link{timetools}}
TimeInstantDataFrame <- function (when, timezone='UTC', data=NULL, ...) {
	if (is.character (when) ) when <- as.POSIXct (when, timezone)
	if (is.null (data)) data <- data.frame (matrix (NA, ncol=0, nrow=length(when) ) )
	new ('TimeInstantDataFrame', instant=when, timezone=timezone, data=data)
}

#' Create a regular TimeInstantDataFrame from scratch
#' 
#' To see all methods related to this class, 
#' see \code{\link{TimeInstantDataFrame-class}}
#'
#' To access to the class documentation, type in the R console :
#'
#' \code{class?TimeInstantDataFrame}
#'
#' @param from POSIXct or character representing a time with a valid
#' format (see \code{\link[base:as.POSIXct]{POSIXct}}). It represents
#' the start of the object.
#' @param to POSIXct or character representing a time with a valid
#' format (see \code{\link[base:as.POSIXct]{POSIXct}}). It represents
#' the end of the object.
#' If missing, its value is deduced from \sQuote{from}, \sQuote{by} 
#' and \sQuote{data}.
#' @param by a \code{\link{POSIXctp}} object indicating the
#' increment to use between instants of the object.
#' @param data a data.frame with a number of rows the fit with the 
#' number of intervals created by the function. If \code{NULL}, the 
#' data slot will be a data.frame with zero column.
#' @inheritParams TimeInstantDataFrame
#'
#' @return a \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}} object.
#' @seealso \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}},
#' \code{\link{TimeInstantDataFrame}}, \code{\link{timetools}}
RegularTimeInstantDataFrame <- function (from, to, by, timezone='UTC', data=NULL) {
	if (is.character (from) ) from <- as.POSIXct (from, timezone)
	if (is.character (by) ) by <- POSIXctp(unit=by)
	if (missing (to))
		to <- from + (nrow(data) - 1) * by
	if (is.character (to) ) to <- as.POSIXct (to, timezone)
	if (!inherits (by, 'POSIXctp') )
		stop ("'by' should be coercible to a 'POSIXctp'.")

	if (as.character(unit(by)) == 'year') {
		nb <- year(to) - year(from) + 
			ifelse(second(to, of='year') == 0, 0, 1)
	} else if (as.character(unit(by)) == 'month') {
		nb <- (year(to) - year(from))*12 + month(to) - month(from) + 
			ifelse(second(to, of='month') == 0, 0, 1)
	} else {
		u <- switch (as.character(unit(by)), second='secs', minute='mins',
						     hour='hours', day='days')
		nb <- as.numeric (difftime(to, from, units=u))
		nb <- ceiling (nb/duration(by))
	}
	when <- from + 0:nb * by
	tk <- !is.na(when) & (when >= from & when <= to) 
	when <- when[tk]

	if (is.null (data)) data <- data.frame (matrix (NA, ncol=0, nrow=length(when) ) )
	new ('TimeInstantDataFrame', instant=when, timezone=timezone, data=data)
}

# definition des accesseurs de l'objet
#-------------------------------------

#' @rdname time.properties
#' @aliases when,TimeInstantDataFrame-method
setMethod (f='when', signature='TimeInstantDataFrame',
	   definition=function(x, ...) return(x@instant) )
#' @rdname time.properties
#' @aliases timezone,TimeInstantDataFrame-method
setMethod (f='timezone', signature='TimeInstantDataFrame',
	   definition=function(object) return(object@timezone) )
#' @rdname time.properties
#' @aliases timezone<-,TimeInstantDataFrame-method
setMethod (f='timezone<-', signature='TimeInstantDataFrame',
		  definition=function(object, value) {
			object@timezone <- value
			object@instant <- as.POSIXct (as.POSIXlt (object@instant, value) )
			return(object)
		} )

# mise en forme pour / et affichage
#----------------------------------
print.TimeInstantDataFrame <- function (x, tz=NULL, ...) {
	if (is.null (tz) ) tz <- timezone(x)
	print(data.frame (when=format (when(x), tz=tz, usetz=TRUE), x@data) )
}
setMethod ('show', 'TimeInstantDataFrame',
	   function (object) print (object, timezone(object))
)
		   #                    print(data.frame (when=when(object), object@data) ), tz=timezone(object))
tail.TimeInstantDataFrame <- function (x, tz, ...) {
	if (missing (tz) ) tz <- x@timezone
	tail(data.frame (when=format (when(x), tz=tz, usetz=TRUE), x@data), ...)
}
head.TimeInstantDataFrame <- function (x, tz, ...) {
	if (missing (tz) ) tz <- x@timezone
	head(data.frame (when=format (when(x), tz=tz, usetz=TRUE), x@data), ...)
}
summary.TimeInstantDataFrame <- function (object, ...)
		summary(data.frame (when=when(object), object@data), ...)
# format

# defintion des accesseurs aux donnees
#-------------------------------------
'[.TimeInstantDataFrame' <- function(x, i, j, drop=FALSE) {
	n.args <- nargs() - hasArg(drop)
	if (missing (j) & n.args==2) {
		j <- i
		i <- seq_len(nrow(x))
	}
	if(missing(i)) i <- seq_len(nrow(x))
	y <- new ('TimeInstantDataFrame', 
	     instant = when(x)[i, drop=drop],
	     data = x@data[i, j, drop=drop])
	validObject(y)
	return(y)
}
setMethod (f='[[', signature='TimeInstantDataFrame',
	   definition=function(x, i, ...) {
		   '[[.data.frame'(x@data, i, ...)
	   })
setMethod (f='$', signature='TimeInstantDataFrame',
	   definition=function(x, name) {
		   do.call ('$', list(x=x@data, name=name))
	   })

'[<-.TimeInstantDataFrame' <- function(x, i, j, value) {
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
		x@instant <- when(x)[available.rows]
	}
	row.names(tmp) <- rn
	x@data <- tmp
	validObject(x)
	return(x)
}
'[[<-.TimeInstantDataFrame' <- function(x, i, j, value) {
   # les ids servent a voir si la data.frame a evoluer
   #	en nombre de lignes
   ids <- sprintf ('ID%i', 1:nrow (x@data))
   rn <- row.names(x@data)
   tmp <- x@data
   row.names(tmp) <- ids
   if (missing (j) )
	   tmp[[i]] <- value else
	   tmp[[i,j]] <- value
		   #            tmp <- '[[<-.data.frame'(tmp, i, j, value)
   if(!all(available.rows <- row.names (tmp) %in% ids ) ){
	   x@instant <- when(x)[available.rows]
   }
   row.names(tmp) <- rn
   x@data <- tmp
   validObject(x)
   return(x)
}
setMethod (f='$<-', signature='TimeInstantDataFrame',
	   definition=function(x, name, value) {
		   x@data <- "$<-.data.frame"(x@data, name, value)
		   validObject(x)
		   return(x)
	   })

setMethod (f='dim', signature='TimeInstantDataFrame',
	   definition=function(x) dim (x@data))
setMethod (f='length', signature='TimeInstantDataFrame',
	   definition=function(x) length (x@data))
setMethod (f='nrow', signature='TimeInstantDataFrame',
	   definition=function(x) nrow (x@data))
setMethod (f='ncol', signature='TimeInstantDataFrame',
	   definition=function(x) ncol (x@data))
row.names.TimeInstantDataFrame <- function(x) row.names (x@data)
'row.names<-.TimeInstantDataFrame' <- function(x, value) {
		   row.names (x@data) <- value
		   x
	   }
setMethod (f='names', signature='TimeInstantDataFrame',
	   definition=function(x) names (x@data))
setMethod (f='names<-', signature='TimeInstantDataFrame',
	   definition=function(x, value) {
		   names (x@data) <- value
		   x
	   } )

# Ops
# Math

# manipulation
#-------------
split.TimeInstantDataFrame <- function(x, f, drop=FALSE, ...) {
		   vect <- seq_len(nrow(x))
		   i <- split (when(x), f, drop)
		   data <- split (x@data, f, drop)
		   mapply (SIMPLIFY=FALSE, new, 'TimeInstantDataFrame',
			   instant=i, data=data)
	   }

# fonction réalisée en S3 pour ne pas imposer de 'signature'
rbind.TimeInstantDataFrame <- function (...) {
	dots <- list (...)
	names(dots) <- NULL
	if (!all (sapply (dots, inherits, 'TimeInstantDataFrame')))
		stop ("all arguments must be 'TimeInstantDataFrame'")
	instant <- as.POSIXct (unlist (lapply (dots, when) ), origin=timetools::origin)
	df <- do.call("rbind", lapply(dots, function(x) x@data) )
	tz <- timezone (dots[[1]])
	if (!all (tz == sapply (dots, timezone)))
		warning ("Not all timezone are identical. Timezone of the first object is used.")
	new('TimeInstantDataFrame', instant=instant, timezone=timezone (dots[[1]]), data=df)
}
# cbind # a faire eventuellement entre un Time*DataFrame et une data.frame
merge.TimeInstantDataFrame <- function(x, y, by, all=TRUE, tz='UTC', ...) {
		instant.vec <- list (when(x), when(y))
		x.data <- data.frame (instant=format (when(x), format='%Y-%m-%d %H:%M:%S', tz='UTC'),
				      x@data)
		y.data <- data.frame (instant=format (when(y), format='%Y-%m-%d %H:%M:%S', tz='UTC'),
				      y@data)
		z <- merge (x.data, y.data, by=unique (c('instant', by) ), all=all, ...)
		z <- new ('TimeInstantDataFrame',
		     instant=as.POSIXct(z$instant, tz='UTC'),
		     data=z[setdiff(names(z), c('instant'))])
		timezone(z) <- tz
		return (z)
	   }

setMethod ('lapply', signature('TimeInstantDataFrame', 'ANY'),
	   function (X, FUN, ...)
	   {
		   res <- lapply (data.frame(X), FUN, ...)
		   if (all (sapply (res, length) == nrow(X))) {
			   X@data <- data.frame (res[names(X)])
		   } else if (all (sapply (res, length) == 1)) {
			   X <- new ('TimeIntervalDataFrame',
				     start=min(when(X)), end=max(when(X)), timezone=timezone(X),
				     data=data.frame (res))
		   } else {
			   stop ("try to apply inadequate function over SubtimeDataFrame.")
		   }
		   return (X)
	   } )

# acces/modification de certaines propriétés
#-------------------------------------------
#' @rdname time.properties
#' @aliases regular,TimeInstantDataFrame-method
setMethod (f='regular', signature='TimeInstantDataFrame',
	   definition=function(x, ...) {
		   len <- length(unique(difftime(start(x)[-1], start(x)[-nrow(x)])))
		   return(length(len) == 1)
	   })

# transformateur de classe
#-------------------------
setAs ('TimeInstantDataFrame', 'data.frame',
       function(from) data.frame (instant=when(from), from@data) )

as.data.frame.TimeInstantDataFrame <- function (x, row.names=NULL, optional=FALSE, include.dates=FALSE, ...) {
	if (include.dates)
		return (data.frame (date=when (x), x@data) ) else
		return (x@data)
}

#' @rdname as.TimeIntervalDataFrame
#' @usage 
#' \method{as.TimeIntervalDataFrame}{TimeInstantDataFrame}(from, period, ...)
#' 
#' @param period \code{\link{POSIXctp}} object indicating
#' 	the period to add to \sQuote{when} slot of \code{from}
#' 	to determine the end of the new period (the \sQuote{when}
#' 	is used for the start of period)
as.TimeIntervalDataFrame.TimeInstantDataFrame <- function(from, period, ...) {
	if (missing(period))
	{
		if (regular(from))
			period <- as.numeric(difftime (when(from)[2] - when(from)[1], units='secs')) else
			stop ("'period' must be of class 'period' or 'from' should be at least 'regular'.")
		period <- POSIXctp (period, 'second')
	}

	to <- new ('TimeIntervalDataFrame', start=when(from), end=when(from)+period, 
		   timezone=timezone(from), data=from@data)
	validObject(to)
	return (to)
}

#' @rdname as.SubtimeDataFrame
#' @usage 
#' \method{as.SubtimeDataFrame}{TimeInstantDataFrame}(from, representation, FUN=mean, ..., first.day=0)
#'
#' @section TimeIntervalDataFrame:
#' If \sQuote{from} is a \code{\link{TimeInstantDataFrame}},
#' data must be agregated befor conversion. The function to use
#' is specified by \sQuote{FUN}. The default function is \code{\link[base]{mean}}.
#' 
#' @inheritParams subtime
#' @param FUN function to use for the agregation (see \sQuote{details})
as.SubtimeDataFrame.TimeInstantDataFrame <- function(from, representation, FUN=mean, ..., first.day=0)
{
	st <- subtime(from, representation, first.day=first.day)
	to <- split (data.frame (from), st)
	st <- factor (names(to), levels=levels (st), ordered=TRUE)
	attributes(st)$timezone <- timezone (from)
	class (st) <- c('subtime', 'factor')
	to <- t(data.frame (lapply (to, sapply, FUN, ...)))
	rownames (to) <- NULL

	to <- new ('SubtimeDataFrame', when=st, data=data.frame (to))
	validObject(to)
	return (to)
}
