#' Divide into Groups and Reassemble (Time*DataFrame objects)
#'
#' \sQuote{split} divides the data in the vector \sQuote{x} into the groups defined
#' by \sQuote{f}.  The replacement forms replace values corresponding to
#' such a division.
#' Here are listed \sQuote{split} methods defined for Time objects defined
#' in the timetools package (\code{\link{TimeIntervalDataFrame}},
#' \code{\link{TimeInstantDataFrame}}, \code{\link{POSIXst}},
#' \code{\link{POSIXcti}}, etc.).
#' See sections below for complete list of methods. 
#'
#' For each new split method defined in \code{\link{timetools}} a 
#' short description is given there. If the method is an S3 method,
#' the class of the first argument only is given ; otherwise the complete
#' signature is given (for S4 methods).
#' 
#' @return
#' The value returned from ‘split’ is a list of vectors containing
#' the values for the groups.  The components of the list are named
#' by the levels of ‘f’ (after converting to a factor).
#'
#' @inheritParams base::split
#' @param \dots further potential arguments passed to methods.
#'
#' @rdname split
#'
#' @seealso \code{\link[base]{split}}, \code{\link{TimeIntervalDataFrame-class}},
#' \code{\link{TimeInstantDataFrame-class}}, \code{\link{SubtimeDataFrame-class}},
#' \code{\link{changeSupport}},
#' \code{\link{POSIXcti-class}}, \code{\link{POSIXst-class}},
#' \code{\link{POSIXctp-class}}
setGeneric (name='split')

#' @rdname split
#' @method split POSIXctp
#' @section POSIXctp:
#' split POSIXctp objects.
split.POSIXctp <- function(x, f, drop=FALSE, ...)
{
	i <- seq_len(length(x))
	i <- split(i, f)
	lapply(i, function(i, x) x[i], x)	
}

#' @rdname split
#' @method split POSIXst
#' @section POSIXst:
#' split POSIXst objects.
split.POSIXst <- function(x, f, drop=FALSE, ...)
{
	i <- seq_len(length(x))
	i <- split(i, f)
	lapply(i, function(i, x) x[i], x)	
}

#' @rdname split
#' @method split POSIXcti
#' @section POSIXcti:
#' split POSIXcti objects.
split.POSIXcti <- function(x, f, drop=FALSE, ...)
{
	i <- seq_len(length(x))
	i <- split(i, f)
	lapply(i, function(i, x) x[i], x)	
}

#' @rdname split
#' @method split SubtimeDataFrame
#' @section SubtimeDataFrame:
#' split SubtimeDataFrame objects.
split.SubtimeDataFrame <- function(x, f, drop=FALSE, ...)
{
	vect <- seq_len(nrow(x))
	w <- split (when(x), f, drop)
	data <- split (x@data, f, drop)
	x <- mapply (SIMPLIFY=FALSE, new, 'SubtimeDataFrame',
		     when=w, data=data, USE.NAMES=FALSE)
	names( x ) <- names( data )
	x
}

#' @rdname split
#' @method split TimeInstantDataFrame
#' @section TimeInstantDataFrame:
#' split TimeInstantDataFrame objects.
split.TimeInstantDataFrame <- function(x, f, drop=FALSE, ...)
{
	vect <- seq_len(nrow(x))
	i <- split (when(x), f, drop)
	data <- split (x@data, f, drop)
	x <- mapply (SIMPLIFY=FALSE, new, 'TimeInstantDataFrame',
		     instant=i, data=data, timezone=timezone(x),
		     USE.NAMES=FALSE)
	names( x ) <- names( data )
	x
}

#' @rdname split
#' @method split TimeIntervalDataFrame
#' @section TimeIntervalDataFrame:
#' split TimeIntervalDataFrame objects.
split.TimeIntervalDataFrame <- function(x, f, drop=FALSE, ...)
{
	vect <- seq_len(nrow(x))
	s <- split (start(x), f, drop)
	e <- split (end(x), f, drop)
	data <- split (x@data, f, drop)
	x <- mapply( SIMPLIFY=FALSE, new, 'TimeIntervalDataFrame',
		     start=s, end=e, data=data, timezone=x@timezone,
		     USE.NAMES=FALSE)
	names( x ) <- names( data )
	x
}

#' @rdname split
#' @aliases split,ANY,POSIXst-method
#' @section signature(ANY, POSIXst):
#' split any objects over \code{\link{POSIXst}}.
setMethod('split', signature('ANY', 'POSIXst'),
	  function(x, f, ...)
	  {
		  f <- as.numeric(f)
		  split(x, f )
	  } )

#' @rdname split
#' @aliases split,TimeIntervalDataFrame,TimeIntervalDataFrame-method
#' 
#' @param split.x logical indicating if data in \sQuote{x} that are over
#' several intervals of 'f' must be 'cut' to fit to new intervals (TRUE) or
#' ignored (FALSE).
#' @param keep.f logical indicating if f values must be kept on the resulting list.
#'
#' @section signature(TimeIntervalDataFrame, TimeIntervalDataFrame):
#' split \code{\link{TimeIntervalDataFrame}}  over another
#' \code{\link{TimeIntervalDataFrame}}.
#' This methode actually act more or less like the function
#' \code{\link{changeSupport}}
#' with the split.from argument set to TRUE. The difference is the output : 
#' there the result is a list of TimeIntervalDataFrame whereas the result of 
#' changeSupport is a TimeIntervalDataFrame.
setMethod('split', signature=signature(x='TimeIntervalDataFrame',
				       f='TimeIntervalDataFrame'),
	definition=function(x, f, ...,
			    split.x=FALSE, keep.f=TRUE)
{
	if (timezone(x) != timezone (f))
	{
		warning("'x' and 'f' have a different timezone. The timezone of 'f' is taken for the result")
		timezone(x) <- timezone(f)
	}
	int.x <- when (x)
	int.f <- when (f)
	   
	s.x <- start (int.x)
	e.x <- end (int.x)
	s.f <- start (int.f)
	e.f <- end (int.f)
	   
	nb <- .C ('project_nb_intersections',
			as.integer(s.x), as.integer(e.x),
			as.integer(length(s.x)),
			as.integer(s.f), as.integer(e.f),
			as.integer(length(s.f)),
			nb=integer(1),
			NAOK=FALSE, PACKAGE='timetools')$nb

	if (nb > 0) {

		whiches <- .C ('project_pos_weight',
			as.integer(s.x), as.integer(e.x),
			as.integer(length(s.x)),
			as.integer(s.f), as.integer(e.f),
			as.integer(length(s.f)),
			pos.x=integer(nb), pos.f=integer(nb),
			weight=integer(nb),
			NAOK=FALSE, PACKAGE='timetools')[c('pos.x', 'pos.f')]
		whiches <- as.data.frame (whiches)

	} else {

		whiches <- data.frame (pos.x=numeric(), pos.f=numeric())

	}

	# si x.split = FALSE, on dégage les x qui intersectent plusieurs f
	# sinon on garde une trace de ceux qui sont "à cheval"

	if (!split.x) {
		whiches <- whiches[!whiches$pos.x %in%
			unique( whiches$pos.x[duplicated(whiches$pos.x)] ),]
		whiches$splitted <- rep(FALSE, nrow(whiches))
	} else {
		whiches$splitted <- whiches$pos.x %in%
			unique( whiches$pos.x[duplicated(whiches$pos.x)] )
	}
	whiches$s <- as.numeric(start(x)[whiches$pos.x])
	whiches$e <- as.numeric( end (x)[whiches$pos.x])

	## si merge.x = FALSE, on dégage les f avec plusieurs x
	#
	#if (!merge.x)
	#	whiches <- whiches[!whiches$pos.f %in%
	#		unique( whiches$pos.f[duplicated(whiches$pos.f)] ),]

	f.notempty <- sort(unique( whiches$pos.f ))
	whiches <- split (whiches, whiches$pos.f)
	start <- s.f[f.notempty]
	end <- e.f[f.notempty]
 
	# split effectif des donnees
	# pour chaque element du whiches (et donc de start et end)
	# on créé un TimeIntervalDataFrame

	result <- list()
	result[f.notempty] <- mapply(
		function(w, s, e, x) {

			sx <- w$s
			ex <- w$e
			if( any(w$splitted) ) {
				sx[w$splitted] <- sapply(sx[w$splitted], max, s)
				ex[w$splitted] <- sapply(ex[w$splitted], min, e)
			}
			sx <- as.POSIXct(sx, origin=origin)
			ex <- as.POSIXct(ex, origin=origin)

			TimeIntervalDataFrame(start=sx, end=ex,
					      timezone=timezone(x),
			      		      data=x@data[w$pos.x,,drop=FALSE])
		},
		whiches, as.numeric(start), as.numeric(end),
		MoreArgs=list(x), SIMPLIFY=FALSE)

	# pour les intervals de f pour lesquels aucune données dans x n'est
	# présente, des TItDFD vides sont insérés

	result[setdiff(1:nrow(f), f.notempty)] <- TimeIntervalDataFrame(
		as.POSIXct(character()),
		as.POSIXct(character()),
		data = x@data[0,,drop=FALSE])

	# finalement on remet les valeurs du f (si souhaitées)

	if( keep.f )
		for( n in names(f) )
		{
			# tests et manip pour gérer les problèmes en cas de 
			# noms identiques entre x et f
			new.n <- n
			i <- 1
			while( new.n %in% names(x) ) {
				new.n <- paste(n, i, sep='.')
				i <- i+1
			}
			if( new.n != n)
				warning(sprintf("'%s' in f renamed as '%s'",
						n, new.n))

			# ajout des valeurs de f dans result
			result <- mapply(function(x, i, value)
			       {
				       x@data[[i]] <- rep(value, nrow(x))
				       return( x )
	       		       },
			       x=result, value=f[[n]],
			       MoreArgs=list(i=new.n), SIMPLIFY=FALSE)
		}

	return( result )
} )

#' @rdname split
#' @aliases split,TimeIntervalDataFrame,POSIXctp-method
#' 
#' @section signature(TimeIntervalDataFrame, POSIXctp):
#' split a \code{\link{TimeIntervalDataFrame}} against 
#' regular time intervals with a period defined by f (a \code{\link{POSIXctp}}).

setMethod (
	'split',
	signature(x='TimeIntervalDataFrame', f='POSIXctp'),
	definition=function (x, f, ...,  split.x=FALSE)
{
	s <- min(start(x))
	e <- max(end(x))
	tzone <- timezone( x )
	f <- TimeIntervalDataFrame(s, e, period=f, timezone=tzone)

	split(x, f, ..., split.x=split.x)
} )

#' @rdname split
#' @aliases split,TimeIntervalDataFrame,POSIXcti-method
#' 
#' @section signature(TimeIntervalDataFrame, POSIXcti):
#' split \code{\link{TimeIntervalDataFrame}} against specified 
#' intervals f (\code{\link{POSIXcti}}).

setMethod('split',
	  signature=signature(x='TimeIntervalDataFrame', f='POSIXcti'),
  	  definition=function(x, f, ..., split.x=FALSE)
{
	tzone <- attributes(start(f))$tzone[1]
	f <- TimeIntervalDataFrame(start(f), end(f), tzone)

	split(x, f, ..., split.x=split.x)
} )

# pas sûr que celui-ci soit possible
# setMethod ('split', signature=signature(x='TimeIntervalDataFrame', f='SubtimeDataFrame', drop='ANY'),
# 	   definition=function(x, f, drop, ..., boundaries=c('include', 'exclude', 'cut'), keep.f=TRUE)
# 	   {
# 	   } )
# 
# setMethod ('split', signature=c(x='TimeInstantDataFrame', f='TimeIntervalDataFrame', drop='ANY'),
# 	   definition=function(x, f, boundaries=c('include', 'exclude', 'cut'), keep.f=TRUE, drop, ...)
# 	   {
# 	   } )
# 
# setMethod ('split', signature=c(x='TimeIntervalDataFrame', f='character', drop='ANY'),
# 	   definition=function(x, f, boundaries=c('include', 'exclude', 'cut'), keep.f=TRUE, drop, ...)
# 	   {
# 	   } )
# 
