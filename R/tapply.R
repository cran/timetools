#' Apply a Function Over a time properties
#'
#' Apply a function over a Time*DataFrame that is first splitted
#' into several sets according to time properties.
#'
#' These functions are equivalent to old \code{\link{changeSupport}} methods.
#' Instead of having the core splitting algorithm in it, it uses
#' the \code{\link{split}} methods. Be aware that default parametres values
#' between the two families (\sQuote{changeSupport} and \sQuote{tapply}) are not
#' necessarily the same.
#' 
#' Users are encouraged to use \sQuote{tapply} instead of \sQuote{changeSupport} since new
#' versions of \sQuote{changeSupport} are only wrappers to tapply.
#'
#' @param X a \code{\link{TimeIntervalDataFrame}} or a 
#' \code{\link{TimeInstantDataFrame}}
#' @param INDEX an object corresponding to or containing
#' a time properties. Classes available depend on X. See sections
#' below to know all (X, INDEX) combination defined.
#' @param FUN the function to be applied.
#' @param \dots optional arguments to 'FUN'.
#' @param simplify if FALSE a list of \sQuote{Time*DataFrame} is returned ;
#' if TRUE \sQuote{tapply} try to reduce the list to a single \sQuote{Time*DataFrame}.
#' @param min.coverage a numeric between 0 and 1 indicating the percentage
#'  of valid values over each interval to allow an aggregation. NA is 
#' returned if the percentage is not reach. In that configuration (min.coverage
#' between 0 and 1, overlapping intervals are not allowed).
#' When a function (FUN) has a na.rm argument, the na.rm=TRUE behaviour
#' is met if na.rm is set to TRUE and min.coverage to 0 (zero) ; the
#' na.rm=FALSE behaviour is met if na.rm is set to FALSE whatever is the value
#' of min.coverage.
#' If min.coverage is set to NA, time coverage of the resulting interval is not
#' checked. Moreover, overlapping of X intervals is not checked. Thus the 
#' aggregation is done according to \sQuote{weights.arg} argument (if given).
#' @param weights.arg if FUN has a \sQuote{weight} argument, this parameter must
#' be a character naming the weight argument. For instance, if FUN is 
#' \code{\link[stats]{weighted.mean}}, then weights.arg is \code{'w'}.
#' @param merge.X logical indicating if data in \sQuote{X} can be
#' merged over interval of the new time support.
#' @param split.X logical indicating if data in \sQuote{X} that are over
#' several intervals of 'INDEX' must be 'cut' to fit to new intervals (TRUE) or
#' ignored (FALSE).
#' @param keep.INDEX logical indicating if INDEX values must be kept on the resulting
#' list.
#'
#' @rdname tapply
#'
#' @seealso \code{\link[base]{tapply}}, \code{\link{TimeIntervalDataFrame-class}},
#' \code{\link{TimeInstantDataFrame-class}}, \code{\link{SubtimeDataFrame-class}},
#' \code{\link{changeSupport}},
#' \code{\link{POSIXcti-class}}, \code{\link{POSIXst-class}},
#' \code{\link{POSIXctp-class}}
setGeneric (name='tapply')

#' @rdname tapply
#' @aliases tapply,TimeIntervalDataFrame,TimeIntervalDataFrame-method
#' 
#' @section signature(TimeIntervalDataFrame, TimeIntervalDataFrame):
#' split \code{\link{TimeIntervalDataFrame}}  over another \code{\link{TimeIntervalDataFrame}}
#' and then apply a function over each elements of the list.

setMethod (
	'tapply',
	signature(X='TimeIntervalDataFrame', INDEX='TimeIntervalDataFrame'),
	definition=function (X, INDEX, FUN, ..., min.coverage=1,
			     weights.arg=NULL, merge.X=TRUE,
			     split.X=FALSE, keep.INDEX=TRUE,
			     simplify=TRUE)
{
	fun.args <- list( ... )
	FUN <- match.fun( FUN )

	s.INDEX <- start(INDEX)
	e.INDEX <- end(INDEX)

	# test de validité de min.coverage

	if( (min.coverage < 0 | min.coverage > 1) & !is.na(min.coverage) )
		stop ("'min.coverage' must be between [0-1] or NA." )

	# splittage des données

	splitted <- split(X, INDEX, split.x=split.X, keep.f = FALSE)

	# test sur l'overlapping des intervals (en fonction de min.coverage)

	if( !is.na( min.coverage ))
		if( any(sapply(splitted, overlapping)) )
			stop ("Overlapping data not allowed. Set min.coverage to NA.")

	# si merge.X = FALSE, les valeurs pour les intervalles avec plus d'une
	# valeur sont mises à NA

	if( !merge.X )
		splitted[sapply(splitted, nrow) > 1] <-
			lapply(splitted[sapply(splitted, nrow) > 1],
			       function(x){x[TRUE] <- NA ; x})

	# pour chaque élément de splitted, FUN est appliquée avec les arguments
	# indiqués, notamment le weights.arg si nécessaire.
	# une sous-fonction calc est définie pour réaliser cette action.

	calc <- function(x, s, e, mc, ws.a, FUN, f.as)
	{
		# si aucune valeur de X n'est projetable sur l'INDEX
		# la fonction retourne un TItDF avec les valeurs à NA
		if( nrow(x) == 0 )
			return( merge(x, TimeIntervalDataFrame(s, e)) )

		# calcul des poids et ajout si nécessaire dans les arguments
		# de la fonction
		d <- as.numeric(e) - as.numeric(s)
		if(!is.null( ws.a ) | !is.na( mc ))
			w <- as.numeric(end(x)) - as.numeric(start(x))
		if(!is.null( ws.a ))
			f.as[[ws.a]] <- w

		# test par rapport à min.coverage : si NA rien, sinon pour
		# chaque variable de x on test individuellement si la 
		# couverture totale représentée par les valeurs non-NA respecte
		# le critère indiqué (min.coverage)
		if( !is.na(mc) ) {
			valid <- sapply(x@data, w=w, m=mc*d,
				function(var, w, m)
				{
					return( sum(w[!is.na(var)]) >= m )
				} )
		} else {
			valid <- rep(TRUE, length(x))
		}

		# calcul effectif
		res <- list()

		res[which(valid)] <- sapply(x@data[valid],
			      function(x, FUN, f.as)
			      {
				      f.as <- c(list(x), f.as)
				      do.call(FUN, f.as)
			      },
			      FUN, f.as)
		res[which(!valid)] <- NA
		res <- as.list(unlist( res ))

		# mise en forme
		names(res) <- names(x)
		res <- as.data.frame(res)
		res <- TimeIntervalDataFrame(s, e, data=res)
		return (res)
	}

	result <- mapply(calc, splitted, s.INDEX, e.INDEX,
			 MoreArgs=list(mc=min.coverage, ws=weights.arg,
				       FUN=FUN, f.as=fun.args),
			 SIMPLIFY=FALSE)

	# conservation des valeurs de INDEX/alias.f (ou pas)

	if( keep.INDEX )
		for( n in names(INDEX) )
		{
			# tests et manip pour gérer les problèmes en cas de 
			# noms identiques entre x et f
			new.n <- n
			i <- 1
			while( new.n %in% names(X) ) {
				new.n <- paste(n, i, sep='.')
				i <- i+1
			}
			if( new.n != n)
				warning(sprintf("'%s' in INDEX renamed as '%s'",
						n, new.n))

			# ajout des valeurs de f dans result
			result <- mapply(function(x, i, value)
			       {
				       x@data[[i]] <- rep(value, nrow(x))
				       return( x )
	       		       },
			       x=result, value=INDEX[[n]],
			       MoreArgs=list(i=new.n), SIMPLIFY=FALSE)
		}

	# simplification du résultat (ou pas)

	if( simplify )
	{
		ns <- names( result[[1]] )
		data <- lapply(ns,
			       function(x, i) sapply(x, '[[', i),
			       x=result)
		names(data) <- ns
		data <- as.data.frame(data)

		start <- sapply(result, start)
		start <- as.POSIXct(start, origin=origin)
		end <- sapply(result, end)
		end <- as.POSIXct(end, origin=origin)
		result <- TimeIntervalDataFrame(start, end, data=data,
						timezone=timezone(INDEX))
	}
	
	return( result )
	} )

#' @rdname tapply
#' @aliases tapply,TimeIntervalDataFrame,POSIXctp-method
#' 
#' @section signature(TimeIntervalDataFrame, POSIXctp):
#' split a \code{\link{TimeIntervalDataFrame}} against 
#' regular time intervals with a period defined by INDEX (a
#' \code{\link{POSIXctp}}).
#' Then a function is applied over each elements of the list.

setMethod (
	'tapply',
	signature(X='TimeIntervalDataFrame', INDEX='POSIXctp'),
	definition=function (X, INDEX, FUN, ..., min.coverage=1,
			     weights.arg=NULL, merge.X=TRUE,
			     split.X=FALSE, simplify=TRUE)
{
	s <- min(start(X))
	e <- max(end(X))
	tzone <- timezone( X )
	INDEX <- TimeIntervalDataFrame(s, e, period=INDEX, timezone=tzone)

	tapply( X, INDEX, FUN, ..., min.coverage=min.coverage, 
	       weights.arg=weights.arg, merge.X=merge.X, split.X=split.X,
	       simplify=simplify)
} )

#' @rdname tapply
#' @aliases tapply,TimeIntervalDataFrame,POSIXcti-method
#' 
#' @section signature(TimeIntervalDataFrame, POSIXcti):
#' split \code{\link{TimeIntervalDataFrame}} against specified 
#' intervals (\code{\link{POSIXcti}}).
#' and then apply a function over each elements of the list.

setMethod (
	'tapply',
	signature(X='TimeIntervalDataFrame', INDEX='POSIXcti'),
	definition=function (X, INDEX, FUN, ..., min.coverage=1,
			     weights.arg=NULL, merge.X=TRUE,
			     split.X=FALSE, simplify=TRUE)
{
	tzone <- attributes(start(INDEX))$tzone[1]
	INDEX <- TimeIntervalDataFrame(start(INDEX), end(INDEX), tzone)

	tapply(X, INDEX, FUN, ..., min.coverage=min.coverage,
	       weights.arg=weights.arg, merge.X=merge.X,
	       split.X=split.X, keep.INDEX=FALSE, simplify=simplify)
} )

