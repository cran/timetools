#' @rdname changeSupport
#' @aliases changeSupport,TimeIntervalDataFrame,TimeIntervalDataFrame,numeric,ANY,ANY,ANY,ANY-method
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
setMethod ('changeSupport', signature(from='TimeIntervalDataFrame', to='TimeIntervalDataFrame',
				min.coverage='numeric', FUN='ANY', weights.arg='ANY',
				split.from='ANY', merge.from='ANY'),
	   definition=function (from, to, min.coverage, FUN=NULL, weights.arg=NULL,
				split.from=FALSE, merge.from=TRUE, ...) {
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
		   } else {
			   FUN <- match.fun (FUN)
		   }
		   if (overlapping (from))
		   	stop ("'changeSupport' function can not deal with overlapping 'from'.")
		   if (min.coverage<0 | min.coverage > 1) stop ("'min.coverage' must be between [0-1].")
		   if (timezone(from) != timezone (to))
		   {
		   	warning("'from' and 'to' have a different timezone. The timezone of 'to' is taken for the result")
			timezone(from) <- timezone(to)
		   }
		   int.from <- when (from)
		   int.to <- when (to)
		   
		   s.from <- start (int.from) ; e.from <- end (int.from) #s.from + int.from
		   s.to <- start (int.to) ; e.to <- end (int.to) #s.to + int.to
		   
		   nb <- .C ('project_nb_intersections',
				  as.integer(s.from), as.integer(e.from), as.integer(length(s.from)),
				  as.integer(s.to), as.integer(e.to), as.integer(length(s.to)),
				  nb=integer(1),
				  NAOK=FALSE, PACKAGE='timetools')$nb
		   
		   if (nb > 0) {
		   whiches <- .C ('project_pos_weight',
				  as.integer(s.from), as.integer(e.from), as.integer(length(s.from)),
				  as.integer(s.to), as.integer(e.to), as.integer(length(s.to)),
				  pos.from=integer(nb), pos.to=integer(nb), weight=integer(nb),
				  NAOK=FALSE, PACKAGE='timetools')[c('pos.from', 'pos.to', 'weight')]
		   whiches <- as.data.frame (whiches)
		   } else {
		   	whiches <- data.frame (pos.from=numeric(), pos.to=numeric(), weight=numeric())
		   }

		   # si from.split = FALSE, on dégage les from qui intersectent plusieurs to et on avertie
		   if (!split.from)
			   whiches <- whiches[!whiches$pos.from %in% unique (whiches$pos.from[duplicated(whiches$pos.from)] ),]

		   # si merge.from = FALSE, on dégage les to avec plusieurs from et on avertie
		   if (!merge.from)
			   whiches <- whiches[!whiches$pos.to %in% unique (whiches$pos.to[duplicated(whiches$pos.to)] ),]
		   # euh... si on merge pas on doit quand même pouvoir "dupliquer des lignes de to ?" Non pas forcément...
		   #	à voir

		   # pour ce qui restent on calcul les poids de from ramenées à 100% de to (ie : si to est complètement
		   #	couvert, la somme des poids est 1, sinon inférieure)
		   dur.to <- as.numeric(difftime(e.to, s.to, units='secs')) #as.duration(int.to)
		   whiches$weight <- whiches$weight / dur.to[whiches$pos.to]
		   whiches <- split (whiches, whiches$pos.to)
		   start <- s.to[as.numeric(names(whiches))]
		   end <- e.to[as.numeric(names(whiches))]

		   # enfin, on fait la projection
		   calc <- function(x, w, min.cov, weights.arg, FUN, fun.args) {
			   if (!is.null (weights.arg) )
				   fun.args[[weights.arg]] <- w
			   fun.args <- c(list(x), fun.args)
			   if (sum (w[!is.na (x)] ) >= min.cov)
				   res <- try (do.call (FUN, fun.args) ) else
				   res <- NA
			   if (inherits (res, 'try-error') )
				   res <- paste (x, collapse='-')
			   return (res)
		   }

		   interm <- mapply (list, whiches=whiches,
		   		     data=lapply (whiches,
				     	          function (i, x) x[i$pos.from,,drop=FALSE],
						  from@data),
				     SIMPLIFY=FALSE)
		   interm <- lapply (interm, function(x, ...)
				     as.data.frame (lapply (x$data, calc, x$whiches$weight, ...) ),
				     min.coverage, weights.arg, FUN, fun.args)

		   if (length (interm) > 0) {
			   while (length (interm) > 1) {
				   interm[[1]] <- rbind (interm[[1]], interm[[2]])
				   interm[[2]] <- NULL
			   }
			   interm <- interm[[1]]
			   interm <- new ('TimeIntervalDataFrame', start=start, end=end,
			   		  timezone=timezone(to), data=interm)
		   } else {
			   data <- data.frame(matrix(ncol=length(names(from)), nrow=0))
			   names (data) <- names (from)
			   interm <- new ('TimeIntervalDataFrame', start=start, end=end,
			   		  timezone=timezone(to), data=data)
		   }
		   
		   merge (to, interm)
	   } )

#' @rdname changeSupport
#' @aliases changeSupport,TimeIntervalDataFrame,character,numeric,ANY,ANY,missing,missing-method
#' @section from=TimeIntervalDataFrame, to=character:
#' \code{to} is one of 'year', 'month', 'day', 'hour', 'minute' or 'second'.
#' It defines the period (\code{\link{POSIXctp}}) to use to build
#' the new TimeIntervalDataFrame on which \code{from} will 
#' be agregated (or disagregated).
#'
#' So first, an \sQuote{empty} (no data) TimeIntervalDataFrame is created,
#' and then, the agregation is done accordingly to the  
#' \sQuote{from=TimeIntervalDataFrame, to=TimeIntervalDataFrame} section.
setMethod ('changeSupport', signature(from='TimeIntervalDataFrame', to='character', min.coverage='numeric',
				      FUN='ANY', weights.arg='ANY', 
				      split.from='missing', merge.from='missing'),
	   definition= function (from, to, min.coverage, FUN=NULL, weights.arg=NULL, ...) {
		   period <- POSIXctp(unit=to)
		   return (changeSupport (from, period, min.coverage, FUN, weights.arg, ...) )
	   } )

#' @rdname changeSupport
#' @aliases changeSupport,TimeIntervalDataFrame,POSIXctp,numeric,ANY,ANY,missing,missing-method
#' @section from=TimeIntervalDataFrame, to=POSIXctp:
#' \code{to} is period (see \code{\link{POSIXctp}}).
#' It defines the base of the new TimeIntervalDataFrame on which
#' \code{from} will be agregated (or disagregated).
#'
#' So first, an \sQuote{empty} (no data) TimeIntervalDataFrame is created,
#' and then, the agregation is done accordingly to the  
#' \sQuote{from=TimeIntervalDataFrame, to=TimeIntervalDataFrame} section.
setMethod ('changeSupport', signature(from='TimeIntervalDataFrame', to='POSIXctp', min.coverage='numeric',
				      FUN='ANY', weights.arg='ANY', 
				      split.from='missing', merge.from='missing'),
	   definition= function (from, to, min.coverage, FUN=NULL, weights.arg=NULL, ...) {
		   if (homogeneous (from) && continuous(from)  && to == period (from) ) return (from)
		   fun.args <- list (...)
		   if (is.null (FUN) ) {
			   if (length (fun.args) != 0 | !is.null (weights.arg) )
			   	warning ('Arguments passed to FUN are set to default.')
			   if (homogeneous (from) ) {
			   	FUN <- mean
			   	fun.args <- list(na.rm = TRUE)
			   } else {
			   	FUN <- weighted.mean
			   	fun.args <- list(na.rm = TRUE, weights.arg='w')
			   }
		   } else {
			   FUN <- match.fun (FUN)
		   }
		   if (length (to) > 1) {
			   warning ('Only the first given period is used as \'to\'.')
			   to <- to[1]
		   }
		   period <- to
		   #to <- names (to)[data.frame(to)>0]
		   #to <- to[length(to)]
		   u <- as.character (unit(to))
		   if (u == 'second') {
		   	s <- trunc (min(start(from)), 'secs')
		   } else if (u == 'minute') {
		   	s <- trunc (min(start(from)), 'mins')
		   } else if (u == 'hour') {
		   	s <- trunc (min(start(from)), 'hours')
		   } else if (u == 'day') {
		   	s <- trunc (min(start(from)), 'days')
		   } else if (u == 'month') {
		   	s <- as.POSIXct(sprintf('%s-01',
						format(min(start(from)), '%Y-%m')),
					timezone(from))
		   } else if (u == 'year') {
		   	s <- as.POSIXct(sprintf('%s-01-01',
						format(min(start(from)), '%Y')),
					timezone(from))
		   }
		   s <- as.POSIXct(s)

		if (u == 'year') {
			e <- max(end(from))
			nb <- year(e) - year(s) +
				ifelse(second(e, of='year') == 0, 0, 1)
		} else if (u == 'month') {
			e <- max(end(from))
			nb <- (year(e) - year(s))*12 + as.numeric(month(e)) - as.numeric(month(s)) +
				ifelse(second(e, of='month') == 0, 0, 1)
		} else {
			u <- switch (u, second='secs', minute='mins',
							     hour='hours', day='days')
			nb <- as.numeric (difftime(max(end(from)), s, units=u))
			nb <- ceiling (nb/duration(to))
		}

		   e <- s+as.numeric(nb) * to

		   result <- RegularTimeIntervalDataFrame (s, e, by=period, timezone=timezone(from))
		   result <- do.call (changeSupport, c(from=from, to=result, min.coverage=min.coverage,
						       FUN=FUN, weights.arg=weights.arg, fun.args,
						       split.from=FALSE, merge.from=TRUE) )
		   return (result)
	   } )

