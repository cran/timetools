# definition de la classe
#------------------------
setClass (Class = 'SubtimeDataFrame', 
	  representation = representation (when='subtime',
					   data='data.frame'),
	  prototype = prototype (when=structure (factor(), class=c('subtime', 'factor') ),
				 data=data.frame()),
	  validity=function(object) {
		  if (length (when (object)) != nrow (object))
			  stop ("In a 'SubtimeDataFrame, 'data' must have a number of rows as long as 'when'.")
		  return (TRUE)
	  } )

# constructeurs
#--------------
#' Create a SubtimeDataFrame from scratch
#' 
#' To see all methods related to this class, 
#' see \code{\link{SubtimeDataFrame-class}}
#'
#' If both \code{when} and \code{data}
#' are given, \code{data} must have a number of rows identical to the length of
#' \code{when}.
#'
#' To access to the class documentation, type in the R console :
#'
#' \code{class?SubtimeDataFrame}
#'
#' @examples
#' st <- subtime (1:4, 'wday')
#' SubtimeDataFrame (st, data.frame (test=sample (0:100, 4)))
#' 
#' @param when \code{\link{subtime}}.
#' @param data a data.frame with as much rows as the length of \sQuote{when}.
#' Can be \code{NULL} (hence the data.frame has zero column and as much
#' rows as needed).
#' @param \dots arguments to or from other methods
#'
#' @return a \code{\link[=SubtimeDataFrame-class]{SubtimeDataFrame}} object.
#' @seealso \code{\link[=SubtimeDataFrame-class]{SubtimeDataFrame}},
#' \code{\link{subtime}}, \code{\link{timetools}}
SubtimeDataFrame <- function (when, data=NULL, ...) {
	if (is.null (data)) data <- data.frame (matrix (NA, ncol=0, nrow=length(when) ) )
	new ('SubtimeDataFrame', when=when, data=data)
}

# definition des accesseurs de l'objet
#-------------------------------------
#' @rdname time.properties
#' @aliases when,SubtimeDataFrame-method
setMethod (f='when', signature='SubtimeDataFrame',
	   definition=function(x, ...) return(x@when) )
#' @rdname time.properties
#' @aliases timezone,SubtimeDataFrame-method
setMethod (f='timezone', signature='SubtimeDataFrame',
	   definition=function(object) return(timezone(when(object))) )
#### À voir plus tard
# @rdname time.properties
# @aliases timezone<-,SubtimeDataFrame-method
# setMethod (f='timezone<-', signature='TimeInstantDataFrame',
# 		  definition=function(object, value) {
# 			object@timezone <- value
# 			object@instant <- as.POSIXct (as.POSIXlt (object@instant, value) )
# 			return(object)
# 		} )

# mise en forme pour / et affichage
#----------------------------------
print.SubtimeDataFrame <- function (x, ...) {
	print(data.frame (when=when(x), x@data) )
	print (levels (when (x)))
}
setMethod ('show', 'SubtimeDataFrame',
	   function (object) {
	   print(data.frame (when=when(object), object@data) )
	   print (levels (when (object)))
	   } )
tail.SubtimeDataFrame <- function (x, ...) {
		print (tail (data.frame (when=when(x), x@data) ) )
		print (levels (when (x)))
	   }

head.SubtimeDataFrame <- function (x, ...) {
		print(head (data.frame (when=when(x), x@data) ) )
		print (levels (when (x)))
	   }

summary.SubtimeDataFrame <- function (object, ...) {
		print (summary (data.frame (when=when(object), object@data) ) )
		print (levels (when (object)))
	   }
# format

# defintion des accesseurs aux donnees
#-------------------------------------
'[.SubtimeDataFrame' <- function(x, i, j, drop=FALSE) {
	n.args <- nargs() - hasArg(drop)
	if (missing (j) & n.args==2) {
		j <- i
		i <- seq_len(nrow(x))
	}
	if(missing(i)) i <- seq_len(nrow(x))
	y <- new ('SubtimeDataFrame', 
	     when =when (x)[i, drop=drop],
	     data = x@data[i, j, drop=drop])
	validObject(y)
	return(y)
}
setMethod (f='[[', signature='SubtimeDataFrame',
	   definition=function(x, i, ...) {
		   '[[.data.frame'(x@data, i, ...)
	   })
setMethod (f='$', signature='SubtimeDataFrame',
	   definition=function(x, name) {
		   do.call ('$', list(x=x@data, name=name))
	   })

'[<-.SubtimeDataFrame' <- function(x, i, j, value) {
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
		x@when <- x@when[available.rows]
	}
	row.names(tmp) <- rn
	x@data <- tmp
	validObject(x)
	return(x)
}
'[[<-.SubtimeDataFrame' <- function(x, i, j, value) {
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
	   x@when <- x@when[available.rows]
   }
   row.names(tmp) <- rn
   x@data <- tmp
   validObject(x)
   return(x)
}
setMethod (f='$<-', signature='SubtimeDataFrame',
	   definition=function(x, name, value) {
		   x@data <- "$<-.data.frame"(x@data, name, value)
		   validObject(x)
		   return(x)
	   })

setMethod (f='dim', signature='SubtimeDataFrame',
	   definition=function(x) dim (x@data))
setMethod (f='nrow', signature='SubtimeDataFrame',
	   definition=function(x) nrow (x@data))
setMethod (f='ncol', signature='SubtimeDataFrame',
	   definition=function(x) ncol (x@data))
row.names.SubtimeDataFrame <- function(x) row.names (x@data)
'row.names<-.SubtimeDataFrame' <- function(x, value) {
		   row.names (x@data) <- value
		   x
	   }
setMethod (f='names', signature='SubtimeDataFrame',
	   definition=function(x) names (x@data))
setMethod (f='names<-', signature='SubtimeDataFrame',
	   definition=function(x, value) {
		   names (x@data) <- value
		   x
	   } )

# Ops
# Math

# manipulation
#-------------
split.SubtimeDataFrame <- function(x, f, drop=FALSE, ...) {
		   vect <- seq_len(nrow(x))
		   w <- split (when(x), f, drop)
		   data <- split (x@data, f, drop)
		   mapply (SIMPLIFY=FALSE, new, 'SubtimeDataFrame',
			   when=w, data=data)
	   }

# # fonction réalisée en S3 pour ne pas imposer de 'signature'
# rbind.SubtimeDataFrame <- function (...) {
# 	dots <- list (...)
# 	names(dots) <- NULL
# 	dots <- lapply (dots, function(x) as(x, 'SubtimeDataFrame') )
# 	when <- unlist (lapply (dots, when) )
# 	df <- do.call("rbind", lapply(dots, function(x) x@data) )
# 	new('SubtimeDataFrame', when=when, data=df)
# }
# cbind # a faire eventuellement entre un Time*DataFrame et une data.frame
merge.SubtimeDataFrame <- function(x, y, by, all=TRUE, ...) {
		if (!inherits(y, 'SubtimeDataFrame')) stop ("'y' must be a 'SubtimeDataFrame'.")
		if (missing (by) ) by <- NULL
		when.vec <- list (when(x), when(y))
		if (!all (levels (when(x))== levels(when(y))))
			stop('Levels of x and y should be the same.')
		x.data <- data.frame (when=when(x), x@data)
		y.data <- data.frame (when=when(y), y@data)
		z <- merge (x.data, y.data, by=unique (c('when', by) ), all=all, ...)
		when <- factor (z$when, levels=levels(when(x)), ordered=TRUE)
		attributes(when)$timezone <- timezone(when(x))
		class (when) <- c('subtime', 'factor')
		z <- new ('SubtimeDataFrame',
			  when=when,
			  data=z[setdiff(names(z), c('when'))])
		return (z)
	   }


# acces/modification de certaines propriétés
#-------------------------------------------


# transformateur de classe
#-------------------------
# as.TimeInstantDataFrame.TimeIntervalDataFrame <- function(from, how=c('mid', 'start', 'end'), ...) {
#         how <- match.arg (how)
#         instant <- switch (how,
#                            mid=.POSIXct(rowMeans(data.frame(unclass(start(from)), unclass(end(from)))), tz=from@timezone),
#                            start=start(from), end=end(from))
#         to <- new ('TimeInstantDataFrame', instant=instant, data=from@data)
#         validObject(to)
#         return (to)
# }

as.data.frame.SubtimeDataFrame <- function (x, row.names=NULL, optional=FALSE, include.dates=FALSE, ...) {
	if (include.dates)
		return (data.frame (when=when(x), x@data) ) else
		return (x@data)
}
