#' 1970-01-01 GMT
#'
#' Origin is the date-time for 1970-01-01 GMT in POSIXct format. This date-time 
#' is the origin for the numbering system used by POSIXct, POSIXlt, chron, and 
#' Date classes.
#'
#' The original implementation of this 'object' is in the lubridate package.
#'
#' @export origin
#' @keywords data chron
#' @author Garrett Grolemund "grolemund at rice.edu", Hadley Wickham "h.wickham at gmail.com"
#' @examples
#' origin
#' # "1970-01-01 GMT"
origin <- structure(0, class = c("POSIXt", "POSIXct"))

POSIXt.units <- function(x=NULL, ...) {
	availables.units <- c('second', 'minute', 'hour', 'day',
			      'week', 'month', 'year', 'AD')
	if(is.null( x ))
		return(factor(availables.units, availables.units,
			      ordered = TRUE) ) else
		return (factor (x, availables.units, ordered=TRUE) )
	      # return (factor (availables.units[c(1:4, 6:7)],
	      # availables.units, ordered = TRUE) ) else
}

#' define generic function to compare anything to a numeric
#'
#' @name ops.numeric
#' @aliases Ops,ANY,numeric-method
#' @aliases Ops,numeric,ANY-method
setMethod ('Ops', c('numeric', 'ANY'),
	function (e1, e2) do.call(.Generic, list(e1=e1, e2=as.numeric(e2))))
setMethod ('Ops', c('ANY', 'numeric'),
	function (e1, e2) do.call(.Generic, list(e1=as.numeric(e1), e2=e2)))
