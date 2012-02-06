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

#' define valid units for time objects
#' 
#' With no argument, the functions return a factor containing
#' the valid time units. With an argument, it returns the units 
#' asked for.
#' @param x a character string representing the needed units
#' @param \dots arguments to or from other methods
POSIXt.units <- function(x=NULL, ...) {
	availables.units <- c('second', 'minute', 'hour', 'day', 'week', 'month', 'year')
	if (is.null (x))
		return (factor (availables.units[c(1:4, 6:7)], availables.units, ordered = TRUE) ) else
	return (factor (x, availables.units, ordered=TRUE) )
}
