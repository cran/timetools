#' @rdname posix.properties
#' @aliases year,ANY-method
setMethod ('year', 'ANY', function(x, ...) POSIXst(x, unit='year', ...) )
#' @rdname posix.properties
#' @aliases month,ANY-method
setMethod ('month', 'ANY', function(x, ...) POSIXst(x, unit='month', ...) )
#' @rdname posix.properties
#' @aliases day,ANY-method
setMethod ('day', 'ANY', function(x, of, ...)
	   POSIXst(x, unit='day', of=of, ...))
#' @rdname posix.properties
#' @aliases hour,ANY-method
setMethod ('hour', 'ANY', function(x, of, ...)
	   POSIXst(x, unit='hour', of=of, ...))
#' @rdname posix.properties
#' @aliases minute,ANY-method
setMethod ('minute', 'ANY', function(x, of, ...)
	   POSIXst(x, unit='minute', of=of, ...))
#' @rdname posix.properties
#' @aliases second,ANY-method
setMethod ('second', 'ANY', function(x, of, ...)
	   POSIXst(x, unit='second', of=of, ...))

