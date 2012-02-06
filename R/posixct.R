#' @rdname posix.properties
#' @aliases year,POSIXct-method
setMethod ('year', 'POSIXct', function(x, ...) as.POSIXlt (x)$year+1900)
#' @rdname posix.properties
#' @aliases month,POSIXct-method
setMethod ('month', 'POSIXct', function(x, ...) as.POSIXlt (x)$mon)
#' @rdname posix.properties
#' @aliases day,POSIXct-method
setMethod ('day', 'POSIXct', function(x, of, ...) {
		switch (of,
		year = as.POSIXlt (x)$yday,
		month = as.POSIXlt (x)$mday,
		week = as.POSIXlt (x)$wday,
		"'of' should be one of (year, month, week)")
		})
#' @rdname posix.properties
#' @aliases hour,POSIXct-method
setMethod ('hour', 'POSIXct', function(x, of, ...) {
		lt <- as.POSIXlt (x)
		switch (of,
		year = 	lt$yday * 24	 + lt$hour,
		month =	(lt$mday-1) * 24 + lt$hour,
		week = 	lt$wday * 24	 + lt$hour,
		day = 			   lt$hour,
		"'of' should be one of (year, month, week, day)")
		})
#' @rdname posix.properties
#' @aliases minute,POSIXct-method
setMethod ('minute', 'POSIXct', function(x, of, ...) {
		lt <- as.POSIXlt (x)
		switch (of,
		year = 	(lt$yday * 24	  + lt$hour) * 60 + lt$min,
		month =	((lt$mday-1) * 24 + lt$hour) * 60 + lt$min,
		week = 	(lt$wday * 24	  + lt$hour) * 60 + lt$min,
		day = 	(		    lt$hour) * 60 + lt$min,
		hour = 					    lt$min,
		"'of' should be one of (year, month, week, day, hour)")
		})
#' @rdname posix.properties
#' @aliases second,POSIXct-method
setMethod ('second', 'POSIXct', function(x, of, ...) {
		lt <- as.POSIXlt (x)
		switch (of,
		year = 	((lt$yday * 24	   + lt$hour) * 60 + lt$min) * 60 + lt$sec,
		month =	(((lt$mday-1) * 24 + lt$hour) * 60 + lt$min) * 60 + lt$sec,
		week = 	((lt$wday * 24	   + lt$hour) * 60 + lt$min) * 60 + lt$sec,
		day = 	((		     lt$hour) * 60 + lt$min) * 60 + lt$sec,
		hour = 	(				     lt$min) * 60 + lt$sec,
		minute = 						    lt$sec,
		"'of' should be one of (year, month, week, day, hour, minute)")
		})


