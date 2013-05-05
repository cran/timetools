# generic pour toutes les classes Time*DataFrame
#-----------------------------------------------
# S4
setGeneric (name='nrow')
setGeneric (name='ncol')
setGeneric (name='lapply')

# fonctions génériques (time properties)
#---------------------------------------
# TODO : dispatch in the classe definition files ?
unit <- function(x, ...) UseMethod('unit')
'unit<-' <- function(object, value) UseMethod('unit<-')
setGeneric ('unit', function (x, ...) standardGeneric ('unit') )
setGeneric ('unit<-', function (object, value) standardGeneric ('unit<-') )

duration <- function(x, ...) UseMethod('duration')
setGeneric ('duration', function (x, ...) standardGeneric ('duration') )

timezone <- function (object) UseMethod('timezone')
'timezone<-' <- function(object, value) UseMethod('timezone<-')
setGeneric (name='timezone',
	    def=function (object) standardGeneric ('timezone') )
setGeneric (name='timezone<-',
	    def=function (object, value) standardGeneric ('timezone<-') )

of <- function(x, ...) UseMethod('of')

continuous <- function(x, ...) UseMethod('continuous')
setGeneric (name='continuous',
	    def=function(x, ...) standardGeneric('continuous'))
'continuous<-' <- function(x, value) UseMethod('continuous<-')
setGeneric (name='continuous<-',
	    def=function(x, value) standardGeneric('continuous<-'))

'homogeneous' <- function(x, ...) UseMethod('homogeneous')
setGeneric (name='homogeneous',
	    def=function(x, ...) standardGeneric('homogeneous'))

'overlapping' <- function(x, ...) UseMethod('overlapping')
setGeneric (name='overlapping',
	    def=function(x, ...) standardGeneric('overlapping'))

'period' <- function(x, ...) UseMethod('period')
setGeneric (name='period', def=function(x, ...) standardGeneric('period'))

'when' <- function(x, ...) UseMethod('when')
setGeneric (name='when', def=function(x, ...) standardGeneric('when'))

'regular' <- function(x, ...) UseMethod('regular')
setGeneric (name='regular', def=function(x, ...) standardGeneric('regular'))

'interval' <- function(x, ...) UseMethod('interval')
setGeneric (name='interval', def=function(x, ...) standardGeneric('interval'))

# generic pour interval (S3)
#---------------------------
#' intersects 2 \sQuote{POSIXcti} objects
#'
#' This function allows to find the intersection of 
#' two objects of the same class.
#'
#' @rdname intersect
#'
#' @examples
#' # to see all existing methods :
#' methods ('\%intersect\%')
#' 
#' @param i1 first object to intersect
#' @param i2 second object to intersect
#'
#' @return object of the same class of parameters
'%intersect%' <- function(i1, i2) UseMethod ('%intersect%')

#' test inclusion of 2 \sQuote{POSIXcti} objects
#' 
#' This function test if the first \sQuote{POSIXcti}
#' object is included in the second.
#'
#' @rdname included
#'
#' @examples
#' # to see all existing methods :
#' methods ('\%included\%')
#' 
#' @param i1 is this object included in the second object ?
#' @param i2 is this object include the first one ?
#'
#' @return boolean
'%included%' <- function(i1, i2) UseMethod ('%included%')

# generic de coercition
#----------------------
#' Convert an object to a POSIXcti
#' 
#' @param from object to convert to a POSIXcti
#' @param \dots more args to or from other methods
#' @return a \code{\link{POSIXcti}}
as.POSIXcti <- function(from, ...) UseMethod ('as.POSIXcti')
#' Convert an object to a POSIXctp
#' 
#' @param from object to convert to a POSIXctp
#' @param \dots more args to or from other methods
#' @return a \code{\link{POSIXctp}}
as.POSIXctp <- function(from, ...) UseMethod ('as.POSIXctp')
#' Convert an object to a TimeIntervalDataFrame
#' 
#' @param from object to convert to a TimeIntervalDataFrame
#' @param \dots more args to or from other methods
#' @return a \code{\link{TimeIntervalDataFrame}}
as.TimeIntervalDataFrame <- function(from, ...) UseMethod ('as.TimeIntervalDataFrame')
#' Convert an object to a TimeInstantDataFrame
#' 
#' @param from object to convert to a TimeInstantDataFrame
#' @param \dots more args to or from other methods
#' @return a \code{\link{TimeInstantDataFrame}}
as.TimeInstantDataFrame  <- function(from, ...) UseMethod ('as.TimeInstantDataFrame')
#' Convert an object to a SubtimeDataFrame
#'
#' @param x object to convert to a TimeInstantDataFrame
#' @inheritParams POSIXst
#' @param \dots more args to or from other methods or FUN
#' @return a \code{\link{SubtimeDataFrame}}
as.SubtimeDataFrame  <- function(x, unit, of, ...)
	UseMethod ('as.SubtimeDataFrame')

