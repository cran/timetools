# fonctions génériques
#---------------------
#' @rdname posix.properties
year <- function(x, ...) UseMethod('year')
#' Extract time properties from POSIX* objects
#'
#' Objects can be POSIXct, POSIXcti, POSIXctp, etc.
#'
#' \code{year}, \code{month}, \code{day}, \code{hour},
#' \code{minute} and \code{second} are methods
#' defined to extract the adequat information from a time object.
#' These functions are wrappers to \code{\link{POSIXst}}.
#'
#' \code{unit} \code{unit<-} is used to extract or set
#' the unit of a time object
#' (cf \code{\link{POSIXt.units}}).
#'
#' @name posix.properties
#' 
#' @param x object from which the property is to be extracted
#' (POSIXct, POSIXcti, POSIXctp)
#' @param of used to specify the main period 
#' from wich the time propertie is to extract.
#' For instance : \code{day(x, 'week')} will differ
#' from \code{day(x, 'year')}.
#' @param \dots arguments to or from other methods
#' @inheritParams POSIXst
#'
#' @aliases year-methods
setGeneric ('year', function (x, ...) standardGeneric ('year') )
#' @rdname posix.properties
month <- function(x, ...) UseMethod('month')
#' @rdname posix.properties
#' @aliases month-methods
setGeneric ('month', function (x, ...) standardGeneric ('month') )
#' @rdname posix.properties
day <- function(x, of, ...) UseMethod('day')
#' @rdname posix.properties
#' @aliases day-methods
setGeneric ('day', function (x, of, ...) standardGeneric ('day') )
#' @rdname posix.properties
hour <- function(x, of, ...) UseMethod('hour')
#' @rdname posix.properties
#' @aliases hour-methods
setGeneric ('hour', function (x, of, ...) standardGeneric ('hour') )
#' @rdname posix.properties
minute <- function(x, of, ...) UseMethod('minute')
#' @rdname posix.properties
#' @aliases minute-methods
setGeneric ('minute', function (x, of, ...) standardGeneric ('minute') )
#' @rdname posix.properties
second <- function(x, of, ...) UseMethod('second')
#' @rdname posix.properties
#' @aliases second-methods
setGeneric ('second', function (x, of, ...) standardGeneric ('second') )
#' @rdname posix.properties
unit <- function(x, ...) UseMethod('unit')
#' @rdname posix.properties
#' @aliases unit-methods
setGeneric ('unit', function (x, ...) standardGeneric ('unit') )
#' @rdname posix.properties
#' @usage unit(object) <- value
'unit<-' <- function(object, value) UseMethod('unit<-')
#' @rdname posix.properties
#' @aliases unit<--methods
setGeneric ('unit<-', function (object, value) standardGeneric ('unit<-') )
#' @rdname posix.properties
duration <- function(x, ...) UseMethod('duration')
#' @rdname posix.properties
#' @aliases duration-methods
setGeneric ('duration', function (x, ...) standardGeneric ('duration') )

# generic divers
#----------------
#' @rdname time.properties
timezone <- function (object) UseMethod('timezone')
#' @rdname time.properties
what <- function(x, ...) UseMethod('what')
#' @rdname time.properties
of <- function(x, ...) UseMethod('of')

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

# generic pour toutes les classes Time*DataFrame
#-----------------------------------------------
# S4
setGeneric (name='nrow')
setGeneric (name='ncol')
setGeneric (name='lapply')

#' @rdname time.properties
continuous <- function(x, ...) UseMethod('continuous')
#' Test or extract different properties of Time objects
#' 
#' These functions can be applied to Time objects such as
#' \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}},
#' \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}} or
#' \code{\link{POSIXst}}.
#'
#' @section reminder:
#' For each class, you can type the following code to know
#' what methods are available.
#'
#' \code{methods(class='myclass')} #S3 methods\cr
#' \code{showMethods(class='myclass')} #S4 'methods\cr
#'
#' For each S3 method, you can type the following code to know
#' for which classes it is defined.
#'
#' \code{methods('mymethod')}
#'
#' For each S4 method, you can type the following code to know
#' for which classes it is defined.
#'
#' \code{showMethods('mymethod')}
#'
#' @section continuous:
#' For objects based on time intervals. After ordering intervals, 
#' test if the end of an interval is the start of the next interval.
#' If any interval overlap another one, it returns \code{FALSE}.
#' 
#' If not any interval overlap another, and the object is not 
#' continuous, the object can be set 'continuous' with
#'
#' \code{continuous(obj) <- TRUE}
#'
#' Interval will be added such as the object can pass the 
#' test describe below. The data is filled with NA values.
#'
#' @param object object to test, from which get or set a property
#' @param x object to test, from which get or set a property
#' @param value boolean if possible, set this value to the property
#' @param \dots arguments to or from other methods
#' @return depends on the method
#' 
#' @name time.properties
#' @aliases continuous continuous-methods
setGeneric (name='continuous', def=function(x, ...) standardGeneric('continuous'))
#' @rdname time.properties
#' @usage continuous(x) <- value
'continuous<-' <- function(x, value) UseMethod('continuous<-')
#' @rdname time.properties
#' @aliases continuous<--methods
setGeneric (name='continuous<-', def=function(x, value) standardGeneric('continuous<-'))
#' @rdname time.properties
'homogeneous' <- function(x, ...) UseMethod('homogeneous')
#' @rdname time.properties
#' @aliases homogeneous-methods
#' @section homogeneous:
#' For objects based on time intervals (\code{\link{POSIXcti}}). Test if intervals
#' of the object are \sQuote{homogeneous} : if the period
#' of each interval is the same.
setGeneric (name='homogeneous', def=function(x, ...) standardGeneric('homogeneous'))
#' @rdname time.properties
'overlapping' <- function(x, ...) UseMethod('overlapping')
#' @rdname time.properties
#' @aliases overlapping-methods
#' @section overlapping:
#' For objects based on time intervals. Test if any interval
#' overlap another one. Because the test can be ressource consuming,
#' it stops at the first case encoutered that does not satisfy this
#' condition. The two indices corresponding are printed.
setGeneric (name='overlapping', def=function(x, ...) standardGeneric('overlapping'))
#' @rdname time.properties
'period' <- function(x, ...) UseMethod('period')
#' @rdname time.properties
#' @aliases period-methods
#' @section period:
#' For objects based on time intervals. Return \code{\link{POSIXctp}}
#' of the object if it is homogeneous and continuous.
setGeneric (name='period', def=function(x, ...) standardGeneric('period'))
#' @rdname time.properties
'when' <- function(x, ...) UseMethod('when')
#' @rdname time.properties
#' @aliases when-methods
#' @section when:
#' For Time objects. If TimeInstantDataFrame, return the instants
#' of the object ; if TimeIntervalDataFrame, return the intervals
#' of the object.
setGeneric (name='when', def=function(x, ...) standardGeneric('when'))
#' @rdname time.properties
'regular' <- function(x, ...) UseMethod('regular')
#' @rdname time.properties
#' @aliases regular-methods
#' @section regular:
#' Test if the object is regular. A TimeInstantDataFrame is regular
#' if all instants are equally spaced. A TimeIntervalDataFrame is regular
#' if it is homogeneous and all interval's start are equally spaced.
setGeneric (name='regular', def=function(x, ...) standardGeneric('regular'))
#' @rdname time.properties
#' @aliases timezone-methods
#' @section timezone:
#' get or set  the timezone of the time object (see  \code{\link[base]{timezone}}
#' in the base package).
setGeneric (name='timezone', def=function (object) standardGeneric ('timezone') )
#' @rdname time.properties
#' @usage timezone(object) <- value
'timezone<-' <- function(object, value) UseMethod('timezone<-')
#' @rdname time.properties
#' @aliases timezone<--methods
setGeneric (name='timezone<-', def=function (object, value) standardGeneric ('timezone<-') )

# generic pour TimeIntervalDataFrame
#-----------------------------------
#' @rdname time.properties
'interval' <- function(x, ...) UseMethod('interval')
#' @rdname time.properties
#' @aliases interval-methods
#' @section interval:
#' For objects based on time intervals. Return \code{\link{POSIXcti}}
#' of each interval.
setGeneric (name='interval', def=function(x, ...) standardGeneric('interval'))

