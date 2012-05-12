setOldClass ('subtime')

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
#' @inheritParams subtime
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
setGeneric ('day', function (x, of, ..., first.day=0) standardGeneric ('day') )
#' @rdname posix.properties
hour <- function(x, of, ..., first.day=0) UseMethod('hour')
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
#' @param from object to convert to a TimeInstantDataFrame
#' @param \dots more args to or from other methods or FUN
#' @return a \code{\link{SubtimeDataFrame}}
as.SubtimeDataFrame  <- function(from, ...) UseMethod ('as.SubtimeDataFrame')

# generic pour toutes les classes Time*DataFrame
#-----------------------------------------------
# S4
setGeneric (name='nrow')
setGeneric (name='ncol')
#setGeneric (name='split')
setGeneric (name='lapply')

#' @rdname time.properties
continuous <- function(x, ...) UseMethod('continuous')
#' Test or extract different properties of Time objects
#' 
#' These functions can be applied to Time objects such as
#' \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}},
#' \code{\link[=TimeInstantDataFrame-class]{TimeInstantDataFrame}} or
#' \code{\link{subtime}}.
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

#' @rdname changeSupport
changeSupport <- function(from,to,min.coverage,FUN=NULL,weights.arg=NULL, ...,split.from=FALSE,merge.from=TRUE)
UseMethod ('changeSupport')
#' Function to change time support of TimeIntervalDataFrame
#'
#' Methods that allows to agregate AND disagregate
#' homogeneous AND heterogeneous time data.
#' 
#' Agregating homogeneous data is for example to calculate
#' daily means of time series from hourly time series.
#'
#' Agregating heterogeneous data is for example to calculate
#' annual means of time series from monthly time series (because each
#' month doesn(t have identical weight).
#'
#' In above cases, the \code{min.coverage} allows to control
#' if means should be calculated or not : for the monthly case,
#' if there are \code{NA} values and the time coverage of \sQuote{not NA}
#' values is lower \code{min.coverage} the result will be \code{NA} ;
#' if time coverage is higher than \code{min.coverage}, the 
#' annual mean will be \sQuote{estimated} by the mean of available
#' data.
#'
#' Disagregating data is more \sQuote{articficial} and is 
#' disabled by default (with the \code{split.from} argument).
#' This argument is also used to precise if one value can
#' be use for agregation in more than one interval in the resulting
#' TimeIntervalDataFrame (for sliding intervals for instance).
#' Here are some examples of time disagregration :
#' \itemize{
#' \item A weekly mean can be dispatched over the days of the week.
#'  By default, the value attribuated to each day is the value 
#' of the week, but this can be changed by using a special function
#' (\code{FUN} argument).
#' \item The value of a variable is known from monday at 15 hours to 
#' tuesday at 15 hours and from tuesday at 15 hours to wednesday at 15 hours.
#' To \sQuote{evaluate} the value of the variable for tuesday can be 
#' estimated by doing a weigthed mean between the two values. Weights are
#' determined by the intersection between each interval and tuesday.
#' Here weights will be \code{0.625} (15/24) and \code{0.375} (9/24)
#' (In this case, disagration is combined with a \sQuote{reagregation}).
#' }
#'
#' These are \sQuote{trivial} examples but many other usage can be found
#' for these methods. Other functions than \code{weighted.mean}
#' or \code{mean} can be used.
#' The Qair package (in its legislative part) gives several examples of usage
#' (this package is not availables on CRAN but see \sQuote{references}
#' to know where you can find it).
#'
#' @examples
#'ti3 <- TimeIntervalDataFrame (
#'        c('2010-01-01', '2010-01-02', '2010-01-04'), NULL,
#'        'UTC', data.frame(ex3=c(6, 1.5)))
#'
#'# weighted mean over a period of 3 days with at least 75% of
#'# coverage (NA is retunr if not)
#'ti3
#'d <- POSIXctp(unit='day')
#'changeSupport (ti3, 3L*d, 0.75)
#'
#'ti4 <- TimeIntervalDataFrame (
#'	c('2010-01-01', '2010-01-02', '2010-01-04',
#'	  '2010-01-07', '2010-01-09', '2010-01-10'), NULL,
#'          'UTC', data.frame(ex4=c(6, 1.5, 5, 3, NA)))
#'
#'# weighted mean over a period of 3 days with at least 75% of
#'# coverage (NA is retunr if not) or 50%
#'ti4
#'changeSupport (ti4, 3L*d, 0.75)
#'changeSupport (ti4, 3L*d, 0.5)
#'
#'# use of split.from
#'ti1 <- RegularTimeIntervalDataFrame('2011-01-01', '2011-02-01', 'hour')
#'ti1$value <- 1:nrow(ti1)
#'# we can calculate sliding mean over periods of 24 hours.
#'# first lets build the corresponding TimeIntervalDataFrame
#'ti2 <- RegularTimeIntervalDataFrame('2011-01-01', '2011-02-01', 'hour', 'day')
#'# if we try to 'project' ti1 over ti2 it won't work :
#'summary (changeSupport (ti1, ti2, 0))
#'# all data are NA because 'spliting' is not enabled. Let's enable it :
#'summary (changeSupport (ti1, ti2, 0, split.from=TRUE))
#'
#' @param from \code{\link{TimeIntervalDataFrame}} for wich
#' the time support is to change
#' @param to an object indicating the new support, see specific sections
#' @param min.coverage a numeric between 0 and 1 indicating the 
#' for 
#' @param FUN function use to agregate data of from. 
#' By default \code{\link[base]{mean}} if \sQuote{from} is 
#' \code{\link{homogeneous}}. \code{\link[stats]{weighted.mean}} otherwise.
#' @param weights.arg if FUN has a \sQuote{weight} argument, this parameter should
#' a character naming the weight argument. For instance, if FUN is 
#' \code{\link[stats]{weighted.mean}}, then weights.arg is \code{'w'}.
#' @param \dots arguments for FUN or for other methods
#' @param split.from logical indicating if data in \sQuote{from} can be
#' used for several intervals of the new time support (see \sQuote{details}).
#' @param merge.from logical indicating if data in \sQuote{from} can be
#' can be merged over interval of the new time support.
#' 
#' @return \code{\link{TimeIntervalDataFrame}}
#' @references Qair-package : \url{http://sourceforge.net/projects/packagerqair/}
#'
#' @seealso \code{\link{TimeIntervalDataFrame}}, \code{\link{POSIXcti}}
setGeneric (name='changeSupport', def=function(from, to, min.coverage,
					       FUN=NULL, weights.arg=NULL, ...,
					       split.from=FALSE, merge.from=TRUE)
	    standardGeneric('changeSupport') )
 
