\name{timetools-package}
\alias{timetools-package}
\alias{timetools}
\docType{package}
\title{
provides objects and tools to manipulate irregular heterogeneous time data and subtime data.
}
\description{
This package is intended to manipulate irregular time series such as subtime series,
series based on time intervals, etc. This package uses only POSIXt* format.

Main things defined in this package are :
\enumerate{
  \item the classes POSIXcti (as \sQuote{time intervals}), POSIXctp (as \sQuote{time periods}) and
  subtime (days of week, weeks of year, months of year, etc.), to deal with basic time concepts ;
  \item TimeIntervalDataFrame, TimeInstantDataFrame and SubtimeDataFrame to deal with time series
  (regular or not) ;
  \item methods to switch from one class to another ;
  \item and the \sQuote{changeSupport} method for TimeIntervalDataFrame (cf
  \code{\link{changeSupport}} for more details).
}
}
\details{
\tabular{ll}{
Package: \tab timetools\cr
Type: \tab Package\cr
Version: \tab 1.1-3\cr
Date: \tab 2012-05-12\cr
Depends: \tab methods\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
URL: \tab http://sourceforge.net/projects/timetools/\cr
Encoding: \tab UTF-8\cr
}

Index:
\preformatted{
%included%              test inclusion of 2 'interval' objects
%intersect%             intersects 2 'interval' objects
RegularTimeIntervalDataFrame
                        Create a regular TimeIntervalDataFrame from
                        scratch
SubtimeDataFrame-class
                        Class '"SubtimeDataFrame"'
TimeInstantDataFrame-class
                        Class '"TimeInstantDataFrame"'
TimeIntervalDataFrame   Create a TimeIntervalDataFrame from scratch
TimeIntervalDataFrame-class
                        Class '"TimeIntervalDataFrame"'
as.SubtimeDataFrame     Convert an object to a SubtimeDataFrame
as.TimeInstantDataFrame
                        Convert an object to a TimeInstantDataFrame
as.TimeIntervalDataFrame
                        Convert an object to a TimeIntervalDataFrame
changeSupport           Function to change time support of
                        TimeIntervalDataFrame
compute.lim             Calculate limits for plotting
subtime                 Extract a specific subtime of of POSIXt object
                        or define a subtime object
timetools-package       provides objects and tools to manipulate
                        irregular heterogeneous time data and subtime
                        data.
timezone                Test different properties of Time objects
as.POSIXctp		Convert an object to a POSIXctp
ops.posixctpi		Operators to sum or substract POSIXctp from POSIXct or POSIXcti
origin			1970-01-01 GMT
POSIXt.units		define valid units for time objects
posix.properties	Extract time properties from POSIX* objects
as.POSIXcti		Convert an object to a POSIXcti
}
}
\author{
Vladislav Navel <vnavel@yahoo.fr>
}
\keyword{ package }
\seealso{
 \code{\link{TimeInstantDataFrame}}, \code{\link{TimeIntervalDataFrame}},
 \code{\link{changeSupport}}, 
 \code{\link{SubtimeDataFrame}}, \code{\link{POSIXcti}}, \code{\link{POSIXctp}}
}
\examples{
ti1 <- TimeIntervalDataFrame (
	c('2010-01-01', '2010-02-01'), c('2010-02-01', '2010-02-02'),
	'UTC', data.frame(ex1=1:2) )

ti2 <- TimeIntervalDataFrame (
	c('2010-01-01', '2010-02-01', '2010-02-02'), NULL,
	'UTC', data.frame(ex1=1:2) )

all.equal (ti1, ti2)

ti3 <- TimeIntervalDataFrame (
	c('2010-01-01', '2010-01-02', '2010-01-04'), NULL,
	'UTC', data.frame(ex3=c(6, 1.5)))

# weighted mean over a period of 3 days with at least 75% of
# coverage (NA is retunr if not)
ti3
d <- POSIXctp(unit='day')
changeSupport (ti3, 3L*d, 0.75)


ti4 <- TimeIntervalDataFrame (
	c('2010-01-01', '2010-01-02', '2010-01-04', 
	  '2010-01-07', '2010-01-09', '2010-01-10'), NULL,
	'UTC', data.frame(ex4=c(6, 1.5, 5, 3, NA)))

# weighted mean over a period of 3 days with at least 75% of
# coverage (NA is retunr if not) or 50%
ti4
changeSupport (ti4, 3L*d, 0.75)
changeSupport (ti4, 3L*d, 0.5)


}
