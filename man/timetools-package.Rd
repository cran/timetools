\name{timetools-package}
\alias{timetools-package}
\alias{timetools}
\docType{package}
\title{
Seasonal/Sequential (Instants/Durations, Even or not) Time Series
}
\description{
Objects to manipulate sequential and seasonal time series. Sequential time
series based on time instants and time durations are handled. Both can be
 regularly or unevenly spaced (overlapping durations are allowed).

Only POSIX* format are used for dates and times.

The following classes are provided : POSIXcti, POSIXctp,
TimeIntervalDataFrame, TimeInstantDataFrame, SubtimeDataFrame ;
methods to switch from a class to another and to modify the time support
of series (hourly time series to daily time series for instance) are also
defined.

Tools provided can be used for instance to handle environmental monitoring
data (not always produced on a regular time base).
}
\details{
\tabular{ll}{
Package: \tab timetools\cr
Type: \tab Package\cr
Version: \tab 1.7.0\cr
Date: \tab 2015-06-16\cr
Depends: \tab methods\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
URL: \tab http://sourceforge.net/projects/timetools/\cr
Encoding: \tab UTF-8\cr
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
