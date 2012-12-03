\name{changeSupport}
\alias{changeSupport}
\alias{changeSupport,TimeIntervalDataFrame,character,numeric,ANY,ANY,missing,missing-method}
\alias{changeSupport,TimeIntervalDataFrame,POSIXctp,numeric,ANY,ANY,missing,missing-method}
\alias{changeSupport,TimeIntervalDataFrame,TimeIntervalDataFrame,numeric,ANY,ANY,ANY,ANY-method}
\title{Function to change time support of TimeIntervalDataFrame}
\usage{
  changeSupport(from, to, min.coverage, FUN = NULL,
    weights.arg = NULL, split.from = FALSE,
    merge.from = TRUE, ...)
}
\arguments{
  \item{from}{\code{\link{TimeIntervalDataFrame}} for wich
  the time support is to change}

  \item{to}{an object indicating the new support, see
  specific sections}

  \item{min.coverage}{a numeric between 0 and 1 indicating
  the for}

  \item{FUN}{function use to agregate data of from. By
  default \code{\link[base]{mean}} if \sQuote{from} is
  \code{\link{homogeneous}}.
  \code{\link[stats]{weighted.mean}} otherwise.}

  \item{weights.arg}{if FUN has a \sQuote{weight} argument,
  this parameter should a character naming the weight
  argument. For instance, if FUN is
  \code{\link[stats]{weighted.mean}}, then weights.arg is
  \code{'w'}.}

  \item{\dots}{arguments for FUN or for other methods}

  \item{split.from}{logical indicating if data in
  \sQuote{from} can be used for several intervals of the
  new time support (see \sQuote{details}).}

  \item{merge.from}{logical indicating if data in
  \sQuote{from} can be can be merged over interval of the
  new time support.}
}
\value{
  \code{\link{TimeIntervalDataFrame}}
}
\description{
  Methods that allows to agregate AND disagregate
  homogeneous AND heterogeneous time data.
}
\details{
  Agregating homogeneous data is for example to calculate
  daily means of time series from hourly time series.

  Agregating heterogeneous data is for example to calculate
  annual means of time series from monthly time series
  (because each month doesn(t have identical weight).

  In above cases, the \code{min.coverage} allows to control
  if means should be calculated or not : for the monthly
  case, if there are \code{NA} values and the time coverage
  of \sQuote{not NA} values is lower \code{min.coverage}
  the result will be \code{NA} ; if time coverage is higher
  than \code{min.coverage}, the annual mean will be
  \sQuote{estimated} by the mean of available data.

  Disagregating data is more \sQuote{articficial} and is
  disabled by default (with the \code{split.from}
  argument). This argument is also used to precise if one
  value can be use for agregation in more than one interval
  in the resulting TimeIntervalDataFrame (for sliding
  intervals for instance). Here are some examples of time
  disagregration : \itemize{ \item A weekly mean can be
  dispatched over the days of the week.  By default, the
  value attribuated to each day is the value of the week,
  but this can be changed by using a special function
  (\code{FUN} argument). \item The value of a variable is
  known from monday at 15 hours to tuesday at 15 hours and
  from tuesday at 15 hours to wednesday at 15 hours. To
  \sQuote{evaluate} the value of the variable for tuesday
  can be estimated by doing a weigthed mean between the two
  values. Weights are determined by the intersection
  between each interval and tuesday. Here weights will be
  \code{0.625} (15/24) and \code{0.375} (9/24) (In this
  case, disagration is combined with a
  \sQuote{reagregation}). }

  These are \sQuote{trivial} examples but many other usage
  can be found for these methods. Other functions than
  \code{weighted.mean} or \code{mean} can be used. The Qair
  package (in its legislative part) gives several examples
  of usage (this package is not availables on CRAN but see
  \sQuote{references} to know where you can find it).
}
\section{from=TimeIntervalDataFrame, to=TimeIntervalDataFrame}{
  \code{to} is a TimeIntervalDataFrame. The method will try
  to adapt data of \code{from} over interval of \code{to}.
  The returned object is the \code{to}
  TimeIntervalDataFrame with new columns corresponding of
  those of \code{from}.

  If merge.from is TRUE, values affected for each interval
  of \code{to} will be calculated with all data in the
  interval. If split.from is TRUE, values partially in the
  interval will also be used for calculation.

  If merge.from is FALSE, values affected for each interval
  of \code{to} will be the one inside this interval. If
  several values are inside the interval, \code{NA} will be
  affected. If split.from is TRUE, a value partially inside
  the interval is considered as being inside it. So if
  there is no other values in the interval, this value will
  be affected, else \code{NA} will be affected.
}

\section{from=TimeIntervalDataFrame, to=character}{
  \code{to} is one of 'year', 'month', 'day', 'hour',
  'minute' or 'second'. It defines the period
  (\code{\link{POSIXctp}}) to use to build the new
  TimeIntervalDataFrame on which \code{from} will be
  agregated (or disagregated).

  So first, an \sQuote{empty} (no data)
  TimeIntervalDataFrame is created, and then, the
  agregation is done accordingly to the
  \sQuote{from=TimeIntervalDataFrame,
  to=TimeIntervalDataFrame} section.
}

\section{from=TimeIntervalDataFrame, to=POSIXctp}{
  \code{to} is period (see \code{\link{POSIXctp}}). It
  defines the base of the new TimeIntervalDataFrame on
  which \code{from} will be agregated (or disagregated).

  So first, an \sQuote{empty} (no data)
  TimeIntervalDataFrame is created, and then, the
  agregation is done accordingly to the
  \sQuote{from=TimeIntervalDataFrame,
  to=TimeIntervalDataFrame} section.
}
\examples{
ti3 <- TimeIntervalDataFrame (
       c('2010-01-01', '2010-01-02', '2010-01-04'), NULL,
       'UTC', data.frame(ex3=c(6, 1.5)))

# weighted mean over a period of 3 days with at least 75\% of
# coverage (NA is retunr if not)
ti3
d <- POSIXctp(unit='day')
changeSupport (ti3, 3L*d, 0.75)

ti4 <- TimeIntervalDataFrame (
	c('2010-01-01', '2010-01-02', '2010-01-04',
	  '2010-01-07', '2010-01-09', '2010-01-10'), NULL,
         'UTC', data.frame(ex4=c(6, 1.5, 5, 3, NA)))

# weighted mean over a period of 3 days with at least 75\% of
# coverage (NA is retunr if not) or 50\%
ti4
changeSupport (ti4, 3L*d, 0.75)
changeSupport (ti4, 3L*d, 0.5)

# use of split.from
ti1 <- RegularTimeIntervalDataFrame('2011-01-01', '2011-02-01', 'hour')
ti1$value <- 1:nrow(ti1)
# we can calculate sliding mean over periods of 24 hours.
# first lets build the corresponding TimeIntervalDataFrame
ti2 <- RegularTimeIntervalDataFrame('2011-01-01', '2011-02-01', 'hour', 'day')
# if we try to 'project' ti1 over ti2 it won't work :
summary (changeSupport (ti1, ti2, 0))
# all data are NA because 'spliting' is not enabled. Let's enable it :
summary (changeSupport (ti1, ti2, 0, split.from=TRUE))
}
\references{
  Qair-package :
  \url{http://sourceforge.net/projects/packagerqair/}
}
\seealso{
  \code{\link{TimeIntervalDataFrame}},
  \code{\link{POSIXcti}}
}

