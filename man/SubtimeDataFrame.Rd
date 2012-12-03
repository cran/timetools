\name{SubtimeDataFrame}
\alias{SubtimeDataFrame}
\title{Create a SubtimeDataFrame from scratch}
\usage{
  SubtimeDataFrame(when, data = NULL, ...)
}
\arguments{
  \item{when}{\code{\link{POSIXst}}.}

  \item{data}{a data.frame with as much rows as the length
  of \sQuote{when}. Can be \code{NULL} (hence the
  data.frame has zero column and as much rows as needed).}

  \item{\dots}{arguments to or from other methods}
}
\value{
  a \code{\link[=SubtimeDataFrame-class]{SubtimeDataFrame}}
  object.
}
\description{
  To see all methods related to this class, see
  \code{\link{SubtimeDataFrame-class}}
}
\details{
  If both \code{when} and \code{data} are given,
  \code{data} must have a number of rows identical to the
  length of \code{when}.

  To access to the class documentation, type in the R
  console :

  \code{class?SubtimeDataFrame}
}
\examples{
st <- POSIXst (1:4, 'day', 'week')
SubtimeDataFrame (st, data.frame (test=sample (0:100, 4)))
}
\seealso{
  \code{\link[=SubtimeDataFrame-class]{SubtimeDataFrame}},
  \code{\link{POSIXst}}, \code{\link{timetools}}
}

