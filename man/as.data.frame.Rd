\name{as.data.frame}
\alias{as.data.frame}
\alias{as.data.frame.TimeInstantDataFrame}
\alias{as.data.frame.TimeIntervalDataFrame}
\alias{as.data.frame.SubtimeDataFrame}
\title{Convert an object to a data.frame}
\usage{
  \method{as.data.frame}{TimeInstantDataFrame}(x, row.names=NULL, optional=FALSE,
  	include.dates=FALSE, ...)
  \method{as.data.frame}{TimeIntervalDataFrame}(x, row.names=NULL, optional=FALSE,
  	include.dates=FALSE, ...)
  \method{as.data.frame}{SubtimeDataFrame}(x, row.names=NULL, optional=FALSE,
  	include.dates=FALSE, ...)
}

\arguments{
  \item{x}{TimeIntervalDataFrame, TimeInstantDataFrame or SubtimeDataFrame}
  \item{row.names}{
    ‘NULL’ or a character vector giving the row names for the
    data frame.  Missing values are not allowed.}
  \item{optional}{
    logical. If ‘TRUE’, setting row names and converting column
    names (to syntactic names: see ‘make.names’) is optional.}
  \item{include.dates}{should time properties be included in the data.frame 
    as a column ? (or 2 columns for TimeIntervalDataFrame)}
  \item{\dots}{additional arguments to be passed to or from methods.}
}
\value{
  a \code{\link[base]{data.frame}}
}
\description{
  Convert an object to a \code{\link[base]{data.frame}}.
}


