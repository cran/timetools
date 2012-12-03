\name{format.POSIXst}
\alias{format.POSIXst}
\title{format a POSIXst object}
\usage{
  \method{format}{POSIXst} (x, format = NULL, ...)
}
\arguments{
  \item{x}{POSIXst object to format}

  \item{format}{character string containing the following
  special characters. \itemize{ \item \%v value \item \%s
  subtime unit (slot \sQuote{unit}) \item \%m main unit
  (slot \sQuote{of}) \item \%a Abbreviated weekday name in
  the current locale. \item \%A Full weekday name in the
  current locale. \item \%b Abbreviated month name in the
  current locale. \item \%B Full month name in the current
  locale. \item \%r timezone }}

  \item{\dots}{other arguments to or from other methods}
}
\value{
  character string
}
\description{
  format a POSIXst object
}

