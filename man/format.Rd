\name{format}
\alias{format.POSIXctp}
\alias{format.POSIXcti}
\alias{format.POSIXst}
\title{format a POSIX*t* object}
\usage{
  \method{format}{POSIXctp} (x, ...)

  \method{format}{POSIXcti} (x, format = '\%Y-\%m-\%d \%H:\%M:\%S', ...)

  \method{format}{POSIXst} (x, format = NULL, ...)
}
\arguments{
  \item{x}{POSIX*t* object to format}

  \item{format}{Character string to precise the desired format.}

  \item{\dots}{other arguments to or from other methods}
}

\description{
  format a POSIX*t* (\link{POSIXctp}, \link{POSIXcti}, or
  \link{POSIXst}) object.
}

\details{
  For 'POSIXctp', the format method return a character :
  'duration unit' (for instance : "1 year", "2 minutes", etc.).

  'POSIXcti' are formatted by pasting the character strings 
  representing both start and end of each intervals. 
  Start and end's format can be specified with the \code{format}
  argument according to the basic
  \link[base]{format.POSIXct} function.

  'POSIXst' are formatted according to the \code{format}
  argument which must respect the following rules.

  \itemize{
	  \item \%v value
	  \item \%s subtime unit (slot \sQuote{unit})
	  \item \%m main unit (slot \sQuote{of})
	  \item \%a Abbreviated weekday name in the current locale.
	  \item \%A Full weekday name in the current locale.
	  \item \%b Abbreviated month name in the current locale.
	  \item \%B Full month name in the current locale.
	  \item \%r timezone
  }
}

\value{
  character string
}

