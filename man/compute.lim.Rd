\name{compute.lim}
\alias{compute.lim}
\title{Calculate limits for plotting}
\usage{
  compute.lim(x, na.rm = FALSE)
}
\arguments{
  \item{x}{'numeric' for which limits must be calculated}

  \item{na.rm}{boolean should NA values be removed before
  calculation ?}
}
\value{
  numeric of length 2
}
\description{
  This function retunrn a 2 elements vectors (numeric)
  which can be use as graph limits (xlim, ylim, rlim, etc.)
}

