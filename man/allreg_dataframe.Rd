% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformation.R
\name{allreg_dataframe}
\alias{allreg_dataframe}
\title{Make data.frame with details of all registrations}
\usage{
allreg_dataframe(regdirs = getOption("nat.templatebrains.regdirs"))
}
\arguments{
\item{regdirs}{Character vector of directories to search for registrations
(see details)}
}
\value{
data.frame with one row for each observed registration and columns
  \itemize{

  \item path

  \item name

  \item dup

  \item bridge

  \item reference

  \item sample }

  If there are no registrations, there will be a data.frame with 0 rows and
  these columns.
}
\description{
Make data.frame with details of all registrations
}
\details{
by default \code{regdirs} is set to
  getOption('nat.templatebrains.regdirs')
}
\examples{
\dontrun{
allreg_dataframe()
}
}
