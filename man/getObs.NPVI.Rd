\name{getObs.NPVI}
\alias{getObs.NPVI}
\alias{getObs}
\title{Retrieves the Observations}
\description{Retrieves the \code{matrix} of observations involved in the TMLE procedure.}
\usage{\method{getObs}{NPVI}(this, tabulate, ...)}
\arguments{
  \item{this}{An object of class \code{TMLE.NPVI}.}
  \item{tabulate}{A \code{logical}, to  specify whether it is the original  data set that is
retrieved (if \code{FALSE}) or a  tabulated version of it (otherwise), for
internal use only.  If \code{tabulate} is missing then  the value attached
to the input object is used.}
  \item{\dots}{Not used.}
}

\value{Either the original data set involved in the TMLE procedure or a tabulated
version of it.}

\author{Antoine Chambaz, Pierre Neuvial}



\seealso{tmle.npvi}
\examples{FALSE}
