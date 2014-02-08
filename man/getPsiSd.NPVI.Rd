\name{getPsiSd.NPVI}
\alias{getPsiSd.NPVI}
\alias{getPsiSd}
\title{Returns Current Estimated Standard Deviation of the Estimator }
\description{Returns the current value of the estimated standard deviation of the current estimator. }
\usage{\method{getPsiSd}{NPVI}(this, ...)}
\arguments{
  \item{this}{An object of class \code{TMLE.NPVI}.}
  \item{\dots}{Not used.}
}

\value{Retrieves  the estimated standard deviation of the current estimator \eqn{\Psi(P_n^k)}  of the
parameter  of interest. Its computation involves  simulation of a large number of 
iid copies of \eqn{(X,W)} under \eqn{P_n^k}.  }

\author{Antoine Chambaz, Pierre Neuvial}



\seealso{tmle.npvi, getHistory, getPsi}
\examples{FALSE}
