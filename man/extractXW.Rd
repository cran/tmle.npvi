\name{extractXW}
\alias{extractXW}
\title{Removes the Y Column from Matrix of Observations}
\description{Removes the \code{Y} column from a \code{matrix} of observations.}
\usage{extractXW(mat)}
\arguments{
  \item{mat}{A   \code{matrix}  of   observations,  as   the  \code{obs}   argument  of
\code{function} \code{\link{tmle.npvi}}.    }
}
\details{Mainly for internal use.}
\value{The  \code{matrix}  extracted from  \code{mat}  by  removing the  \code{Y}
column in such a way that the first column is \code{X}.}

\author{Antoine Chambaz, Pierre Neuvial}





