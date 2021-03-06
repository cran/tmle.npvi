\name{getHistory.NPVI}
\alias{getHistory.NPVI}
\alias{getHistory}
\title{Returns History of TMLE Procedure}
\description{Returns the 'history' of the TMLE procedure.}
\usage{\method{getHistory}{NPVI}(this, ...)}
\arguments{
  \item{this}{An object of class \code{TMLE.NPVI}.}
  \item{\dots}{Not used.}
}

\value{Returns a \code{numeric}  \code{matrix} which encapsulates a summary of
the  TMLE procedure.  If \eqn{k} successive  updates were performed,
then   the   \code{matrix}   has   either   \eqn{k+1}   rows   (if
\code{cleverCovTheta}  was  set  to  \code{FALSE} in  the  call  to
\code{tmle.npvi})   or    \code{2k+1}   rows   (otherwise).    The
\code{matrix} has 14 columns:
\itemize{
\item{\code{"eps"},   values of  the unique
fluctuation  parameter   (if  \code{cleverCovTheta}  was  set  to
\code{FALSE} in  the call  to \code{tmle.npvi}),  or  
values of the  parameter involved in the fluctuation of
the   joint  distribution  of  \eqn{(X,W)}   during  each  update
(otherwise).  }
\item{\code{"lli"},     increases in likelihood
yielded  by  each  update  (if  \code{cleverCovTheta}  was  set  to
\code{FALSE}  in the  call  to  \code{tmle.npvi}),  or 
increases in likelihood yielded by the fluctuation of the
joint distribution of \eqn{(X,W)} during each update (otherwise).}
\item{\code{"mic1"},   empirical means  of the  first
component of the efficient influence  curve at each step of the TMLE
procedure.}
\item{\code{"epsT"},   values of  the fluctuation
parameter involved in the fluctuation of the conditional distribution of
\eqn{Y}  given \eqn{(X,W)} during each  update (if \code{cleverCovTheta}
was  set to \code{TRUE} in  the call to  \code{tmle.npvi}), or \code{NA}
(otherwise).}
\item{\code{"lliT"},  successive increases in likelihood
yielded by  the  fluctuation of  the  conditional distribution  of
\eqn{Y}    given   \eqn{(X,W)}    during    each   update    (if
\code{cleverCovTheta}  was  set  to  \code{TRUE} in  the  call  to
\code{tmle.npvi}), or \code{NA} (otherwise).}
\item{\code{"mic2"},  empirical means  of the  second
component of the efficient influence  curve at each step of the TMLE
procedure.}
\item{\code{"psi"},   increasingly targeted
estimators \eqn{\Psi(P_n^k)} of  the  parameter  of  interest. The last one is the TMLE.  Their  computation  
involves  simulation  of  \code{B}  iid  copies  of
\eqn{(X,W)} under \eqn{P_n^k}. }
\item{\code{"psi.sd"},  estimated standard deviations of
the   increasingly targeted  estimators of  the  parameter of
interest. The last one corresponds to the TMLE. The  computation involves  the  same \code{B}  iid copies  of
\eqn{(X,W)} as above.}
\item{\code{"psiPn"},  same as \code{"psi"} except  that the *observed*
\eqn{(X_i,W_i)}  are  used  instead  of simulated  copies  drawn  from
\eqn{P_n^k}. Of course, \code{"psi"} must be favored.}
\item{\code{"psiPn.sd"},  same  as  \code{"psi.sd"}  except  that  the
*observed*  \eqn{(X_i,W_i)} are used instead of  simulated copies drawn
from \eqn{P_n^k}. Of course, \code{"psi.sd"} must be favored.}
\item{\code{"mic"},   empirical  means  of  the  efficient
influence curve at each step of the TMLE procedure. This column is the sum of the \code{"mic1"} and \code{"mic2"} columns.}
\item{\code{"div"},  total variation  distances between
each pair  of successive distributions constructed in  the course of the
TMLE procedure. }
\item{\code{"sic"},  estimated standard deviations   of  the  efficient
influence curve at each step of the TMLE procedure.}
\item{\code{"phi"},    non-parametric     substitution    estimator    of
\eqn{\phi=\Phi(P)}             where            \deqn{\Phi(P)            =
\frac{E_P[f(X)Y]}{E_P[f(X)^2]},}{\Phi(P)  =  E_P[f(X)Y]  /  E_P[f(X)^2],}
with  \eqn{P}  the  distribution   of  the  random  vector  \eqn{(W,X,Y)}. The alternative parameter \eqn{\phi} should be interpreted as the counterpart of \eqn{\psi} which neglects \eqn{W}. }
\item{\code{"sicAlt"},  estimated standard deviations   of  the  efficient
influence curve of \eqn{\Psi - \Phi} at each step of the TMLE procedure.}
}}

\author{Antoine Chambaz, Pierre Neuvial}



\seealso{tmle.npvi}
\examples{FALSE}
