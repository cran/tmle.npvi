\name{getSample}
\alias{getSample}
\title{Generates Simulated Data}
\description{Generates a  run of  simulated observations of  the form  \eqn{(W,X,Y)} to
investigate  the  "effect"  of  \eqn{X}  on \eqn{Y}  taking  \eqn{W}  into
account.}
\usage{getSample(n, O, lambda0, p = rep(1/3, 3), omega = rep(1/2, 3), 
    Sigma1 = matrix(c(1, 1/sqrt(2), 1/sqrt(2), 1), 2, 2), sigma2 = omega[2]/5, 
    Sigma3 = Sigma1, f = identity, verbose = FALSE)}
\arguments{
  \item{n}{An \code{integer}, the number of observations to be generated.}
  \item{O}{A 3x3 numeric \code{matrix} or \code{data.frame}. Rows are 3 baseline
observations used as "class centers" for the simulation.
Columns are:
\itemize{
\item{\eqn{W}, baseline   covariate   (e.g.   DNA   methylation),   or
"confounder" in a causal model.}
\item{\eqn{X}, continuous exposure variable (e.g. DNA copy number), or
"cause" in a causal model ,  with a reference value \eqn{x_0} equal to
\code{O[2, "X"]}.}
\item{\eqn{Y}, outcome  variable  (e.g.  gene  expression  level),  or
"effect" in a causal model.}  }}
  \item{lambda0}{A  \code{function}  that  encodes  the relationship  between  \eqn{W}  and
\eqn{Y}  in observations  with levels  of \eqn{X}  equal to  the reference
value \eqn{x_0}.}
  \item{p}{A \code{vector} of  length 3 whose entries sum to  1. Entry \code{p[k]} is
the probability that each observation  belong to class \code{k} with class
center \code{O[k, ]}. Defaults to \code{rep(1/3, 3)}.}
  \item{omega}{A \code{vector}  of length 3 with positive  entries. Entry \code{omega[k]}
is  the  standard deviation  of  \eqn{W} in  class  \code{k}  (on a  logit
scale). Defaults to \code{rep(1/2, 3)}.}
  \item{Sigma1}{A 2x2 covariance  \code{matrix} of the random vector  \eqn{(X,Y)} in class
\code{k=1}, assumed  to be bivariate Gaussian with  mean \code{O[1, c("X",
"Y")]}. Defaults to \code{matrix(c(1, 1/sqrt(2), 1/sqrt(2), 1), 2, 2)}.}
  \item{sigma2}{A positive \code{numeric}, the variance  of the random variable \eqn{Y} in
class \code{k=2},  assumed to be univariate Gaussian  with mean \code{O[2,
"Y"]}. Defaults to \code{omega[2]/5}.}
  \item{Sigma3}{A 2x2 covariance  \code{matrix} of the random vector  \eqn{(X,Y)} in class
\code{k=3}, assumed  to be bivariate Gaussian with  mean \code{O[3, c("X",
"Y")]}. Defaults to \code{Sigma1}.}
  \item{f}{A \code{function} involved in the  definition of the parameter of interest
\eqn{\psi},  which must  satisfy  \eqn{f(0)=0} (see  Details). Defaults  to
\code{identity}.}
  \item{verbose}{Prescribes the amount of information  output by the function.  Defaults to
\code{FALSE}.}
}
\details{The  parameter of interest is  defined as \eqn{\psi=\Psi(P)}
with   \deqn{\Psi(P)   =   \frac{E_P[f(X-x_0)  *   (\theta(X,W)   -
\theta(x_0,W))]}{E_P[f(X-x_0)^2]},}{\Psi(P)    =   E_P[f(X-x_0)   *
(\theta(X,W) - \theta(x_0,W))] / E_P[f(X-x_0)^2],} with \eqn{P} the
distribution of the random vector \eqn{(W,X,Y)}, \eqn{\theta(X,W) =
E_P[Y|X,W]}, \eqn{x_0} the reference value for \eqn{X}, and \eqn{f}
a   user-supplied   function    such   that   \eqn{f(0)=0}   (e.g.,
\eqn{f=identity},  the  default value).   The  value \eqn{\psi}  is
obtained  using   the  \bold{known}  \eqn{\theta}   and  the  joint
empirical  distribution of  \eqn{(X,W)} based  on the  same  run of
observations  as  in  \code{obs}.   Seeing  \eqn{W, X,  Y}  as  DNA
methylation, DNA copy number and gene expression, respectively, the
simulation scheme implements the following constraints:
\itemize{
\item There are two or  three copy number classes: normal regions
(\code{k=2}), and  regions of  copy number gains  and/or losses
(\code{k=1} and/or \code{k=3}).
\item In  normal regions,  gene expression levels  are negatively
correlated with DNA methylation.
\item  In regions  of  copy number  alteration,  copy number  and
expression are positively correlated.  }}
\value{Returns a list with the following tags:
\item{obs}{A \code{matrix} of \code{n} observations. The \code{c(W,X,Y)}
colums of \code{obs} have the  same interpretation as the columns of the
input argument \code{O}.}
\item{psi}{A  \code{numeric}, approximated value  of the  true parameter
\eqn{\psi} obtained  using the  \bold{known} \eqn{\theta} and  the joint
empirical  distribution  of  \eqn{(X,W)}   based  on  the  same  run  of
observations  as in  \code{obs}. The  larger the  sample size,  the more
accurate the approximation.}
\item{g}{A   \code{function},  the  \bold{known}   positive  conditional
probability   \eqn{P(X=x_0|W)}.}
\item{mu}{A  \code{function}, the  \bold{known}  conditional expectation
\eqn{E_P(X|W)}.}
\item{muAux}{A \code{function}, the \bold{known} conditional expectation
\eqn{E_P(X|X\neq x_0, W)}.}
\item{theta}{A \code{function}, the \bold{known} conditional expectation
\eqn{E_P(Y|X,W)}.}
\item{theta0}{A    \code{function},    the   \bold{known}    conditional
expectation \eqn{E_P(Y|X=x_0,W)}.}
\item{sigma2}{A  positive \code{numeric},  the  \bold{known} expectation
\eqn{E_P(f(X-x_0)^2)}.}
\item{effIC}{A  \code{function},  the  \bold{known} efficient  influence
curve of  the functional \eqn{\Psi} at \eqn{P},  \bold{assuming} that the
reference value \eqn{x_0=0}.}
\item{varIC}{A  positive  \code{numeric},   approximated  value  of  the
variance of the efficient influence curve of the functional \eqn{\Psi} at
\eqn{P}  and  evaluated at  \eqn{O},  obtained  using  the same  run  of
observations  as in  \code{obs}. The  larger the  sample size,  the more
accurate the approximation. }}
\references{Chambaz, A., Neuvial, P., & van der Laan, M. J. (2012).
Estimation of a non-parametric variable importance measure of a
continuous exposure. Electronic journal of statistics, 6, 1059--1099.}
\author{Antoine Chambaz, Pierre Neuvial}




\examples{
## Parameters for the simulation (case 'f=identity')
O <- cbind(W=c(0.05218652, 0.01113460),
           X=c(2.722713, 9.362432),
           Y=c(-0.4569579, 1.2470822))
O <- rbind(NA, O)
lambda0 <- function(W) {-W}
p <- c(0, 1/2, 1/2)
omega <- c(0, 3, 3)
S <- matrix(c(10, 1, 1, 0.5), 2 ,2)

## Simulating a data set of 200 i.i.d. observations
sim <- getSample(2e2, O, lambda0, p=p, omega=omega, sigma2=1, Sigma3=S)
str(sim)

obs <- sim$obs
head(obs)
pairs(obs)

## Adding (dummy) baseline covariates
V <- matrix(runif(3*nrow(obs)), ncol=3)
colnames(V) <- paste("V", 1:3, sep="")
obsV <- cbind(V, obs)
head(obsV)

## True psi and confidence intervals (case 'f=identity')      
sim01 <- getSample(1e4, O, lambda0, p=p, omega=omega, sigma2=1, Sigma3=S)
truePsi1 <- sim01$psi

confInt01 <- truePsi1+c(-1, 1)*qnorm(.975)*sqrt(sim01$varIC/nrow(sim01$obs))
confInt1 <- truePsi1+c(-1, 1)*qnorm(.975)*sqrt(sim01$varIC/nrow(obs))
msg <- "\nCase f=identity:\n"
msg <- c(msg, "\ttrue psi is: ", paste(signif(truePsi1, 3)), "\n")
msg <- c(msg, "\t95\%-confidence interval for the approximation is: ", 
         paste(signif(confInt01, 3)), "\n")
msg <- c(msg, "\toptimal 95\%-confidence interval is: ", 
         paste(signif(confInt1, 3)), "\n")
cat(msg)

## True psi and confidence intervals (case 'f=atan')
f2 <- function(x) {1*atan(x/1)}
sim02 <- getSample(1e4, O, lambda0, p=p, omega=omega, sigma2=1, Sigma3=S, f=f2);
truePsi2 <- sim02$psi;

confInt02 <- truePsi2+c(-1, 1)*qnorm(.975)*sqrt(sim02$varIC/nrow(sim02$obs))
confInt2 <- truePsi2+c(-1, 1)*qnorm(.975)*sqrt(sim02$varIC/nrow(obs))

msg <- "\nCase f=atan:\n"
msg <- c(msg, "\ttrue psi is: ", paste(signif(truePsi2, 3)), "\n")
msg <- c(msg, "\t95\%-confidence interval for the approximation is: ", 
         paste(signif(confInt02, 3)), "\n")
msg <- c(msg, "\toptimal 95\%-confidence interval is: ", 
         paste(signif(confInt2, 3)), "\n")
cat(msg)
}
