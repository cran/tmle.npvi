\name{getLightFit}
\alias{getLightFit}
\title{Makes Lighter Fitted Object}
\description{Makes a lighter version of a fitted object by removing elements containing data.     }
\usage{getLightFit(fit)}
\arguments{
  \item{fit}{A fitted object.}
}
\details{Most of  the space used by a fitted object  is not necessary for
prediction.   This  concerns, for  instance,  the "residuals",  "effects",
"fitted.values",  and  "model"  entries   of  a  linear  model  fitted  by
\code{lm}.   These entries  can thus  be removed  from the  object without
affecting  the  model  predictions.    This  function  is  currently  only
implemented for fitted objects that  derive from class 'lm' or 'rpart'. It
is mainly for internal use.}
\value{Returns the same object as the input without the entries that contain data.}

\author{Antoine Chambaz, Pierre Neuvial}





