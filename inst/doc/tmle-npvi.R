### R code from vignette source 'tmle-npvi.Rnw'

###################################################
### code chunk number 1: tmle-npvi.Rnw:83-87
###################################################
library("tmle.npvi")
library("R.utils")
log <- Arguments$getVerbose(-8, timestamp=TRUE)
set.seed(12345)


###################################################
### code chunk number 2: tmle-npvi.Rnw:124-135
###################################################
O <- cbind(W=c(0.05218652, 0.01113460),
           X=c(2.722713, 9.362432),
           Y=c(-0.4569579, 1.2470822))
O <- rbind(NA, O)

lambda0 <- function(W) {-W}

p <- c(0, 1/2, 1/2)
omega <- c(0, 3, 3)
S <- matrix(c(10, 1, 1, 0.5), 2 ,2)
n <- 200


###################################################
### code chunk number 3: tmle-npvi.Rnw:141-144
###################################################
sim <- getSample(n, O, lambda0, p=p, omega=omega, sigma2=1, Sigma3=S)
obs <- sim$obs
head(obs)


###################################################
### code chunk number 4: tmle-npvi.Rnw:153-157
###################################################
V <- matrix(runif(3*nrow(obs)), ncol=3)
colnames(V) <- paste("V", 1:3, sep="")
obs <- cbind(V, obs)
head(obs)


###################################################
### code chunk number 5: tmle-npvi.Rnw:180-194
###################################################
sim <- getSample(1e4, O, lambda0, p=p, omega=omega, 
                 sigma2=1, Sigma3=S, verbose=log)
truePsi <- sim$psi

confInt0 <- truePsi + c(-1, 1)*qnorm(.975)*sqrt(sim$varIC/nrow(sim$obs))
confInt <- truePsi + c(-1, 1)*qnorm(.975)*sqrt(sim$varIC/nrow(obs))

msg <- "\nCase f=identity:\n"
msg <- c(msg, "\ttrue psi is: ", paste(signif(truePsi, 3)), "\n")
msg <- c(msg, "\t95%-confidence interval for the approximation is: ", 
         paste(signif(confInt0, 3)), "\n")
msg <- c(msg, "\toptimal 95%-confidence interval is: ", 
         paste(signif(confInt, 3)), "\n")
cat(msg)


###################################################
### code chunk number 6: tmle-npvi.Rnw:200-214
###################################################
sim2 <- getSample(1e4, O, lambda0, p=p, omega=omega, 
                  sigma2=1, Sigma3=S, f=atan, verbose=log)
truePsi2 <- sim2$psi

confInt02 <- truePsi2 + c(-1, 1)*qnorm(.975)*sqrt(sim2$varIC/nrow(sim2$obs))
confInt2 <- truePsi2 + c(-1, 1)*qnorm(.975)*sqrt(sim2$varIC/nrow(obs))

msg <- "\nCase f=atan:\n"
msg <- c(msg, "\ttrue psi is: ", paste(signif(truePsi2, 3)), "\n")
msg <- c(msg, "\t95%-confidence interval for the approximation is: ", 
         paste(signif(confInt02, 3)), "\n")
msg <- c(msg, "\toptimal 95%-confidence interval is: ", 
         paste(signif(confInt2, 3)), "\n")
cat(msg)


###################################################
### code chunk number 7: tmle-npvi.Rnw:227-232
###################################################
X0 <- O[2,2]
obsC <- obs
obsC[, "X"] <- obsC[, "X"] - X0
obs <- obsC
head(obs)


###################################################
### code chunk number 8: tmle-npvi.Rnw:244-246
###################################################
npvi <- tmle.npvi(obs, f=identity, flavor="learning")
npvi


###################################################
### code chunk number 9: tmle-npvi.Rnw:265-267
###################################################
setConfLevel(npvi, 0.9)
npvi


###################################################
### code chunk number 10: tmle-npvi.Rnw:274-276
###################################################
history <- getHistory(npvi)
print(round(history, 4))


###################################################
### code chunk number 11: confInt
###################################################
hp <- history[, "psi"]
hs <- history[, "sic"]
hs[1] <- NA
ics <-  c(-1,1) %*% t(qnorm(0.975)*hs/sqrt(nrow(getObs(npvi))))

pch <- 20
ylim <- range(c(confInt, hp, ics+hp), na.rm=TRUE)

xs <- (1:length(hs))-1
plot(xs, hp, ylim=ylim, pch=pch, xlab="Iteration", ylab=expression(psi[n]),
     xaxp=c(0, length(hs)-1, length(hs)-1))
dummy <- sapply(seq(along=xs), function(x) lines(c(xs[x],xs[x]), hp[x]+ics[, x]))

abline(h=confInt, col="blue")
abline(h=confInt0, col="red")


###################################################
### code chunk number 12: tmle-npvi.Rnw:318-319
###################################################
sessionInfo()


