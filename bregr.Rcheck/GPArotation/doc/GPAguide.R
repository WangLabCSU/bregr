### R code from vignette source 'GPAguide.Stex'

###################################################
### code chunk number 1: GPAguide.Stex:21-22
###################################################
 options(continue="  ")


###################################################
### code chunk number 2: GPAguide.Stex:35-36
###################################################
library("GPArotation")  


###################################################
### code chunk number 3: GPAguide.Stex:67-75
###################################################
data(ability.cov)
z <- factanal(factors = 2, covmat = ability.cov, rotation = "none")
# quartimax rotation
GPFRSorth(loadings(z), method = "quartimax")
quartimax(z$loadings)
# oblimin rotation
GPFRSoblq(z$loadings, method = "oblimin")
oblimin(loadings(z))


###################################################
### code chunk number 4: GPAguide.Stex:88-95
###################################################
y <- factanal(factors=3, covmat=ability.cov, rotation = "none")
y.quart <- quartimax(y$loadings)
max( loadings(y.quart) %*% t(y.quart$Th) - loadings(y) )
y.obli <- oblimin(y$loadings, normalize=TRUE, randomStarts=15)
max( loadings(y.obli) %*% t(y.obli$Th) - loadings(y) )
# last equation on Page 678
max( loadings(y.obli) - loadings(y) %*% solve(t(y.obli$Th)) )


###################################################
### code chunk number 5: GPAguide.Stex:98-101
###################################################
y <- factanal(factors=3, covmat=ability.cov, rotation = "none", randomStarts=15)
y.obli <- oblimin(y$loadings, normalize=TRUE, randomStarts=15)
max(abs(y.obli$Phi  - t(y.obli$Th) %*% y.obli$Th))


###################################################
### code chunk number 6: GPAguide.Stex:111-115
###################################################
data(Thurstone, package = "GPArotation")
infomaxQ(box26, randomStarts = 100) # 100 random starts
infomaxQ(box26, Tmat=Random.Start(3)) # a single random start
infomaxQ(box26, randomStarts = 1) # also a single random start


###################################################
### code chunk number 7: GPAguide.Stex:162-188
###################################################
origdigits <- options("digits")
options(digits = 2)
trBritain <- matrix( c(.783,-.163,.811,.202,.724,.209,.850,.064,
-.031,.592,-.028,.723,.388,.434,.141,.808,.215,.709), byrow=TRUE, ncol=2)
trGermany <- matrix( c(.778,-.066, .875,.081, .751,.079, .739,.092,
.195,.574, -.030,.807, -.135,.717, .125,.738, .060,.691), byrow=TRUE, ncol = 2)
# orthogonal rotation of trGermany towards trBritain
trx <- targetT(trGermany, Target = trBritain)
# Factor loadings after target rotation
trx
# Differences between loadings matrices after rotation
y <- trx$loadings - trBritain
print(y, digits = 1)
# Square Root of the mean squared difference per item
sqrt(apply((y^2), 1, mean))
# Square Root of the mean squared difference per factor
sqrt(apply((y^2), 2, mean))
# Identity coefficient per factor after rotation
2 * colSums(trx$loadings*trBritain)/( colSums(trx$loadings^2)+colSums(trBritain^2))
# Additivity coefficient per factor after rotation
diag(2 * cov(trx$loadings, trBritain) ) / diag(var(trx$loadings)+var(trBritain))
# Proportionality coefficient per factor after rotation
colSums(trBritain * trx$loadings)/sqrt(colSums(trBritain^2)*colSums(trx$loadings^2))
# Correlation for each factor per factor after rotation
diag(cor(trBritain, trx$loadings))
options(digits = origdigits$digits)


###################################################
### code chunk number 8: GPAguide.Stex:201-215
###################################################
A <- matrix(c(.664, .688, .492, .837, .705, .82, .661, .457, .765, .322, 
  .248, .304, -0.291, -0.314, -0.377, .397, .294, .428, -0.075,.192,.224,
  .037, .155,-.104,.077,-.488,.009), ncol=3)  
 # using targetT
SPA <- matrix(c(rep(NA, 6), .7,.0,.7, rep(0,3), rep(NA, 7), 
  0,0, NA, 0, rep(NA, 4)), ncol=3)
xt <- targetT(A, Target=SPA)
# using pstT
SPApst <- matrix(c(rep(0, 6), .7,.0,.7, rep(0,3), rep(0, 7), 
  0, 0, 0, 0, rep(0, 4)), ncol=3)
SPAW <- matrix(c(rep(0, 6), rep(1, 6), rep(0, 7), 1, 1, 0, 1, 
  rep(0, 4)), ncol=3)
xpst <- pstT(A, Target = SPApst, W = SPAW)
max(abs(loadings(xt)- loadings(xpst)))


