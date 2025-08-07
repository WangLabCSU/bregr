### R code from vignette source 'GPArotateDF.Stex'

###################################################
### code chunk number 1: GPArotateDF.Stex:19-20 (eval = FALSE)
###################################################
##  options(continue="  ")


###################################################
### code chunk number 2: GPArotateDF.Stex:76-83 (eval = FALSE)
###################################################
## library(GPArotateDF)
## ff.quartimax<- function(L){
##   f = -sum(L^4) / 4
##   list(f = f, Method = "DF-Quartimax")
## }
## data(Harman, package="GPArotation")
## GPForth.df(Harman8, method="quartimax")


###################################################
### code chunk number 3: GPArotateDF.Stex:95-100 (eval = FALSE)
###################################################
## ff.cubimax<- function(L){
##   f = -sum(abs(L^3))
##   list(f = f, Method = "DF-Cubimax")
## }
## GPForth.df(Harman8, method="cubimax")


###################################################
### code chunk number 4: GPArotateDF.Stex:127-155 (eval = FALSE)
###################################################
## ff.fss <- function(L, kij=2){
##   m <- ncol(L)
##   p <- nrow(L)
##   zm <- m + kij
##   Imat <- matrix(0, p, m)
##   for (j in 1:m){
##     Imat[abs(L[,j]) <= sort(abs(L[,j]))[zm],j] <- 1 }
##   for (i in 1:(m-1)){
##     for (j in (i+1):m){
##       nz <- sum( (Imat[,i] + Imat[,j]) ==1)
##       while (nz < zm && sum(Imat[ ,c(i,j)]) < m * 2){
## 	    tbc <- c(abs(L[,i]), abs(L[,j]))
## 	    tbcs <- sort(tbc [c(Imat[,i], Imat[,j])==0])[1]
## 	    Imat[abs(L) == tbcs] <- 1
## 	    nz <- sum( (Imat[,i] + Imat[,j]) ==1)
##       }
##     }
##   }
##   Method <- paste("DF-Forced Simple Structure (kij = ",kij,")", sep="")
##   f <- sum(Imat*L^2)
##   list(f = f, Imat = Imat,
##        Method = Method)
## }
## data(WansbeekMeijer, package = "GPArotation")
## z <- factanal(covmat = NetherlandsTV, factors = 3, rotation = "none")
## fssT.df(loadings(z), kij = 3)
## # which loadings get weight 1 in the first iteration?
## ff.fss(loadings(z), kij = 3)$Imat


###################################################
### code chunk number 5: GPArotateDF.Stex:167-172 (eval = FALSE)
###################################################
## ff.oblimax <- function(L){
##   f <- -(log(sum(L^4))-2*log(sum(L^2)))
##   list(f = f,
##        Method = "DF-Oblimax")
## }


###################################################
### code chunk number 6: GPArotateDF.Stex:175-180 (eval = FALSE)
###################################################
## ff.entropy <- function(L){
##   f <- -sum(L^2 * log(L^2 + (L^2==0)))/2
##   list(f = f,
##        Method = "DF-Entropy")
## }


###################################################
### code chunk number 7: GPArotateDF.Stex:183-190 (eval = FALSE)
###################################################
## ff.simplimax <- function(L,k=nrow(L)){
##   # k: Number of close to zero loadings
##   Imat <- sign(L^2 <= sort(L^2)[k])
##   f <- sum(Imat*L^2)
##   list(f = f,
##        Method = "DF-Simplimax")
## }


###################################################
### code chunk number 8: GPArotateDF.Stex:194-205 (eval = FALSE)
###################################################
## ff.pst <- function(L,W,Target){
##   # Needs weight matrix W with 1's at specified values, 0 otherwise
##   # e.g. W = matrix(c(rep(1,4),rep(0,8),rep(1,4)),8). 
##   # When W has only 1's this is procrustes rotation
##   # Needs a Target matrix Target with hypothesized factor loadings.
##   # e.g. Target = matrix(0,8,2)
##   Btilde <- W * Target
##   f <- sum((W*L-Btilde)^2)
##   list(f = f,
##         Method = "DF-PST")
## }


