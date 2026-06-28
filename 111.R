library(MASS) 
library(robustbase) 
set.seed(42) 
x <- rnorm(30, 10, sqrt(2)) 
t_vals <- seq(-10, 30, length=200) 

Mode_fn <- function(v) { d <- density(v); d$x[which.max(d$y)] } 
GM_fn <- function(v) exp(mean(log(v[v>0]))) 
HM_fn <- function(v) 1/mean(1/v[v!=0]) 

IC_mean <- sapply(t_vals, function(t) mean(c(x,t)) - mean(x)) 
IC_median <- sapply(t_vals, function(t) median(c(x,t)) - median(x)) 
IC_mode <- sapply(t_vals, function(t) Mode_fn(c(x,t))- Mode_fn(x)) 
IC_gm <- sapply(t_vals, function(t) GM_fn(c(x,t)) - GM_fn(x)) 
IC_hm <- sapply(t_vals, function(t) HM_fn(c(x,t)) - HM_fn(x)) 

matplot(t_vals, cbind(IC_mean, IC_median, IC_mode, IC_gm), 
        type="l", lty=1, col=1:4, lwd=2, xlim = c(0,20), 
        main="IC: Location Measures", xlab="Outlier t", ylab="IC value") 
legend("topleft", c("Mean","Median","Mode","GM"), 
       col=1:4, lty=1, cex=0.5) 
plot(t_vals,IC_hm,type="l", main="IC Harmonic mean", xlab="Outlier t", ylab="IC value",lwd=2) 



####2
library(MASS) 
library(robustbase) 
set.seed(42) 
x <- rnorm(30, 10, sqrt(2)) 
t_vals <- seq(-10, 30, length=200) 
IC_var <- sapply(t_vals, function(t) var(c(x,t)) - var(x)) 
IC_rng <- sapply(t_vals, function(t) diff(range(c(x,t))) - diff(range(x))) 
IC_md <- sapply(t_vals, function(t) mean(abs(c(x,t)-mean(c(x,t)))) - mean(abs(x-mean(x)))) 
matplot(t_vals, cbind(IC_var, IC_rng, IC_md), 
        type="l", lty=1, col=1:3, lwd = 2, 
        main="IC: Dispersion Measures", xlab="Outlier t", ylab="IC value") 
legend("topleft", c("Variance","Range","Mean Dev"), 
       col=1:3, lty=1, cex=0.8, lwd=2)


####3
library(MASS) 
library(robustbase) 
set.seed(42) 
x <- rnorm(30, 10, sqrt(2)) 
t_vals <- seq(-10, 30, length=200) 
c0_mcd <- covMcd(matrix(x, ncol=1))$center[1] 
c0_mve <- cov.mve(matrix(x, ncol=1))$center[1] 
IC_mean <- sapply(t_vals, function(t) mean(c(x,t)) - mean(x)) 
IC_median <- sapply(t_vals, function(t) median(c(x,t)) - median(x)) 
IC_mcd <- sapply(t_vals, function(t) 
  covMcd(matrix(c(x,t), ncol=1))$center[1] - c0_mcd) 
IC_mve <- sapply(t_vals, function(t) 
  cov.mve(matrix(c(x,t), ncol=1))$center[1] - c0_mve) 
matplot(t_vals, cbind(IC_mean, IC_median, IC_mcd, IC_mve), 
        type="l", lty=1, col=1:4, lwd=2, 
        main="IC: Location (MCD & MVE)", xlab="Outlier t", ylab="IC value") 
legend("topleft", c("Mean","Median","MCD","MVE"), col=1:4, lty=1, cex=0.8, lwd=2) 




####4
library(MASS) 
library(robustbase) 
set.seed(42) 
x <- rnorm(30, 10, sqrt(2)) 
t_vals <- seq(-10, 30, length=200) 
v0_mcd <- covMcd(matrix(x, ncol=1))$cov[1,1] 
v0_mve <- cov.mve(matrix(x, ncol=1))$cov[1,1] 
IC_var <- sapply(t_vals, function(t) var(c(x,t)) - var(x)) 
IC_mcd <- sapply(t_vals, function(t) 
  covMcd(matrix(c(x,t), ncol=1))$cov[1,1] - v0_mcd) 
IC_mve <- sapply(t_vals, function(t) 
  cov.mve(matrix(c(x,t), ncol=1))$cov[1,1] - v0_mve) 
matplot(t_vals, cbind(IC_var, IC_mcd, IC_mve), 
        type="l", lty=1, col=1:3, lwd=2, 
        main="IC: Dispersion (MCD & MVE)", xlab="Outlier t", ylab="IC value") 
legend("topleft", c("Variance","MCD","MVE"), col=1:3, lty=1, cex=0.8, lwd=2) 
  
  
  ###5
  library(MASS) 
  library(robustbase) 
  set.seed(42) 
  mu <- c(10, 5); V <- matrix(c(2,1,1,2), 2) 
  X <- mvrnorm(30, mu, V) # MASS::mvrnorm 
  t_grid <- seq(-10, 25, length=100) # Vary outlier along t, t (diagonal direction) 
  IC_mean <- sapply(t_grid, function(t){ 
    Xnew <- rbind(X, c(t,t)) 
    sum((colMeans(Xnew) - colMeans(X))^2) }) 
  IC_median <- sapply(t_grid, function(t){ 
    Xnew <- rbind(X, c(t,t)) 
    sum((apply(Xnew,2,median) - apply(X,2,median))^2) }) 
  IC_mcd <- sapply(t_grid, function(t){ 
    Xnew <- rbind(X, c(t,t)) 
    sum((covMcd(Xnew)$center - covMcd(X)$center)^2) }) 
  IC_mve <- sapply(t_grid, function(t){ 
    Xnew <- rbind(X, c(t,t)) 
    sum((cov.mve(Xnew)$center - cov.mve(X)$center)^2) }) 
  matplot(t_grid, cbind(IC_mean, IC_median, IC_mcd, IC_mve), 
          type="l", lty=1, col=1:4, lwd=2, 
          main="IC: Bivariate Location", xlab="Outlier t", ylab="Squared shift") 
  legend("topleft", c("Mean-vec","Median-vec","MCD","MVE"), 
         col=1:4, lty=1, cex=0.8, lwd=2) 
  
  
  
  ####6
  library(MASS) 
  library(robustbase) 
  set.seed(42) 
  mu <- c(10, 5) 
  V <- matrix(c(2,1,1,2), 2) 
  X <- mvrnorm(30, mu, V) 
  t_grid <- seq(-10, 25, length=100) 
  fnorm <- function(A) sum(A^2) # Frobenius norm squared 
  
  IC_var <- sapply(t_grid, function(t){ 
    Xnew <- rbind(X, c(t,t)) 
    fnorm(cov(Xnew) - cov(X)) }) 
  IC_mcd <- sapply(t_grid, function(t){ 
    Xnew <- rbind(X, c(t,t)) 
    fnorm(covMcd(Xnew)$cov - covMcd(X)$cov) }) 
  IC_mve <- sapply(t_grid, function(t){ 
    Xnew <- rbind(X, c(t,t)) 
    fnorm(cov.mve(Xnew)$cov - cov.mve(X)$cov) }) 
  
  matplot(t_grid, cbind(IC_var, IC_mcd, IC_mve), 
          type="l", lty=1, col=1:3, lwd=2, 
          main="IC: Bivariate Dispersion", xlab="Outlier t", ylab="Frobenius shift") 
  legend("topleft", c("Cov","MCD","MVE"), 
         col=1:3, lty=1, lwd=2, cex=0.8) 
  
  
  
 ###7
 ###i
  library(MASS) 
  library(robustbase) 
  set.seed(42) 
  X <- rnorm(20, 15, sqrt(2)) 
  u <- rnorm(20, 0, 1) 
  Y <- 3 + 2*X + u 
  ols <- lm(Y ~ X) 
  rob <- rlm(Y ~ X) # MASS::rlm (M-estimate, default Huber) 
  cat("OLS coefs: ", coef(ols)) 
  cat("Robust coefs: ", coef(rob)) 
  plot(X, Y, main="OLS vs Robust Regression") 
  abline(ols, col="blue", lwd=2) 
  abline(rob, col="red", lwd=2, lty=2) 
  legend("topleft", c("OLS","Robust"), col=c("blue","red"), lty=1:2) 
  
  ii. 
  t_vals <- seq(-5, 5, length=100) 
  b_ols <- coef(ols)[2] 
  b_rob <- coef(rob)[2] 
  IC_ols <- sapply(t_vals, function(t){ 
    Xn <- c(X, mean(X)); Yn <- c(Y, 3 + 2*mean(X) + t) 
    coef(lm(Yn ~ Xn))[2] - b_ols }) 
  IC_rob <- sapply(t_vals, function(t){ 
    Xn <- c(X, mean(X)); Yn <- c(Y, 3 + 2*mean(X) + t) 
    coef(rlm(Yn ~ Xn))[2] - b_rob }) 
  matplot(t_vals, cbind(IC_ols, IC_rob), 
          type="l", lty=1, col=c("blue","red"), 
          main="IC of Beta: OLS vs M-estimate", xlab="Error perturbation t", ylab="IC") 
  legend("topleft", c("OLS","M-est"), col=c("blue","red"), lty=1) 
  
  
  iii. 
  set.seed(42) 
  Y2 <- Y 
  idx <- sample(1:20, 5) 
  Y2[idx] <- rnorm(5, 3*max(Y), sqrt(2)) 
  
  ols2 <- lm(Y2 ~ X) 
  rob2 <- rlm(Y2 ~ X) 
  
  cat("OLS (with Y outliers): ", coef(ols2)) 
  cat("Robust (with Y outliers): ", coef(rob2)) 
  
  plot(X, Y2, main="Y Outliers: OLS vs Robust", col=ifelse(1:20 %in% idx,"red","black")) 
  abline(ols2, col="blue", lwd=2) 
  abline(rob2, col="red", lwd=2, lty=2) 
  legend("topleft", c("OLS","Robust","Outlier"), col=c("blue","red","red"), 
         lty=c(1,2,NA), pch=c(NA,NA,1)) 
  
  iv. 
  #clean data 
  set.seed(42) 
  X <- rnorm(20, 15, sqrt(2)) 
  u <- rnorm(20, 0, 1) 
  Y <- 3 + 2*X + u 
  #contaminated data 
  X2 <- X 
  idx2 <- sample(1:20, 5) 
  X2[idx2] <- rnorm(5, 22, sqrt(3)) 
  #Contaminated fit 
  ols3 <- lm(Y ~ X2) 
  rob3 <- lmrob(Y ~ X2) 
  cat("OLS (leverage points): ", coef(ols3)) 
  cat("Robust (leverage points): ", coef(rob3)) 
  
  #plot 
  plot(X2, Y, main="High Leverage X: OLS vs Robust", 
       col=ifelse(1:20 %in% idx2,"red","black")) 
  abline(ols3, col="blue", lwd=2) 
  abline(rob3, col="red", lwd=2, lty=2) 
  legend("topleft", c("OLS","Robust","High Lev"), col=c("blue","red","red"), 
         lty=c(1,2,NA), pch=c(NA,NA,1)) 
  
  
sc(t)=T(X U {t}) - T(X)
mean: X-bar = 1/n summation of xi
    sc(t)=mean(c(x,t))-mean(x)
median: sc(t)=median(c(x,t))-median(x)
mode=arg max(y)f-cap (y)
    sc(t)=mode(c(x,t))-mode(x)
GM=exp(1/n summation of ln(xi))    xi>0
    sc(t)=gm(c(x,t))-gm(x)
Hm=1/(1/m*summation of 1/xi)
    sc(t)
var(x)
range(x)=max(x)-min(x)
MAD=1/n summation of |xi-x_bar|
  MCD:
mu_cap MCD = 1/n summation of xi
H mCD = arg min h<x det(sigma_cap H)
MVE:
  mu_cap mce= center of E mve
E mve = the smallest ellipsoid containing h points
  
  
  
  
  
  
  

  #1(a)
  X =seq(-5, 10, by=0.1)
  pdf.X= dnorm(X, 3, 2)
  pdf.X
  # (b)
  win.graph()
  plot(X, pdf.X)
  # (c)
  nor.x= rnorm(1000, 3, 2)
  hist(nor.x)
  boxplot(nor.x)
  summary(nor.x)
  
  
  
  ### Practical problem : 02
  # 2(a)
  x = rpois(n=1000, lambda=10) # lambda is mean
  win.graph()
  hist(x)
  boxplot(x)
  summary(x)
  mean(x)
  var(x)
  
  
  lcg=function(n){
    rng=vector(length = n)
    m=2**32
    a=1103515245
    c=12345
    #set the seed using the currect system time in micri scound
    z=as.numeric(Sys.time())*1000
    for(i in 1:n){
      z=(a*z+c)%%m
      rng[i]=z/m
    }
    return(rng)
  }
  #b
  lcg(100)
  lcg(500)
  lcg(1000)
  #c
  par(mfrow=c(1,3))
  hist(lcg(100), breaks = 10, main = "n = 100", col = "skyblue")
  hist(lcg(500), breaks = 10, main = "n = 500", col = "lightgreen")
  hist(lcg(1000), breaks = 10, main = "n = 1000", col = "salmon")
  

  
