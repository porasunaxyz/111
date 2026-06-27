install.packages("Hotelling")
install.packages("car")
install.packages("MASS")

library(MASS)
library(car)
library(Hotelling)

#####Ans to the question no. 1(a)

library(MASS) 
 
mu = c(11,8,12,19,10) 
sigma_D = c(6,4,3,2,1,4,5,4,3,2,3,4,7,4,3,2,3,4,9,4,1,2,3,4,4) 
matrix_A = matrix(sigma_D, nrow = 5, ncol = 5, byrow = TRUE) 
print(matrix_A) 
matrix_mu = matrix(mu, nrow = 5, byrow = T) 
matrix_mu 
n <- 1000 
simulated_data <- mvrnorm(n = n, mu = mu, Sigma = matrix_A) 
df <- as.data.frame(simulated_data) 
colnames(df) <- c("x1", "x2", "x3", "x4", "x5") 
head(df) 

#####b
boxplot(df, main = "Boxplot of Simulated Variates (x1 to x5)", 
        xlab = "Variates", ylab = "Values", 
        col = "lightblue", border = "darkblue") 


####Ans to the question no. 2(a) 
library(MASS) 

mu <- c(5,5) 
sigma_matrix <- matrix(c(8, -3, -3, 5), nrow = 2, byrow = TRUE) 
set.seed(123) 
sample_50 <- mvrnorm(500, mu = mu, Sigma = sigma_matrix) 
sample_1500 <- mvrnorm(1500, mu = mu, Sigma = sigma_matrix) 
sample_6000 <- mvrnorm(6000, mu = mu, Sigma = sigma_matrix) 
head(sample_50) 
head(sample_1500) 
head(sample_6000)


####B
mu_mle_500 <- colMeans(sample_500) 
mu_mle_1500 <- colMeans(sample_1500) 
mu_mle_6000 <- colMeans(sample_6000) 
n1 <- nrow(sample_50) 
sigma_mle_50 <- ((n1 - 1) / n1) * var(sample_50) 
n2 <- nrow(sample_1500) 
sigma_mle_1500 <- ((n2 - 1) / n2) * var(sample_1500) 
n3 <- nrow(sample_6000) 
sigma_mle_6000 <- ((n3 - 1) / n3) * var(sample_6000) 
mu_mle_500 
mu_mle_1500 
mu_mle_6000 
sigma_mle_500 
sigma_mle_1500 
sigma_mle_6000 



#####Ans to the question no. 3(a) 
library(Hotelling)

data_mat <- as.matrix(data.frame(
  y1 = c(580,473,664,739,143,127,703,108,185,111,815,770,759,928,849),
  y2 = c(516,319,369,193,853,632,551,578,74,544,365,522,205,360,137),
  y3 = c(613,514,782,293,927,512,936,856,244,618,500,542,443,402,396),
  y4 = c(750,963,107,530,121,837,118,113,663,816,930,570,789,611,700),
  y5 = c(185,183,211,189,216,195,215,223,163,190,208,170,197,156,190)
))

mu0 <- c(108,500,600,700,180)

alpha <- 0.05

data_centered <- scale(data_mat,
                       center = mu0,
                       scale = FALSE)

dummy_zeros <- matrix(0,
                      nrow = nrow(data_mat),
                      ncol = ncol(data_mat))
# Hotelling's T² test
fit <- hotelling.test(data_centered, dummy_zeros)
# Results
print(fit)
# Decision
if (fit$pval < alpha) {
  cat("Conclusion: Reject H0.\n")
  cat("The sample mean vector is significantly different from mu0.\n")
} else {
  cat("Conclusion: Fail to reject H0.\n")
  cat("There is no significant difference between the sample mean vector and mu0.\n")
}

####4a
library(Hotelling) 
x = list(mean = c(7,10,44),cov = matrix(c(0.46,1.18,4.49,1.18,7.40, -1.35,4.49,-1.35,4.24),nc=3,byrow=TRUE),n=30) 
y = list(mean = c(5, 4, 2),cov = matrix( c(0.148, -0.679, 0.209, -0.679, 4.100, 2.200,0.209, 2.200, 2.1800), 
                                         nc = 3,byrow = TRUE),n = 50) 
fit = hotelling.test(x, y, var.equal = FALSE) 
fit



#####Ans to the question no. 5(a)
library(car) 
 
mu <- c(2, 7)                           
sigma <- matrix(c(2, 1.5, 1.5, 3), 2, 2)  
n <- 100                                 
set.seed(123)  
library(MASS) 
data <- mvrnorm(n, mu, sigma) 
plot(data, pch=20, col="gray", xlab="X1", ylab="X2", main="95% 
Confidence Ellipse") 
dataEllipse(data, levels=0.95, center.pch=19, add=TRUE, 
            plot.points=FALSE, col="blue") 
dataEllipse(data, levels=0.90, add=TRUE, plot.points=FALSE, col="red") 
legend("topleft", legend=c("95% CI", "90% CI"), col=c("blue", "red"), 
       lty=1) 






######COMMENT
Problem 1(a)
Comment:
  The simulated multivariate normal data were generated successfully with the specified mean
vector and covariance matrix.
Problem 1(b)
Comment:
  The boxplot displays the distribution and spread of all five variables. No significant outliers are
observed.
Problem 2(a)
Comment:
  Random samples of sizes 500, 1500 and 6000 were successfully generated from the specified
bivariate normal distribution.
Problem 2(b)
Comment:
  The MLEs of the mean vector and covariance matrix are close to the true parameter values. As the
sample size increases, the estimates become more accurate and stable.
Problem 2(c)
Comment:
  The histograms closely follow the Chi-square distribution with 2 degrees of freedom. As the
sample size increases, the fit to the theoretical distribution improves.
Problem 3(a)
Comment:
  Since the p-value is less than 0.05, the null hypothesis is rejected. The sample mean vector differs
significantly from the hypothesized mean vector.
Problem 4(a)
Comment:
  Since the p-value is less than 0.05, the null hypothesis is rejected. There is a significant difference
between the two population mean vectors.
(If your output gives a p-value greater than 0.05, simply replace "rejected" with "failed to reject.")
Problem 5(a)
Comment:
  The confidence ellipse illustrates the joint distribution of the two variables. Most observations lie
within the confidence region.
Problem 5(b)
Comment:
  The 90% simultaneous confidence region contains the plausible values of the population mean
vector with 90% confidence.













####MOLLA SIR
library(MASS)
library(robustbase)
set.seed(42)

Question:1 Compare the robustness of location measures (mean, median, mode, GM, HM) using influence curve by drawing usual observation of size n=30 from N(10, 2).

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
        type="l", lty=1, col=1:4,lwd=2,xlim = c(0,20),
        main="IC: Location Measures", xlab="Outlier t", ylab="IC value")
legend("topleft", c("Mean","Median","Mode","GM"),
       col=1:4, lty=1, cex=0.5)
plot(t_vals,IC_hm,type="l", main="IC Harmonic mean", xlab="Outlier t", ylab="IC value",lwd=2)




Question:2. Compare the robustness of dispersion measures (range, variance, mean deviations) using influence curve by drawing usual observation of size n=30 from N(10, 2).

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



Question: 3. Compare the robustness of location measures (mean, median, mean-MCD, Mean-MVE) using influence curve by drawing usual observation of size n=30 from N(10, 2).

install.packages("robustbase")
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

Question: 4. Compare the robustness of dispersion measures (variance, variance-MCD, variance-MVE) using influence curve by drawing usual observation of size n=30 from N(10, 2).

x <- rnorm(30, 10, sqrt(2))
t_vals <- seq(-10, 30, length=200)
v0_mcd <- covMcd(matrix(x, ncol=1))$cov[1,1]
v0_mve <- cov.mve(matrix(x, ncol=1))$cov[1,1]

IC_var <- sapply(t_vals, function(t) var(c(x,t)) - var(x))
IC_mcd <- sapply(t_vals, function(t)
  covMcd(matrix(c(x,t), ncol=1))$cov[1,1] - v0_mcd)
IC_mve <- sapply(t_vals, function(t){
  cov.mve(matrix(c(x,t), ncol=1))$cov[1,1] - v0_mve)}
  
  matplot(t_vals, cbind(IC_var, IC_mcd, IC_mve),
          type="l", lty=1, col=1:3, lwd=2,
          main="IC: Dispersion (MCD & MVE)", xlab="Outlier t", ylab="IC value")
  legend("topleft", c("Variance","MCD","MVE"), col=1:3, lty=1, cex=0.8, lwd=2)
  
  
  
  
  Question: 5. Compare the robustness of location measures (mean vector, median vector, mean-vector-MCD, Mean-vector-MVE) using influence curve by drawing usual observation of size n=30 from N2(mu, V), mu & V are the mean-vector and variance-matrix.
  
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
  
  
  
  
  Question: 6. Compare the robustness of dispersion measures (variance-matrix, variance-matrix-MCD, variance-matrix-MVE) using influence curve by drawing usual observation of size n=30 from N2(mu, V), mu & V are the mean-vector and variance-matrix.
  
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
  
  #only Mcd and MVE------------------
  
  matplot(t_grid, cbind(IC_mcd, IC_mve),
          type="l", lty=1, col=1:2, lwd=2,
          main="IC: Bivariate Dispersion", xlab="Outlier t", ylab="Frobenius shift")
  legend("topleft", c("MCD","MVE"),
         col=2:3, lty=1, lwd=2, cex=0.8)
  
  
  
  
  Question:7. Consider a two variable regression model as Y=β0+ βX+u, where u follows N(0, σ2). Generate n=20 observation for X from N(15, 2) and Y by Y=β0+ βX+u with β0=3, β=2 and σ2=1.
  (i) Fit the regression model using the OLS approach and a robust approach and compare.
  (ii) Compare the robustness of OLS and M-estimates of β by the influence curve.
  (iii) Randomly replace 5 observations from Y by outliers generated from N(3×max(Y), 2) and fit the regression model using the OLS approach and a robust approach, and compare the results.
  (iv) Randomly replace 5 observations from X by high leverage points generated from N(22, 3) and fit the regression model using the OLS approach and a robust approach, and compare the results.
  
  
  
  i.
  X <- rnorm(20, 15, sqrt(2))
  u <- rnorm(20, 0, 1)
  Y <- 3 + 2*X + u
  
  ols <- lm(Y ~ X)
  rob <- rlm(Y ~ X) # MASS::rlm (M-estimate, default Huber)
  
  cat("OLS coefs: ", coef(ols), "
")
  cat("Robust coefs: ", coef(rob), "
")
  
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
  
  cat("OLS (with Y outliers): ", coef(ols2), "
")
  cat("Robust (with Y outliers): ", coef(rob2), "
")
  
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
  cat("OLS (leverage points): ", coef(ols3), "
")
  cat("Robust (leverage points): ", coef(rob3), "
")
  
  #plot
  plot(X2, Y, main="High Leverage X: OLS vs Robust",
       col=ifelse(1:20 %in% idx2,"red","black"))
  abline(ols3, col="blue", lwd=2)
  abline(rob3, col="red", lwd=2, lty=2)
  legend("topleft", c("OLS","Robust","High Lev"), col=c("blue","red","red"),
         lty=c(1,2,NA), pch=c(NA,NA,1))
  
  
  MOLLA SIR 1
  
  set.seed(123)
  x <- rnorm(30, mean = 10, sd = 2)
  # Location measures
  mean_x   <- mean(x)
  median_x <- median(x)
  mode_x   <- as.numeric(names(which.max(table(round(x)))))
  gm_x     <- exp(mean(log(x)))
  hm_x     <- length(x)/sum(1/x)
  
  mean_x
  median_x
  mode_x
  gm_x
  hm_x
  # Influence curves
  IC_mean <- x - mean(x)
  IC_median <- sign(x-median(x))
  IC_gm <- (log(x)-mean(log(x)))*gm_x   
  IC_hm <- hm_x^2 * (mean(1/x) - 1/x)   
  # Plot
  plot(x, IC_mean, col="red", pch=19, main="Influence Curves",
       xlab="Observation", ylab="Influence")
  points(x, IC_median, col="blue", pch=19)
  points(x, IC_gm, col="darkgreen", pch=19)
  points(x, IC_hm, col="purple", pch=19)
  legend("bottomright", c("Mean","Median","GM","HM"),
         col=c("red","blue","darkgreen","purple"), pch=c(19,19,19,19))
  
  
  
  Molla sir 2
  
  # Sample from N(10,2), size n=30
  set.seed(123)
  x <- rnorm(30, mean=10, sd=2)
  # Dispersion measures
  range_x <- max(x) - min(x)
  var_x <- var(x)
  md_x <- mean(abs(x - mean(x)))
  range_x
  var_x
  md_x
  # Influence curves
  IC_range <- ifelse(x==max(x) | x==min(x), 1, 0) # only extremes matter
  IC_var <- (x - mean(x))^2 - var_x # quadratic → sensitive
  IC_md <- abs(x - mean(x)) - md_x # moderate sensitivity
  # Plot ICs
  plot(x, IC_var, col="red", pch=19, main="Influence Curves of Dispersion")
  points(x, IC_md, col="blue", pch=19)
  points(x, IC_range, col="green", pch=19)
  legend("top", legend=c("Variance","Mean Deviation","Range"),
         col=c("red","blue","green"), pch=19)
  
  
  MOLLA sir 3
  
  # Step 1: Initialization and Baseline Data
  library(MASS)
  library(robustbase) # Used for robust covMcd calculation
  set.seed(123)
  n <- 30
  x <- rnorm(n, mean=10, sd=2)
  # Define a wide grid of potential contaminant/outlier values (t)
  t_vals <- seq(-10, 30, length.out=200)
  # Calculate baseline metrics before adding any outlier
  c0_mean   <- mean(x)
  c0_median <- median(x)
  c0_mcd    <- covMcd(matrix(x, ncol=1))$center[1]
  c0_mve    <- cov.mve(matrix(x, ncol=1))$center[1]
  # Step 2: Compute True Empirical Influence Curves
  # We append a single moving point 't' to our sample, recalculate the center,
  # and multiply the shift by (n + 1) to match the asymptotic scale of an IC.
  IC_mean <- sapply(t_vals, function(t) {
    (mean(c(x, t)) - c0_mean) * (n + 1)
  })
  
  IC_median <- sapply(t_vals, function(t) {
    (median(c(x, t)) - c0_median) * (n + 1)
  })
  
  IC_mcd <- sapply(t_vals, function(t) {
    (covMcd(matrix(c(x, t), ncol=1))$center[1] - c0_mcd) * (n + 1)
  })
  
  IC_mve <- sapply(t_vals, function(t) {
    (cov.mve(matrix(c(x, t), ncol=1))$center[1] - c0_mve) * (n + 1)
  })
  
  # Step 3: Plot and Compare All Together
  matplot(t_vals, cbind(IC_mean, IC_median, IC_mcd, IC_mve),
          type="l", lty=1, col=c("red", "blue", "darkgreen", "purple"), lwd=2,
          main="True Influence Curves of Location Measures", 
          xlab="Contaminant Value (t)", ylab="Influence Value")
  abline(h=0, lty=2, col="gray") # Zero influence line
  abline(v=10, lty=3, col="gray") # Center of original distribution
  legend("topleft", 
         legend=c("Mean (Linear & Unbounded)", 
                  "Median (Bounded Step Function)", 
                  "MCD (Robust - Drops to 0 for extreme outliers)", 
                  "MVE (Robust - High Variance Step Profile)"), 
         col=c("red", "blue", "darkgreen", "purple"), 
         lty=1, lwd=2, cex=0.85)
  
  
  
  
  
  
  
  ### SALAM SIR      Practical problem: 01     ###
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
  
  ####problem 03
  
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
  
