# 6.3 - Basics of Multiple Linear Regression

data = read.csv("AudienceAdvertising_9.25.csv")

res = c(-3.4, 1.7, 2.8, 1.4, -5.2, -1.4)
SSE = sum(res^2)
SSE

# MSE with 2 predictors
n <- length(res)
p <- 2
MSE <- SSE / (n - p - 1)
MSE


SST = 1506.72
SSE = 854.42
SSR= SST - SSE
p = 4
n = 347
MSR = SSR / p
MSE = SSE / (n-p-1)
F = MSR / MSE
r2 <- 1 - SSE/SST
r2


### Sample sizes
nA <- 34
nB <- 21
nC <- 28

### Means
mA <- 6.5
mB <- 7.5
mC <- 8.1

### Standard deviations
sdA <- 0.7
sdB <- 1.6
sdC <- 1.2

### Total sample size
N <- nA + nB + nC

### Overall mean
Ybar <- (nA*mA + nB*mB + nC*mC) / N

### Between-group sum of squares (SSB)
SSB <- nA*(mA - Ybar)^2 +
  nB*(mB - Ybar)^2 +
  nC*(mC - Ybar)^2

### Within-group sum of squares (SSW)
SSW <- (nA - 1)*sdA^2 +
  (nB - 1)*sdB^2 +
  (nC - 1)*sdC^2

### Degrees of freedom
dfB <- 3 - 1          # k - 1
dfW <- N - 3          # N - k

### Mean squares
MSB <- SSB / dfB
MSE <- SSW / dfW

### F-statistic
F_stat <- MSB / MSE
F_stat






