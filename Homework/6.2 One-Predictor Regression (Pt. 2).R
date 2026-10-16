# 6.2 - One-Predictor Regression (Pt. 2)

data = read.csv("AudienceAdvertising_9.25.csv")

X = data$Number.of.Ad.Clicks.per.Week
Y = data$Customer.Lifetime.Value..USD.

#correlation coefficient 
n = length(X)
dx = X - mean(X)
dy = Y - mean(Y)
cov_xy = sum(dx * dy) / (n-1)
sd_x = sqrt(sum(dx^2) / (n-1))
sd_y = sqrt(sum(dy^2) / (n-1))
r = cov_xy / (sd_x*sd_y)

# fisher 
z = 0.5 * log((1 + r) / (1 - r))

# fisher CI
SE_z = 1 / sqrt(n - 3)
alpha = 0.085
z_crit = qnorm(1 - alpha/2)  
z_L = z - z_crit * SE_z
z_U = z + z_crit * SE_z
r_L <- (exp(2 * z_L) - 1) / (exp(2 * z_L) + 1)
r_U <- (exp(2 * z_U) - 1) / (exp(2 * z_U) + 1)
r_L
r_U

# RMSE
b1 = r * sd_y / sd_x
b0 = mean(Y) - b1 * mean(X)

yhat = b0 + b1 * X
res = Y - yhat
SSE = sum(res^2)

RMSE = sqrt(SSE / length(Y))
RMSE

# unbiased and consistent estimator for the variance of the random variable Y|X
sigma_hat = sqrt(SSE / (n-2))
sigma_hat

# CI
sigma2_hat <- SSE / (n - 2)   # unbiased VARIANCE
xbar <- mean(X)
SXX <- sum((X - xbar)^2)
seb1 <- sqrt(sigma2_hat / SXX)
seb0 <- sqrt(sigma2_hat * (1/n + xbar^2 / SXX))

alpha <- 0.0947
tcrit <- qt(1 - alpha/2, df = n - 2)

lower_b1 <- b1 - tcrit * seb1
upper_b1 <- b1 + tcrit * seb1
lower_b1
upper_b1

lower_b0 <- b0 - tcrit * seb0
upper_b0 <- b0 + tcrit * seb0
lower_b0
upper_b0

# confidence and prediction bands around some predictor value x* = 73 with a two-sided alpha value of 4.8%.
alpha <- 0.048
tcrit <- qt(1 - alpha/2, df = n - 2)
xstar <- 73
yhat_star <- b0 + b1 * xstar

SE_mean <- sigma_hat * sqrt( (1/n) + ((xstar - xbar)^2)/SXX )
CB_lower <- yhat_star - tcrit * SE_mean
CB_upper <- yhat_star + tcrit * SE_mean
CB_lower
CB_upper

SE_pred <- sigma_hat * sqrt( 1 + (1/n) + ((xstar - xbar)^2)/SXX )
PB_lower <- yhat_star - tcrit * SE_pred
PB_upper <- yhat_star + tcrit * SE_pred
PB_lower
PB_upper

# confidence and prediction bands around the point for which the bands are the most narrow with a two-sided alpha value of 1.5%
alpha <- 0.015
tcrit <- qt(1 - alpha/2, df = n - 2)
xstar <- mean(X)
yhat_star <- b0 + b1 * xstar

SE_mean <- sigma_hat * sqrt( (1/n) + ((xstar - xbar)^2)/SXX )
CB_lower <- yhat_star - tcrit * SE_mean
CB_upper <- yhat_star + tcrit * SE_mean
CB_lower
CB_upper

SE_pred <- sigma_hat * sqrt( 1 + (1/n) + ((xstar - xbar)^2)/SXX )
PB_lower <- yhat_star - tcrit * SE_pred
PB_upper <- yhat_star + tcrit * SE_pred
PB_lower
PB_upper


