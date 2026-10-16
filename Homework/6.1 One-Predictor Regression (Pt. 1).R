# M06.1 - One-Predictor Regression (Pt. 1)


data = read.csv("MedicalData_9.25.csv")

x = data$Weight
y = data$Systolic_BP

n = length(x)

# correlation
r = cor(x, y)
r

# slope of the least-squares regression line
b1 = r * ((sd(y)) / (sd(x)))
b1

# intercept of the least-squares regression line
b0 = mean(y) - b1 * mean(x)
b0

# RMSE (Root Mean Squared Error)
Yhat = b0 + b1 * x
residuals = y - Yhat
RMSE = sqrt(sum(residuals^2) / (n))
RMSE

# Unbiased estimate for Var(Y | X)


# standard error associated to the slope
s = sqrt(sum(residuals^2) / (length(y) - 2))
Sxx = sum((x - mean(x))^2)
SE_b1 = s / sqrt(sum(Sxx))
SE_b1

# standard error associated to the intercept
SE_b0 = s * (sqrt((1/n) + (mean(x)^2 / Sxx)))
SE_b0

# confidence interval for the slope
alpha = 0.0943
tstar = qt(1 - alpha/2, df = n - 2)
slope_CI = c(b1 - tstar*SE_b1, b1 + tstar*SE_b1)
int_CI = c(b0 - tstar*SE_b0, b0 + tstar*SE_b0)
slope_CI
int_CI

# Fisher z-transformation
z_r = 0.5 * log((1 + r) / (1 - r))
SE_z = 1 / sqrt(n - 3)
zstar = qnorm(1 - alpha/2)
z_lower = z_r - zstar * SE_z
z_upper = z_r + zstar * SE_z
r_lower = (exp(2*z_lower) - 1) / (exp(2*z_lower) + 1)
r_upper = (exp(2*z_upper) - 1) / (exp(2*z_upper) + 1)
c(r_lower, r_upper)

























