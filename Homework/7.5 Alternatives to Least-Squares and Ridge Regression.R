# M07.5 - Alternatives to Least-Squares and Ridge Regression

source("myladlasso.R")
data = read.csv("HousingData_9.25.csv")

Y = data$price_usd
X1 = data$living_space_ft2
X2 = data$home_age
X3 = data$distance_city_mi
X4 = data$crime_rate_index

n = length(Y)
X = cbind(1, X1, X2, X3, X4)
p = ncol(X) - 1
SST = sum((Y - mean(Y))^2)


# Least Squares
beta_ols = solve(t(X) %*% X) %*% (t(X) %*% Y)
Yhat = X %*% beta_ols
SSE = sum((Y - Yhat)^2)
RMSE_LS = sqrt(SSE / n)
SSR = SST - SSE
F_LS = (SSR / p) / (SSE / (n - p - 1))

# Ridge
I = diag(5)
L = 15
beta_ridge = solve(t(X) %*% X + L * I) %*% (t(X) %*% Y)
Yhat = X %*% beta_ridge
SSE = sum((Y - Yhat)^2)
RMSE_RIDGE = sqrt(SSE / n)
SSM = sum((Yhat - mean(Y))^2)  # Calculate directly!
F_RIDGE = (SSM / p) / (SSE / (n - p - 1))

# LAD - calculate SSM directly!
b_lad = myladlasso(Xk, Y, lam = 0, method = "lad")
Yhat = X %*% b_lad
SSE = sum((Y - Yhat)^2)
RMSE_LAD = sqrt(SSE / n)
SSM = sum((Yhat - mean(Y))^2)  # Calculate directly!
F_LAD = (SSM / p) / (SSE / (n - p - 1))

# Lasso - calculate SSM directly!
b_lasso = myladlasso(Xk, Y, lam = 15, method = "lasso")
Yhat = X %*% b_lasso
SSE = sum((Y - Yhat)^2)
RMSE_LASSO = sqrt(SSE / n)
SSM = sum((Yhat - mean(Y))^2)  # Calculate directly!
F_LASSO = (SSM / p) / (SSE / (n - p - 1))

# LAD-Lasso - calculate SSM directly!
b_ladlasso = myladlasso(Xk, Y, lam = 15, method = "ladlasso")
Yhat = X %*% b_ladlasso
SSE = sum((Y - Yhat)^2)
RMSE_LADLASSO = sqrt(SSE / n)
SSM = sum((Yhat - mean(Y))^2)  # Calculate directly!
F_LADLASSO = (SSM / p) / (SSE / (n - p - 1))

cat(sprintf("Least-Squares: %.2f, %.2f\n", RMSE_LS, F_LS))
cat(sprintf("Ridge:         %.2f, %.2f\n", RMSE_RIDGE, F_RIDGE))
cat(sprintf("LAD:           %.2f, %.2f\n", RMSE_LAD, F_LAD))
cat(sprintf("Lasso:         %.2f, %.2f\n", RMSE_LASSO, F_LASSO))
cat(sprintf("LAD-Lasso:     %.2f, %.2f\n", RMSE_LADLASSO, F_LADLASSO))

