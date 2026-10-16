# M06.4 - Multiple Linear Regression (Pt. 1)

data = read.csv("HousingData_9.25.csv")

# question 5
Y = data$price_usd
X1 = data$living_space_ft2
X2 = data$lot_size_ft2
X3 = data$home_age
X4 = data$distance_city_mi

X = cbind(1, X1, X2, X3, X4)

XtX = t(X) %*% X
XtX_inv = solve(XtX)
XtY = t(X) %*% Y
beta_hat = XtX_inv %*% XtY
beta_hat

# question 6
Y_hat = X %*% beta_hat
residuals = Y - Y_hat

RMSE = sqrt(mean(residuals^2))
RMSE

# question 7
SST = sum((Y - mean(Y))^2)
SSE = sum( residuals^2 )
n = length(Y)
p = 4
R2 = 1 - (SSE / SST)
R2

adj_R2 = 1 - ( (SSE / (n - p - 1)) / (SST / (n - 1)) )
adj_R2

# question 8
F_stat = ((SST - SSE) / p) / (SSE / (n - p - 1))
F_stat

# question 9
pf(2.43, p, n-p-1, lower.tail = FALSE)

# question 10
X_lot = cbind(1, X1, X3, X4)
y_lot = X2
beta_lot = solve(t(X_lot) %*% X_lot) %*% (t(X_lot) %*% y_lot)
y_lot_hat = X_lot %*% beta_lot
SSE_lot = sum((y_lot - y_lot_hat)^2)
SST_lot = sum((y_lot - mean(y_lot))^2)
R2_lot = 1 - SSE_lot/SST_lot
VIF_loftsize =  1 / (1 - R2_lot)
VIF_loftsize

X_lot = cbind(1, X2, X3, X4)
y_lot = X1
beta_lot = solve(t(X_lot) %*% X_lot) %*% (t(X_lot) %*% y_lot)
y_lot_hat = X_lot %*% beta_lot
SSE_lot = sum((y_lot - y_lot_hat)^2)
SST_lot = sum((y_lot - mean(y_lot))^2)
R2_lot = 1 - SSE_lot/SST_lot
VIF_livingspace =  1 / (1 - R2_lot)
VIF_livingspace





