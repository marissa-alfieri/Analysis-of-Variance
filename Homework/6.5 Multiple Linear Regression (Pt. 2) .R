# M06.5 - Multiple Linear Regression (Pt. 2)

data = read.csv("HousingData_9.25.csv")

W = ifelse(data$waterfront == "Yes", 1, 0)

Y = data$price_usd
X1 = data$living_space_ft2
X2 = data$lot_size_ft2
X3 = data$home_age
X4 = data$distance_city_mi

X = cbind(1, X1, X2, X3, X4, W)

# question 5
beta_hat = solve(t(X) %*% X) %*% (t(X) %*% Y)
beta_water = beta_hat[6]
sqrt_beta = sqrt(beta_water)
sqrt_beta


# question 6
Y_hat = X %*% beta_hat
res = Y - Y_hat
n = length(Y)
p = 5

sigma_hat = sqrt(sum(res^2) / (n - p - 1))
sigma_hat


# question 7
X_lot = cbind(1, X1, X3, X4, W)
y_lot = X2
beta_lot = solve(t(X_lot) %*% X_lot) %*% (t(X_lot) %*% y_lot)
y_lot_hat = X_lot %*% beta_lot
SSE_lot = sum((y_lot - y_lot_hat)^2)
SST_lot = sum((y_lot - mean(y_lot))^2)
R2_lot = 1 - SSE_lot/SST_lot
VIF_lot =  1 / (1 - R2_lot)
VIF_lot

MCI_lot = sqrt(VIF_lot)
MCI_lot


# question 8
sigma2_hat = (sigma_hat)^2
Var_beta = sigma2_hat * solve(t(X) %*% X)
SE_home_age = sqrt(Var_beta[4,4])  
SE_home_age


# question 9
beta_lot = beta_hat[3]
SE_lot = sqrt(Var_beta[3,3])
t_lot = beta_lot / SE_lot
t_lot












