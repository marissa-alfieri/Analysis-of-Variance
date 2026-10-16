# M07.2 - Multiple Linear Regression (Pt. 4)

data = read.csv("HousingData_9.25.csv")

Y = data$price_usd
X1 = data$living_space_ft2
X2 = data$home_age
X3 = data$distance_city_mi
X4 = data$crime_rate_index
X = cbind(1, X1, X2, X3, X4)

p = ncol(X) - 1   
n = length(Y)

beta_hat = solve(t(X) %*% X) %*% (t(X) %*% Y)
Y_hat = X %*% beta_hat
res = Y - Y_hat
SSE = sum(res^2)


# question 1
logLik_value = -(n/2) * (log(2*pi) + 1 + log(SSE/n))
logLik_value


# question 2 
AIC = (2 * p) - (2 * logLik_value)
AIC


# question 3
BIC = (p * log(n)) - (2 * logLik_value)
BIC


# question 4 
H = X %*% solve(t(X) %*% X) %*% t(X)
h = diag(H)
sigma2_hat = sum(res^2) / (n - p - 1)
std_res = res / sqrt(sigma2_hat * (1 - h))
sortx = sort(std_res)
nj = length(sortx)
F_emp = (1:nj)/(nj+1)
F_null = pnorm(sortx, mean(std_res), sd(std_res))
AD_stat = sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
AD_stat


# question 5 
Y_space = X[, 2]  
X_space = X[, -2]
beta_space = solve(t(X_space) %*% X_space) %*% (t(X_space) %*% Y_space)
y_hat_space = X_space %*% beta_space
SSE_space = sum((Y_space - y_hat_space)^2)
SST_space = sum((Y_space - mean(Y_space))^2)
R2_space = 1 - SSE_space/SST_space
VIF_space = 1 / (1 - R2_space)
MCI_space = sqrt(VIF_space)
MCI_space

Y_age = X[, 3]  
X_age = X[, -3]
beta_age = solve(t(X_age) %*% X_age) %*% (t(X_age) %*% Y_age)
y_hat_age = X_age %*% beta_age
SSE_age = sum((Y_age - y_hat_age)^2)
SST_age = sum((Y_age - mean(Y_age))^2)
R2_age = 1 - SSE_age/SST_age
VIF_age = 1 / (1 - R2_age)
MCI_age = sqrt(VIF_age)
MCI_age

Y_mi = X[, 4]  
X_mi = X[, -4]
beta_mi = solve(t(X_mi) %*% X_mi) %*% (t(X_mi) %*% Y_mi)
y_hat_mi = X_mi %*% beta_mi
SSE_mi = sum((Y_mi - y_hat_mi)^2)
SST_mi = sum((Y_mi - mean(Y_mi))^2)
R2_mi = 1 - SSE_mi/SST_mi
VIF_mi = 1 / (1 - R2_mi)
MCI_mi = sqrt(VIF_mi)
MCI_mi

Y_crime = X[, 5]  
X_crime = X[, -5]
beta_crime = solve(t(X_crime) %*% X_crime) %*% (t(X_crime) %*% Y_crime)
y_hat_crime = X_crime %*% beta_crime
SSE_crime = sum((Y_crime - y_hat_crime)^2)
SST_crime = sum((Y_crime - mean(Y_crime))^2)
R2_crime = 1 - SSE_crime/SST_crime
VIF_crime = 1 / (1 - R2_crime)
MCI_crime = sqrt(VIF_crime)
MCI_crime

all_MCI = c(MCI_space, MCI_age, MCI_mi, MCI_crime)
mean(all_MCI)

# question 5
# For Living Space (first predictor, X[,2])
X_others_space = cbind(X[,1], X[,3], X[,4], X[,5])  # Keep intercept, age, distance, crime
beta_Y_space = solve(t(X_others_space) %*% X_others_space) %*% t(X_others_space) %*% Y
beta_X1_space = solve(t(X_others_space) %*% X_others_space) %*% t(X_others_space) %*% X[,2]

e_Y_space = Y - X_others_space %*% beta_Y_space
e_X1_space = X[,2] - X_others_space %*% beta_X1_space

# Standardized slope for Living Space
slope_space_std = cor(e_X1_space, e_Y_space)
slope_space_std


# For Home Age
X_others_age = cbind(X[,1], X[,2], X[,4], X[,5])
beta_Y_age = solve(t(X_others_age) %*% X_others_age) %*% t(X_others_age) %*% Y
beta_X2_age = solve(t(X_others_age) %*% X_others_age) %*% t(X_others_age) %*% X[,3]
e_Y_age = Y - X_others_age %*% beta_Y_age
e_X2_age = X[,3] - X_others_age %*% beta_X2_age
slope_age_std = cor(e_X2_age, e_Y_age)
slope_age_std

# For Distance from City
X_others_dist = cbind(X[,1], X[,2], X[,3], X[,5])
beta_Y_dist = solve(t(X_others_dist) %*% X_others_dist) %*% t(X_others_dist) %*% Y
beta_X3_dist = solve(t(X_others_dist) %*% X_others_dist) %*% t(X_others_dist) %*% X[,4]
e_Y_dist = Y - X_others_dist %*% beta_Y_dist
e_X3_dist = X[,4] - X_others_dist %*% beta_X3_dist
slope_dist_std = cor(e_X3_dist, e_Y_dist)
slope_dist_std

# For Crime Rate Index
X_others_crime = cbind(X[,1], X[,2], X[,3], X[,4])
beta_Y_crime = solve(t(X_others_crime) %*% X_others_crime) %*% t(X_others_crime) %*% Y
beta_X4_crime = solve(t(X_others_crime) %*% X_others_crime) %*% t(X_others_crime) %*% X[,5]
e_Y_crime = Y - X_others_crime %*% beta_Y_crime
e_X4_crime = X[,5] - X_others_crime %*% beta_X4_crime
slope_crime_std = cor(e_X4_crime, e_Y_crime)
slope_crime_std

