# M07.3 - Sum-of-Squares Analysis

data = read.csv("CompanyPayroll_9.25.csv")

Y = data$Efficiency_Factor_Obs
X1 = data$Age
X2 = data$Prior_Experience
X3 = data$Hourly_Wage
X = cbind(1, X1, X2, X3)

p = ncol(X) - 1   
n = length(Y)

beta_hat = solve(t(X) %*% X) %*% (t(X) %*% Y)
Y_hat = X %*% beta_hat
res = Y - Y_hat
SSE = sum(res^2)

logLik_value = -(n/2) * (log(2*pi) + 1 + log(SSE/n))
logLik_value

# question 1
AIC = (2 * p) - (2 * logLik_value)
AIC
BIC = (p * log(n)) - (2 * logLik_value)
BIC

# question 3
X1_only = cbind(X[,1], X[,2])  # intercept + Age
beta1 = solve(t(X1_only) %*% X1_only) %*% t(X1_only) %*% Y
Y_hat1 = X1_only %*% beta1
SSM1 = sum((Y_hat1 - mean(Y))^2)
X12 = cbind(X[,1], X[,2], X[,3])  # intercept + Age + Prior_Experience
beta12 = solve(t(X12) %*% X12) %*% t(X12) %*% Y
Y_hat12 = X12 %*% beta12
SSM12 = sum((Y_hat12 - mean(Y))^2)
X123 = cbind(X[,1], X[,2], X[,3], X[,4])  # Full model
beta123 = solve(t(X123) %*% X123) %*% t(X123) %*% Y
Y_hat123 = X123 %*% beta123
SSM123 = sum((Y_hat123 - mean(Y))^2)
round(SSM1, 4)         # Row 1: SSM with just Age
round(SSM12 - SSM1, 4) # Row 2: Δ SSM when adding Prior_Experience  
round(SSM123 - SSM12, 4) # Row 3: Δ SSM when adding Hourly_Wage


# question 4
X3_only = cbind(X[,1], X[,4])  # intercept + Hourly_Wage
beta3 = solve(t(X3_only) %*% X3_only) %*% t(X3_only) %*% Y
Y_hat3 = X3_only %*% beta3
SSM3 = sum((Y_hat3 - mean(Y))^2)
X32 = cbind(X[,1], X[,4], X[,3])  # intercept + Hourly_Wage + Prior_Experience
beta32 = solve(t(X32) %*% X32) %*% t(X32) %*% Y
Y_hat32 = X32 %*% beta32
SSM32 = sum((Y_hat32 - mean(Y))^2)
X321 = cbind(X[,1], X[,4], X[,3], X[,2])  # intercept + Hourly_Wage + Prior_Experience + Age
beta321 = solve(t(X321) %*% X321) %*% t(X321) %*% Y
Y_hat321 = X321 %*% beta321
SSM321 = sum((Y_hat321 - mean(Y))^2)
round(SSM3, 4)          # Row 1: SSM with just Hourly_Wage
round(SSM32 - SSM3, 4)  # Row 2: Δ SSM when adding Prior_Experience
round(SSM321 - SSM32, 4) # Row 3: Δ SSM when adding Age

# question 5
# Full model SSM (all predictors: 1, 2, 3)
X_full = cbind(X[,1], X[,2], X[,3], X[,4])  # intercept + Age + Prior_Experience + Hourly_Wage
beta_full = solve(t(X_full) %*% X_full) %*% t(X_full) %*% Y
Y_hat_full = X_full %*% beta_full
SSM_full = sum((Y_hat_full - mean(Y))^2)

# Model without predictor 1 (Age) - keep predictors 2,3
X_no1 = cbind(X[,1], X[,3], X[,4])  # intercept + Prior_Experience + Hourly_Wage
beta_no1 = solve(t(X_no1) %*% X_no1) %*% t(X_no1) %*% Y
Y_hat_no1 = X_no1 %*% beta_no1
SSM_no1 = sum((Y_hat_no1 - mean(Y))^2)

# Model without predictor 2 (Prior_Experience) - keep predictors 1,3
X_no2 = cbind(X[,1], X[,2], X[,4])  # intercept + Age + Hourly_Wage
beta_no2 = solve(t(X_no2) %*% X_no2) %*% t(X_no2) %*% Y
Y_hat_no2 = X_no2 %*% beta_no2
SSM_no2 = sum((Y_hat_no2 - mean(Y))^2)

# Model without predictor 3 (Hourly_Wage) - keep predictors 1,2
X_no3 = cbind(X[,1], X[,2], X[,3])  # intercept + Age + Prior_Experience
beta_no3 = solve(t(X_no3) %*% X_no3) %*% t(X_no3) %*% Y
Y_hat_no3 = X_no3 %*% beta_no3
SSM_no3 = sum((Y_hat_no3 - mean(Y))^2)

# Calculate relative changes (as decimals, not percentages)
rel_change_1 = (SSM_full - SSM_no1) / SSM_full  # Removing predictor 1
rel_change_2 = (SSM_full - SSM_no2) / SSM_full  # Removing predictor 2  
rel_change_3 = (SSM_full - SSM_no3) / SSM_full  # Removing predictor 3

# Results to 4 decimal places
round(rel_change_1, 4)
round(rel_change_2, 4)
round(rel_change_3, 4)




