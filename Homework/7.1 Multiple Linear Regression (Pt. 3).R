# M07.1 - Multiple Linear Regression (Pt. 3)

data = read.csv("EnvStudyData_9.25.csv")


# question 5
Y = data$SpeciesCount
X1 = data$AnnualRainfall
X2 = data$AverageTemperature_C
X3 = data$AirQualityIndex
X4 = data$ForestAreaPercentage
C = model.matrix(~ ClimateZone, data)[ , -1]     
P  = model.matrix(~ PollutionLevel, data)[ , -1]   
X = cbind(1, X1, X2, X3, X4, C, P)
p = ncol(X) - 1   


# question 6l
beta_hat = solve(t(X) %*% X) %*% (t(X) %*% Y)
beta_air = beta_hat[4]
beta_air

Y_hat = X %*% beta_hat
res = Y - Y_hat
n = length(Y)

sigma2_hat = sum(res^2) / (n - p - 1)
Var_beta = sigma2_hat * solve(t(X) %*% X)

SE_forest = sqrt(Var_beta[5,5])
SE_forest


# question 7
SST = sum((Y - mean(Y))^2)
SSE = sum(res^2)

R2 = 1 - (SSE / SST)
adj_R2 = 1 - ((SSE / (n - p - 1)) / (SST / (n - 1)))

F_stat = ((SST - SSE) / p) / (SSE / (n - p - 1))
F_stat


# question 8
y_temp = X[, 3]  
X_temp = X[, -3] 
beta_temp = solve(t(X_temp) %*% X_temp) %*% (t(X_temp) %*% y_temp)
y_hat_temp = X_temp %*% beta_temp
SSE_temp = sum((y_temp - y_hat_temp)^2)
SST_temp = sum((y_temp - mean(y_temp))^2)
R2_temp = 1 - SSE_temp/SST_temp
VIF_temp = 1 / (1 - R2_temp)
VIF_temp

y_rain = X[, 2]  
X_rain = X[, -2]
beta_rain = solve(t(X_rain) %*% X_rain) %*% (t(X_rain) %*% y_rain)
y_hat_rain = X_rain %*% beta_rain
SSE_rain = sum((y_rain - y_hat_rain)^2)
SST_rain = sum((y_rain - mean(y_rain))^2)
R2_rain = 1 - SSE_rain/SST_rain
VIF_rain = 1 / (1 - R2_rain)
MCI_rain = sqrt(VIF_rain)
MCI_rain


# question 9
H = X %*% solve(t(X) %*% X) %*% t(X)
leverages = diag(H)
i = which.max(leverages)
avg_temp_highest_leverage = data$AverageTemperature_C[i]
avg_temp_highest_leverage


# question 10
h = diag(H)
std_res = res / sqrt(sigma2_hat * (1 - h))
i = which.max(abs(std_res))
air_quality_largest_std_resid = data$AirQualityIndex[i]
air_quality_largest_std_resid


# question 11
sortx = sort(std_res)
nj = length(sortx)
F_emp = (1:nj)/(nj+1)
f_null = pnorm(sortx, mean(std_res), sd(std_res))
cvm = sum(abs(F_emp - f_null)^2)
cvm 

set.seed(123)
nmc= 15000
mc_cvm = c()
for(k in 1:nmc){
  smc = rnorm(nj, 0, 1)
  smc = sort(smc)
  fxmc = (1:nj)/(nj+1)
  psixmc = pnorm(smc, 0, 1)
  mc_cvm = c(mc_cvm, sum((fxmc - psixmc)^2))
}

crit = quantile(mc_cvm, 1-.075)
crit
pval = mean(mc_cvm >= cvm)
pval

# question 12
logLik_value = -(n/2) * ( log(2*pi) + 1 + log(SSE/n) )
logLik_value


