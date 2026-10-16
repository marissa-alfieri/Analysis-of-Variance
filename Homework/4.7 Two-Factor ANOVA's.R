# 4.7 - Two-Factor ANOVA's

source("functions.R")

data = read.csv("WeightLossProgram_9.25.csv")

####### Question 1 ####### 
# K-factor ANOVA test Nh = 2^k - 1

####### Question 2 ####### 
WeightLoss = data$Weight_Start_lb - data$Weight_End_lb
grand_mean = mean(WeightLoss)
grand_sd = sd(WeightLoss)

####### Question 3 ####### 
WeightLoss.sl = split(WeightLoss, data$Studio_Locale)
onefac.ANOVA(WeightLoss.sl)

####### Question 4 ####### 
WeightLoss.bs = split(WeightLoss, data$Biological_Sex)
onefac.ANOVA(WeightLoss.bs)

####### Question 5 ####### 
twofac.ANOVA(WeightLoss, data$Studio_Locale, data$Biological_Sex)
p1_value = 1 - pf(two.an$F1, two.an$df1, two.an$dfE)
p2_value = 1 - pf(two.an$F2, two.an$df2, two.an$dfE)
p12_value = 1 - pf(two.an$F12, two.an$df12, two.an$dfE)




X1 = as.factor(data$Studio_Locale)
X2 = as.factor(data$Biological_Sex)
Y = WeightLoss

g1 = length(unique(X1))   
g2 = length(unique(X2))   
n = length(Y)

xbarbar = mean(Y)
s2all = var(Y)
SST = (n - 1) * s2all
df_total = n - 1

Cells = split(Y, interaction(X1, X2, drop = TRUE, lex.order = TRUE))

varsp = function(S){
  if (length(S) > 1) {
    return(var(S))
  } else {
    return(0)
  }
}

n_k = sapply(Cells, length)
s2_k = sapply(Cells, varsp)
SSE = sum((n_k - 1) * s2_k)
dfE = sum(n_k - 1)
MSE = SSE / dfE

mean_loc = tapply(Y, X1, mean)
mean_sex = tapply(Y, X2, mean)
mean_cell = tapply(Y, list(X1, X2), mean)

SSM1 = sum(sapply(mean_loc, function(m) sum(n_k) / g1 * (m - xbarbar)^2))
SSM2 = sum(sapply(mean_sex, function(m) sum(n_k) / g2 * (m - xbarbar)^2))

SSM12 = 0

for (i in levels(X1)){
  for (j in levels(X2)){
    cell = which(X1 == i & X2 == j)
    nij = length(cell)
    cell_mean = mean(Y[cell])
    row_mean = mean(Y[X1 == i])
    col_mean = mean(Y[X2 == j])
    SSM12 = SSM12 + nij * (cell_mean - row_mean - col_mean + mean(Y))^2
  }
}

df1 = g1 - 1
df2 = g2 - 1
df12 = df1 * df2

MSM1 = SSM1 / df1
MSM2 = SSM2 / df2
MSM12 = SSM12 / df12

F1 = MSM1 / MSE
F2 = MSM2 / MSE
F12 = MSM12 / MSE

