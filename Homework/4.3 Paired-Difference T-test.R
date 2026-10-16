# 4.3 - Paired-Difference T-test

data = read.csv("CholesterolStudy_9.25.csv")

d = data$Cholesterol_Before_mg_dL - data$Cholesterol_After_mg_dL

n  = 75
df = n - 1
xbar = mean(d)
sd_d = sd(d)
SE = sd_d / sqrt(n)

alpha <- 0.07
tcrit <- qt(1 - alpha/2, df)

lower <- xbar - tcrit*SE
upper <- xbar + tcrit*SE
tstat <- xbar / SE


