# 4.2 - Heteroscedastic ANOVA 

data <- read.csv("FlowerGrowth_9.25.csv")

####### Question 1 ####### 

#A
x = data$Height_2Months_cm
x_sorted = sort(x)
n = length(x_sorted)
F_emp = (1:n) / (n+1) 
mean_obs = mean(x_sorted)
sd_obs = sd(x_sorted)
F_null = pnorm(x_sorted, mean_obs, sd_obs)
AD_stat = sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
AD_stat

#B
x_split = split(data$Height_2Months_cm, data$Climate_Zone)
x_arid = x_split$Arid
x_sorted = sort(x_arid)
n = length(x_sorted)
F_emp = (1:n) / (n+1) 
mean_obs <- mean(x_sorted)
sd_obs   <- sd(x_sorted)
F_null <- pnorm(x_sorted, mean_obs, sd_obs)
AD_stat <- sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
AD_stat

#C
x_split = split(data$Height_2Months_cm, data$Climate_Zone)
x_temperate = x_split$Temperate
x_sorted = sort(x_temperate)
n = length(x_sorted)
F_emp = (1:n) / (n+1) 
mean_obs <- mean(x_sorted)
sd_obs   <- sd(x_sorted)
F_null <- pnorm(x_sorted, mean_obs, sd_obs)
AD_stat <- sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
AD_stat

#D
x_split = split(data$Height_2Months_cm, data$Climate_Zone)
x_tropical = x_split$Tropical
x_sorted = sort(x_tropical)
n = length(x_sorted)
F_emp = (1:n) / (n+1) 
mean_obs <- mean(x_sorted)
sd_obs   <- sd(x_sorted)
F_null <- pnorm(x_sorted, mean_obs, sd_obs)
AD_stat <- sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
AD_stat


####### Question 2 ####### 

#A
X = Data$Height_2Months_cm
Xs = split(X, Data$Climate_Zone)
s2j = sapply(Xs, var)
nj  = sapply(Xs, length)
vj  = nj - 1
sp2 = sum(vj * s2j) / sum(vj)
n = sum(nj)
v = sum(vj)
g = n-v
Ba_num = v*log(sp2) - sum(vj*log(s2j))
Ba_den = 1 + (1 / (3*(g-1))) * (sum(1/vj) - 1/v)
Ba_stat = Ba_num/Ba_den
Ba_stat

#B
pval = 1 - pchisq(Ba_stat, df)
pval


####### Question 3 ####### 

#A
x_split = split(data$Height_2Months_cm, data$Climate_Zone)
x_arid = x_split$Arid
mean(x_arid)

#B
x_split = split(data$Height_2Months_cm, data$Climate_Zone)
x_temperate = x_split$Temperate
mean(x_temperate)

#C
x_split = split(data$Height_2Months_cm, data$Climate_Zone)
x_tropical = x_split$Tropical
mean(x_tropical)


####### Question 4 ####### 

#A (Homoscedastic)
X = Data$Height_2Months_cm
Xs = split(X, Data$Climate_Zone)
nj = sapply(Xs, length)
xbarj= sapply(Xs, mean)
xbarbar_homo = sum(nj * xbarj) / sum(nj)
xbarbar_homo

#B (Heteroscedastic)
X = Data$Height_2Months_cm
Xs_h = split(X, Data$Climate_Zone)
nj_h = sapply(Xs_h, length)
s2j_h = sapply(Xs_h, var)
xbarj_h= sapply(Xs_h, mean)
wj_h = nj_h/s2j_h
xbarbar_h = sum(wj_h*xbarj_h)/sum(wj_h)
xbarbar_h


####### Question 5 ####### 
v_h_top <- (sum(nj_h/s2j_h))^2
v_h_bot <- sum((nj_h^2) / (s2j_h^2 * (nj_h - 1)))
v_h     <- v_h_top / v_h_bot
v_h #MSE


####### Question 6 ####### 
ssm_h = sum(wj_h*(xbarj_h-xbarbar_h)^2)
g = length(Xs_h)
msm_h = ssm_h/(g-1)

F_stat_h = msm_h/(1+2*(g-2)/v_h) 
F_stat_h





