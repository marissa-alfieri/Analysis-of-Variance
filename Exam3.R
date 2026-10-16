# Exam 3
# Marissa Alfieri

Data <- read.csv("FlowerGrowth_9.25.csv")

####### Question 6 ####### 
#A 
x = Data$Height_2Months_cm
x_sorted = sort(x) 
n = length(x_sorted)
F_emp = (1:n) / (n+1) 
mean_obs= mean(x)
sd_obs= sd(x)
F_null = pnorm(x_sorted, mean_obs, sd_obs)
CvM_stat = sum((F_emp - F_null)^2)
CvM_stat

#B
x = Data$Height_2Months_cm
x_sorted = sort(x) 
n = length(x_sorted)
F_emp = (1:n) / (n+1) 
F_null = pnorm(x_sorted, mean=27, sd=5) 
AD_stat = sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
AD_stat


####### Question 7 ####### 

#A 
X = Data$Height_4Months_cm
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
df = length(Xs)-1
crit_b = qchisq(1-0.011, df)
crit_b

#C
pval = 1 - pchisq(3.8, df)
pval

####### Question 8 ####### 

set.seed(12345)
n = 17
nmc = 10000
mc_astats = c()
Astat = function(x){
  (((abs(median(x))^mean(x))+1) / (var(x)+1))
}
for(k in 1:nmc){
  sim_sample = rnorm(n, 0, 1)
  mc_val = Astat(sim_sample)
  mc_astats = c(mc_astats, mc_val)
}
E_astat = mean(mc_astats)

#A
Acrit = quantile(mc_astats, (1 - 0.04))
Acrit

#B
pval = mean(mc_astats >= 1.2)
pval


