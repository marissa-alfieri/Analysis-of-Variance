## Homework 3.5: The Bartlett Test for Homoscedasticity
source("functions.R")
mydata <- read.csv("EnvStudyData_9.25.csv")

X = mydata$ForestAreaPercentage

xgrandmean = mean(X)
xgrandmean

s2all = var(X)
sqrt_s2all = sqrt(s2all)
sqrt_s2all

U = unique(mydata$PollutionLevel)
length = length(U)

X1 = X[which(mydata$PollutionLevel == U[1])]
X2 = X[which(mydata$PollutionLevel == U[2])]
X3 = X[which(mydata$PollutionLevel == U[3])]
X4 = X[which(mydata$PollutionLevel == U[4])]

#pooled variance 
Xs = split(X, mydata$PollutionLevel)
s2j   = sapply(Xs, var)
nj    = sapply(Xs, length)
vj    = nj - 1
sp2 = sum(vj * s2j) / sum(vj)
sqrt_sp2 = sqrt(sp2)
sqrt_sp2

#Bartlett 
n = sum(nj)
v = sum(vj)
g = n-v

Ba_num = v*log(sp2) - sum(vj*log(s2j))
Ba_den = 1 + (1 / (3*(g-1))) * (sum(1/vj) - 1/v)
Ba_den
Ba_stat = Ba_num/Ba_den
Ba_stat

df = length-1
crit_10 = qchisq(1-0.10, df)
crit_10
crit_05 = qchisq(1-0.05, df)
crit_05
crit_02 = qchisq(1-0.02, df)
crit_02

pval = 1 - pchisq(Ba_stat, df)
pval


