# Exam 5.2

source("functions.R")
data = read.csv("AudienceAdvertising_9.25.csv")

onesamp_bootSD = function(X, S, N, alpha){
  set.seed(S)
  mysd = c()
  
  for(k in 1:N){
    sboot = sample(X, length(X), replace=T)
    mysd = c(mysd, sd(sboot))
  }
  cimean = quantile(mysd, c(alpha/2, 1-alpha/2))
  return(cimean)
  
  }
  
onesamp_bootSD(data$Number.of.Ad.Clicks.per.Week, 121, 15123,0.084)
twosamp_bootLSR(data$Number.of.Ad.Clicks.per.Week, data$Number.of.Online.Purchases.in.Last.Month, 345, 15123,0.084)


fac = twofac.ANOVA(data$Average.Discount.Rate.Used...., data$Preferred.Platform, data$Product.Interest.Category)
mean(fac$pA, fac$pB, fac$pAB)
one = onefac.ANOVA(data$Monthly.Online.Spending..USD., data$Preferred.Platform)
sqrt(one$MSE)

mydata = read.csv('AudienceAdvertising_9.25.csv')
set.seed(1234)
options(digits = 10)
var1 = mydata$Average.Discount.Rate.Used....
var2 = mydata$Monthly.Online.Spending..USD.
n = length(var1)

X1 = mydata$Number.of.Ad.Clicks.per.Week
X2 = X1 + abs(rnorm(n, 0, 5.4))*(var2/var1)^(1/154)
X3 = X2 + abs(rnorm(n, 0, 5.4))*(var2/var1)^(1/187)

X = cbind(X1, X2, X3)
X = as.matrix(X)

rm = rm.ANOVA(X)
nonspher.rmANOVA(X)


welch.ANOVA(data$Average.Time.on.Social.Media.per.Day..min., data$Product.Interest.Category)


x = data$Average.Time.on.Social.Media.per.Day..min.
y = data$Product.Interest.Category

Xs = split(x,y)
nj = sapply(Xs, length)
s2j = sapply(Xs, var)
wj = nj/s2j
xbarbar_h = sum(wj*xbarj)/sum(wj)

g = length(unique(y))
n = length(x)

ssm_h = sum(wj*(xbarj-xbarbar_h)^2)
msm_h = ssm_h/g

v_h_top = (sum(nj/s2j))^2
v_h_bot = sum((nj^2) / (s2j^2 * (nj - 1)))
v_h = v_h_top / v_h_bot

Fstat_h = msm_h/(1+2 * (g-2)/v_h)
p = pf(Fstat_h, g-1, v_h, lower.tail=F)





