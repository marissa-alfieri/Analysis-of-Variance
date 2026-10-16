# 4.1 - One-Factor ANOVA Testing 

mydata = read.csv("FlowerGrowth_9.25.csv")

####### Question 2 ####### 
'''sample pooled covariance'''
X = mydata$Height_2Weeks_cm
Xs = split(X, mydata$Climate_Zone)
xbarj = sapply(Xs, mean)
s2j   = sapply(Xs, var)
nj    = sapply(Xs, length)
vj    = nj - 1
sp2 = sum(vj * s2j) / sum(vj)

n = sum(nj)
v = sum(vj)
g = n-v

####### Question 3 ####### 
'''ANOVA'''
s2all = var(X)
xbarbar = mean(X)

#sum of squares 
SST = s2all*(n-1) #total
SSE = sum(s2j*vj) #within groups
SSM = sum(nj*(xbarj - xbarbar)^2) #between groups 

#degrees of freedom
total.dof = n-1
within.dof = n-g
between.dof = g-1

#mean sum of squares
MST = SST/(n-1) #total
MSE = SSE/(n-g) #within
MSM = SSM/(g-1) #between

####### Question 4 ####### 
'''F-test statistic'''
Fstat = MSM/MSE

