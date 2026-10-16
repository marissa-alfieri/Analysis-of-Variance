##Exam 3

Data <- read.csv("")

#################Star 3.1 = Monte Carlo Simulations (with PC)#################
'''find given test stat'''
xbar= mean(Data$Variable)
s= sd(Data$Variable)
mu0= 35
observed_astat= abs(xbar-mu0)/ (s+1)
observed_astat


'''null distribution is a normally distributed population with a mean of 32 and a standard deviation of 2.9
Conduct a MC with 10,000 simulated samples under a reproducibility seed of 12121'''
set.seed(12121)
mc_astats <- c()
nmc = 10000
n = length(Data$Variable)
null_mean = 32
null_sd = 2.9
mystat <- function(x, mu0) {
  abs(mean(x) - mu0) / (sd(x) + 1)
}
for(k in 1:nmc){
  sim_sample <- rnorm(n, mean = null_mean, sd = null_sd)
  mc_val <- mystat(sim_sample, mu0)
  mc_astats <- c(mc_astats, mc_val)
}
E_astat <- mean(mc_astats)
E_astat


'''Use Ha: mu > 35 with aplha=0.085
what would be MC Acrit'''
alpha <- 0.085
Acrit <- quantile(mc_astats, probs = 1 - alpha)
Acrit


'''Use Ha: mu > 35, find MC pval'''
xbar <- mean(Data$Variable)
s <- sd(Data$Variable)
observed_astat <- abs(xbar - mu0) / (s + 1)
pval <- mean(mc_astats >= observed_astat)
pval


#################Star 3.2 = Distribution Testing#################
'''A cumultative density function always ranges from 0 to 1'''

'''if n=17: KS[0,1], CvM[0,17], AD[8.5,17]'''

'''which is easier to "fail to reject" in terms of a null hypothesis structure: composite'''

'''simple vs. composite: 
X∼N(0,1) → mean and variance fixed → simple
X∼N(μ,1),μ ∈ R → μ can be any real → many possible normals → composite
X∼Exp(λ),λ > 0 → any positive rate → family of exponentials → composite
X∼Poisson(5) → rate fixed at 5 → simple
Lilliefors = composite'''

'''Suppose that you wish to assess the null hypothesis: H0: X∼N(μ=4, σ=2)'''
x <- Data$Variable
x_sorted <- sort(x) # Step 1: Sort data
n <- length(x_sorted)
F_emp <- (1:n) / (n+1) # Step 2: CDF
F_null <- pnorm(x_sorted, mean=4, sd=2) 
# --- Kolmogorov–Smirnov Statistic ---
KS_stat <- max(abs(F_emp - F_null))
KS_stat
## --- Cramér–von Mises Statistic ---
CvM_stat <- sum((F_emp - F_null)^2)
CvM_stat
### --- Anderson–Darling Statistic ---
AD_stat <- sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
AD_stat


'''Suppose that you wish to assess the null hypothesis: H0: X∼N(μ,σ)'''
x <- Data$Variable
x_sorted <- sort(x) # Step 1: Sort data
n <- length(x_sorted)
F_emp <- (1:n) / (n+1) # Step 2: CDF
mean_obs= mean(x)
sd_obs= sd(x)
F_null <- pnorm(x_sorted, mean_obs, sd_obs)
# --- Kolmogorov–Smirnov Statistic ---
KS_stat <- max(abs(F_emp - F_null))
KS_stat
## --- Cramér–von Mises Statistic ---
CvM_stat <- sum((F_emp - F_null)^2)
CvM_stat
### --- Anderson–Darling Statistic ---
AD_stat <- sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
AD_stat


'''MC associated to an AD test stat to the null in the previous question. 
Use a seed of "123123" and use 10,000 simulations to generate your results, 
for which should have a histogram (with 100 breaks) as shown below.'''
set.seed(123123)
nmc <- 10000
mc_ad <- c()
Data <- read.csv("")
x <- Data$Variable 
n <- length(x)
for (k in 1:nmc) {
  Smc <- rnorm(n, 0, 1) # simulate sample from standard normal
  Smc <- sort(Smc)
  Fxmc <- (1:n) / (n+1) # empirical CDF for this simulated sample
  Psixmc <- pnorm(Smc, 0, 1) # null CDF values
  mc_ad <- c(mc_ad, sum((Fxmc - Psixmc)^2 / (Fxmc * (1 - Fxmc)))) # AD statistic for this simulated dataset
}
hist(mc_ad, breaks=100, col="magenta",
     main="Empirical MC AD Distribution", xlab="AD Statistic") # Histogram


'''empirical p-value associated to:'''
#AD=2 
emp_pval = mean(mc_ad >= 2)
emp_pval
##AD=1.4
emp_pval = mean(mc_ad >= 1.4)
emp_pval
###AD=1.1
emp_pval = mean(mc_ad >= 1.1)
emp_pval


'''empirical critical values associated to:'''
#α=0.07
emp_ad_crit = quantile(mc_ad, 1-0.01)
emp_ad_crit
##α=0.05
emp_ad_crit = quantile(mc_ad, 1-0.01)
emp_ad_crit
###α=0.01
emp_ad_crit = quantile(mc_ad, 1-0.01)
emp_ad_crit

#################Star 3.3 = Pairwise Comparison Tests#################
'''five independent groups, if conduct all pairwise T-tests instead of single global test, 
increase the probability of making at least one false positive conclusion (Type 1)'''

'''ANOVA avoids inflating the Type I error rate by testing all group means simultaneously'''

'''one variance test (H0:σ^2=σnull^2) '''
#test stat approach
S <- c(5, 17, 7, 14, 19, 18, 21, 5, 6)
sx <- sd(S)
sx2 <- var(S)
sig2_0 <- 49
c(sig2_0, sx2)
sqrt(c(sig2_0, sx2))
n <- length(S)
chisq2_stat <- (n-1) * var(S) / sig2_0
alpha <- 0.05
c2crit_L <- qchisq(alpha/2, n-1, lower.tail=TRUE)
c2crit_R <- qchisq(alpha/2, n-1, lower.tail=FALSE)
c(c2crit_L, c2crit_R)
chisq2_stat #if this between [c2crit_L, c2crit_R] fail to reject
##pval approach
chisq2_stat_L <- (n-1) * var(S) / sig2_0 # when sample variance is smaller
chisq2_stat_R <- (n-1) * sig2_0 / var(S) # when sample variance is larger
c(chisq2_stat_L, chisq2_stat_R)
pval <- pchisq(chisq2_stat_L, n-1, lower.tail=TRUE) +
  pchisq(chisq2_stat_R, n-1, lower.tail=FALSE)
pval
###Confidence Interval Approach
ci_var <- (n-1) * var(S) * c(1/c2crit_R, 1/c2crit_L)
ci_sd <- sqrt(ci_var)
ci_var
ci_sd #Both variance CI & sd CI include the null values (49 for variance, 7 for SD). Therefore, at α = 0.05, you fail to reject H₀'''


'''two variance test (H0:σ1^2=σ2^2) '''
set.seed(123)
S1 <- runif(32, 4, 19) # sample 1
S2 <- rexp(23, 1/7.7)  # sample 2
var1 <- var(S1)
var2 <- var(S2)
c(var1, var2)
sqrt(c(var1, var2))
fstat <- var2 / var1
fstat
alpha <- 0.05
fcrit <- qf(alpha, length(S2)-1, length(S1)-1, lower.tail=FALSE)
pval <- pf(fstat, length(S2)-1, length(S1)-1, lower.tail=FALSE)
fcrit #Decision rule: If F_stat > F_crit → Reject H0
pval 


'''Pairwise (H0:σ1^2=σ2^2=...σg^2)'''
#Example with g=3 groups:
set.seed(123)
S1 <- rnorm(14, 121, 3)
S2 <- rnorm(15, 124, 3)
S3 <- rnorm(17, 127, 3)
n1 <- length(S1)
n2 <- length(S2)
n3 <- length(S3)
sdx <- c(sd(S1), sd(S2), sd(S3))
sdx
fstat_12 <- var(S2) / var(S1)
fstat_13 <- var(S3) / var(S1)
fstat_23 <- var(S2) / var(S3)
c(fstat_12, fstat_13, fstat_23)
pval_12 <- pf(fstat_12, n2-1, n1-1, lower.tail=FALSE)
pval_13 <- pf(fstat_13, n3-1, n1-1, lower.tail=FALSE)
pval_23 <- pf(fstat_23, n2-1, n3-1, lower.tail=FALSE)
c(pval_12, pval_13, pval_23) #Since all p-values > 0.05, we fail to reject H₀ for each pair.

#################Star 3.4 = Bartlett Test for Homoscedasticity#################
X = mydata$Variable1

'''Grand Mean'''
xgrandmean = mean(X)
xgrandmean

'''Grand Variance'''
s2all = var(X)
sqrt_s2all = sqrt(s2all)
sqrt_s2all

'''partition this vector (Variable1) with Variable 2'''
U = unique(mydata$Variable2)
length = length(U)
X1 = X[which(mydata$Variable2 == U[1])]
X2 = X[which(mydata$Variable2 == U[2])]
X3 = X[which(mydata$Variable2 == U[3])]
X4 = X[which(mydata$Variable2 == U[4])]

'''Pooled Variance'''
Xs = split(X, mydata$Variable2)
s2j   = sapply(Xs, var)
nj    = sapply(Xs, length)
vj    = nj - 1
sp2 = sum(vj * s2j) / sum(vj)

'''Bartlett Test Stat'''
n = sum(nj)
v = sum(vj)
g = n-v
Ba_num = v*log(sp2) - sum(vj*log(s2j))
Ba_den = 1 + (1 / (3*(g-1))) * (sum(1/vj) - 1/v)
Ba_stat = Ba_num/Ba_den

'''critical values'''
df = length-1
crit_10 = qchisq(1-0.10, df)
crit_05 = qchisq(1-0.05, df)

'''p value'''
pval = 1 - pchisq(Ba_stat, df)


#################Other#################
'''construct normally-distributed data set of size n = 5, mean is 125, standard deviation is 3.7
To 2 decimal places, what would be the elements of this data set (from least to greatest)?'''
# Parameters
n <- 5
mu <- 125
sigma <- 3.7
# Construct ideal normal points (expected order statistics)
p <- (1:n) / (n + 1)    # equally spaced probabilities
z <- qnorm(p)           # corresponding z-scores
x <- mu + sigma * z     # scale and shift


'''have a data set of size 5 equal to S = {121, 121.6, 123.4, 124.7, 126.8}, 
normally distributed population whose mean is 125 and standard deviation is 3.7
what would be the associated CDF values for each of these data points '''
# Data and parameters
x <- c(121, 121.6, 123.4, 124.7, 126.8)
mu <- 125
sigma <- 3.7
# Compute CDF values
cdf_values <- pnorm(x, mean = mu, sd = sigma)


'''empirical CDF'''
zx_sample = (S - mean(S))/sd(S)
hist(zx_sample)
n = length(S)
Fx = seq(from=1, to=n, by=1)/(n+1)



