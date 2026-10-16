
## M02.5 - Monte-Carlo Simulations

EnvStudyData <- read_csv("Desktop/MA416/EnvStudyData_9.25.csv")


#homework 2

###question 1
xbar= mean(EnvStudyData$AverageTemperature_C)
s= sd(EnvStudyData$AverageTemperature_C)
mu0= 35

observed_astat= abs(xbar-mu0)/ (s+1)
observed_astat

###question 2
set.seed(12121)
mc_astats <- c()
nmc = 10000
n = length(EnvStudyData$AverageTemperature_C)

null_mean = 32
null_sd = 2.9

# function to compute Astat
mystat <- function(x, mu0) {
  abs(mean(x) - mu0) / (sd(x) + 1)
}

# Monte Carlo loop
for(k in 1:nmc){
  sim_sample <- rnorm(n, mean = null_mean, sd = null_sd)
  mc_val <- mystat(sim_sample, mu0)
  mc_astats <- c(mc_astats, mc_val)
}

# Q2: Expected value under null
E_astat <- mean(mc_astats)
E_astat

# Q3: Critical value for right-tailed test (alpha = 0.085)
alpha <- 0.085
Acrit <- quantile(mc_astats, probs = 1 - alpha)
Acrit

# Q4: Monte Carlo p-value vs observed statistic
xbar <- mean(EnvStudyData$AverageTemperature_C)
s <- sd(EnvStudyData$AverageTemperature_C)
observed_astat <- abs(xbar - mu0) / (s + 1)

pval <- mean(mc_astats >= observed_astat)
pval



