## Homework 3.2: Distribution Testing 

###
#"Efficiency Factor" variable, X, as your sample of focus
#Suppose that you wish to assess the null hypothesis 
# H0: X~N (mu=4, sigma= 2)
#To 4 decimal places, what is the value of the following test statistics in reference to the null hypothesis above?

####### Question 5 ####### 

Data <- read.csv("Desktop/MA416/CompanyPayroll_9.25.csv")
x <- Data$Efficiency_Factor_Obs

# Step 1: Sort data
x_sorted <- sort(x)
n <- length(x_sorted)

# Step 2: CDF
F_emp <- (1:n) / (n+1)
F_null <- pnorm(x_sorted, mean=4, sd=2)

# --- Kolmogorov–Smirnov Statistic ---
KS_stat <- max(abs(F_emp - F_null))
KS_stat

# --- Cramér–von Mises Statistic ---
CvM_stat <- sum((F_emp - F_null)^2)
CvM_stat

# --- Anderson–Darling Statistic ---
AD_stat <- sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
AD_stat


###################################################


## Homework 3.2: Part 2 


####### Question 2 ####### 
# H0: X~N (mu, sigma)
Data <- read.csv("Desktop/MA416/CompanyPayroll_9.25.csv")
x <- Data$Efficiency_Factor_Obs

# Step 1: Sort data
x_sorted <- sort(x)
n <- length(x_sorted)

# Step 2: CDF
F_emp <- (1:n) / (n+1)

mean_obs= mean(x)
sd_obs= sd(x)
F_null <- pnorm(x_sorted, mean_obs, sd_obs)

# --- Kolmogorov–Smirnov Statistic ---
KS_stat <- max(abs(F_emp - F_null))
KS_stat

# --- Cramér–von Mises Statistic ---
CvM_stat <- sum((F_emp - F_null)^2)
CvM_stat

# --- Anderson–Darling Statistic ---
AD_stat <- sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
AD_stat

####### Question 3 ####### 

# Monte Carlo for Anderson–Darling Statistic (10,000 sims)
set.seed(123123)
nmc <- 10000
mc_ad <- c()

Data <- read.csv("Desktop/MA416/CompanyPayroll_9.25.csv")
x <- Data$Efficiency_Factor_Obs
n <- length(x)

for (k in 1:nmc) {
  # simulate sample from standard normal
  Smc <- rnorm(n, 0, 1)
  Smc <- sort(Smc)
  
  # empirical CDF for this simulated sample
  Fxmc <- (1:n) / (n+1)
  
  # null CDF values
  Psixmc <- pnorm(Smc, 0, 1)
  
  # AD statistic for this simulated dataset
  mc_ad <- c(mc_ad, sum((Fxmc - Psixmc)^2 / (Fxmc * (1 - Fxmc))))
}

# Histogram
hist(mc_ad, breaks=100, col="magenta",
     main="Empirical MC AD Distribution", xlab="AD Statistic")


####### Question 4 ####### 
emp_pval = mean(mc_ad >= 1.1)
emp_pval

####### Question 5 ####### 
emp_ad_crit = quantile(mc_ad, 1-0.01)
emp_ad_crit



