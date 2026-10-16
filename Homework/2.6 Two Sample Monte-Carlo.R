# 2.6 Two Sample Monte-Carlo Simulations 

#H_0: mu1 = mu2

#if normal, start with F to acess homoscedastity then do pooled/unpooled
#but we won't make assumptions... 

# starstat = 1/(abs(xbar1/xbar2) + 1) # +1 incase xbar1/2 is 0 and then it eliminates the denomenator 


# is E(starstat) = 1/(abs())
mystar = function(S1, S2){
  xbar1 = mean(S1)
  xbar2 = mean(S2)
  val = 1/(abs(xbar1-xbar2) + 2)
  return(val)
}

set.seed(1234)
S1 = rnorm(17, 21.5, 1.3)
S2 = rnorm(27, 24.3, 1.9)

starstat = mystar(S1, S2)
starstat

#is 0.53 good evidence to support means are close to each other?
# ideal value for not rejecting the null is 0.5 
# depends on critical value 
# if crit value is 0.51, starstat is too big
# if crit value is 0.6, star stat is small and not reject null 

# Null distribution for both is that they are normally distributed 

nmc = 100000 #number of monte carlo similuations 
set.seed(90909)
mcstars = c()
for(k in 1:nmc){
  smc1 = rnorm(17, 0, 1) # same size as our first sample, with sample normal distribution
  smc2 = rnorm(27, 0, 1) 
  mcval = mystar(smc1, smc2) #test stat
  mcstars = c(mcstars, mcval) 
}

hist(mcstars)
# only left tail gives you rejection 

mean(mcstars <= starstat) #less than 0.5 only exists in left tail

starstat2 = 0.37
mean(mcstars <= starstat2) 

# pval compared to alpha 
# test stat compared to critical value 

########
# normal dis 0 in middle and 50% on each side 
 #Q) Did our sample(s) really come from a normally distributed population? 
      # yes bc we used rnorm 

S = rnorm(78, 123, 5.7)
hist(S)
# does it look exactly normal? No, just bc you come from a normal place, doesn't mean it'll always be exactly normal

zx_sample = (S - mean(S))/sd(S)
hist(zx_sample)
n = length(S)
Fx = seq(from=1, to=n, by=1)/(n+1) # empirical CDF (different from ecdf which chat might give)

zx_true = qnorm(Fx, 0, 1, lower.tail=T) #left-tail normal quantiles 
zx
plot(zx_true, Fx)


