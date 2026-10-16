# Functions 

## Tests for Normality ##
CvM_stat = function(x){
  x_sorted = sort(x) 
  n = length(x_sorted)
  F_emp = (1:n) / (n+1) 
  F_null = pnorm(x_sorted, mean(x_sorted), sd(x_sorted)) 
  CvM_stat = sum((F_emp - F_null)^2)
  
  print(CvM_stat)
}

KS_stat = function(x){
  x_sorted = sort(x) 
  n = length(x_sorted)
  F_emp = (1:n) / (n+1) 
  F_null = pnorm(x_sorted, mean(x_sorted), sd(x_sorted)) 
  KS_stat = max(abs(F_emp - F_null))
  
  print(KS_stat)
} 

AD_stat = function(x){
  x_sorted = sort(x) 
  n = length(x_sorted)
  F_emp = (1:n) / (n+1) 
  F_null = pnorm(x_sorted, mean(x_sorted), sd(x_sorted)) 
  AD_stat = sum(((abs(F_emp - F_null))^2) / (F_emp * (1 - F_emp)))
  
  print(AD_stat)
}


## Tests for Homoscedasticity ##
Ba_stat = function(x,y){
  Xs = split(x, y)
  s2j = sapply(Xs, var) # sample variance of groups
  nj = sapply(Xs, length) # same size of groups
  vj = nj - 1 #each group dof 
  sp2 = sum(vj * s2j) / sum(vj) # pooled variance
  
  g = length(Xs) #number of groups
  v = sum(vj) #total dof
  
  Ba_num = v*log(sp2) - sum(vj*log(s2j))
  Ba_den = 1 + (1 / (3*(g-1))) * (sum(1/vj) - 1/v)
  Ba_stat = Ba_num/Ba_den
  
  list(Ba_stat)
}


## Tests for Sphericity ##
mymauchly = function(x, alpha = 0.05){
  k = ncol(x)
  n = nrow(x)
  
  c = cov(x)
  tr = sum(diag(c))
  det = det(c)
  eigen_vals = eigen(c)$values
  sigma_sq_hat = mean(diag(c))
  
  W = det / ((tr / k)^k)
  
  Mstat.o = -(n-1) * log(W) 
  df.o = (k*(k-1)/2)
  Mcrit.o = qchisq(1-alpha, df.o)
  Mpval.o = pchisq(Mstat.o, df.o,lower.tail = F)

  
  Mstat.m = -((n-1) - ((2*k + 1)/6)) * log(W)
  df.m = (k*(k-1)/2) - 1
  Mcrit.m = qchisq(1-alpha, df.m)
  Mpval.m = pchisq(Mstat.m, df.m,lower.tail = F)
    
  list(trace = tr,
       determinant = det,
       eigen.values = eigen_vals,
       compound.symmetric = sigma_sq_hat,
       W = W,
       Mstat.o = Mstat.o,
       Mcrit.o = Mcrit.o,
       Mpval.o = Mpval.o,
       Mstat.m = Mstat.m,
       Mcrit.m = Mcrit.m,
       Mpval.m = Mpval.m)
}


## ANOVA ##
onefac.ANOVA = function(x,y){
  Xs = split(x,y)
  
  xbarj = sapply(Xs, mean)
  s2j = sapply(Xs, var)
  nj = sapply(Xs, length)
  vj = nj - 1
  sp2 = sum(vj * s2j) / sum(vj)
 
  n = sum(nj)
  v = sum(vj)
  g = n-v
  
  s2all = var(x)
  xbarbar = mean(x)
 
  SST = s2all*(n-1) #total
  SSE = sum(s2j*vj) #within groups
  SSM = sum(nj*(xbarj - xbarbar)^2) #between groups 

  total.dof = n-1
  within.dof = n-g
  between.dof = g-1
  
  MST = SST/(n-1) #total
  MSE = SSE/(n-g) #within
  MSM = SSM/(g-1) #between
  Fstat = MSM/MSE
  p = pf(Fstat, between.dof, within.dof, lower.tail=F )

  list(s2all = s2all,
       xbarbar = xbarbar,
       SST = SST,
       SSM = SSM,
       SSE = SSE,
       MSM = MSM,
       MSE = MSE,
       Fstat = Fstat,
       pval = p)
}

welch.ANOVA = function(x,y){
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
  
  list(s2all = s2j_h,
       xbarbar = xbarbar_h,
       MSE = v_h,
       MSM = msm_h,
       SSM = ssm_h,
       Fstat = F_stat_h,
       pval = p)
}

twofac.ANOVA = function(Y, X1, X2) {
  # Y 
  nall = length(Y)
  xbarbar = mean(Y)
  s2all = var(Y)
  
  # A
  g1 = length(unique(X1)) 
  XsA = split(Y, X1)
  nA = sapply(XsA, length) 
  
  # B
  g2 = length(unique(X2)) 
  XsB = split(Y, X2)
  nB = sapply(XsB, length) 
  
  # Interaction matrix
  cells = split(Y, interaction(X1, X2, drop = TRUE, lex.order = TRUE))
  cell_ns = matrix(sapply(cells, length), nrow = g1, ncol = g2, byrow = TRUE)
  cell_means = matrix(sapply(cells, mean), nrow = g1, ncol = g2, byrow = TRUE)
  cell_vars = matrix(sapply(cells, var), nrow = g1, ncol = g2, byrow = TRUE)
  row_means = matrix(sapply(XsB, mean), nrow = 1, ncol = g2 , byrow = TRUE)
  col_means = matrix(sapply(XsA, mean), nrow = g1, ncol = 1, byrow = TRUE)
  
  #SSE
  SSE = sum((cell_ns - 1) * cell_vars)
  dfE = nall - (g1 * g2)
  MSE = SSE / dfE
  
  # Hypothesis 1 (Does changing Factor A make a difference in the outcome, on average?)
  xbarA = sapply(XsA, mean)
  SSA = sum(nA * ((xbarA - xbarbar)^2))
  dfA = g1 - 1
  MSA = SSA / dfA
  FA = MSA / MSE
  pA = pf(FA, dfA, dfE, lower.tail = F)
 
  # Hypothesis 2 (Does changing Factor B make a difference in the outcome, on average?)
  xbarB = sapply(XsB, mean)
  SSB = sum(nB * ((xbarB - xbarbar)^2))
  dfB = g2 - 1
  MSB = SSB / dfB
  FB = MSB / MSE
  pB = pf(FB, dfB, dfE, lower.tail = F)  
 
  # Hypothesis 3 (Does the effect of one factor depend on the level of the other factor?)
  SSAB = 0 
  for (i in 1:g1){
    for (j in 1:g2){
      SSAB = SSAB + (cell_ns[i,j] * ((cell_means[i,j] - xbarbar)^2))
    }
  }
  
  dfAB = (g1 - 1) * (g2 - 1)

  MSAB = SSAB / dfAB
  FAB = MSAB / MSE
  pAB = pf(FAB, dfAB, dfE, lower.tail = F)
  
  list(
    SSE = SSE, 
    SSA = SSA, FA = FA, pA = pA,
    SSB = SSB, FB = FB, pB = pB,
    SSAB = SSAB, FAB = FAB, pAB = pAB)
}

rm.ANOVA = function(X){
  xbarj = colMeans(X)
  xbari = rowMeans(X)
  xbarbar = mean(X)
  s2all = var(X)
  
  n = nrow(X)
  g = ncol(X)
  
  SST = s2all * (n*g-1)
  SSM = n*sum((xbarj - xbarbar)^2)
  SSS = g*sum((xbari - xbarbar)^2)
  E = X - xbari - matrix(xbarj, n, g, byrow=T) + xbarbar
  SSE = sum(E^2)
  
  MST = SST / ((n*g) - 1)
  MSM = SSM / (g-1)
  MSS = SSS / (n-1)
  MSE = SSE / ((n-1) * (g-1))
  
  Fstat = MSM / MSE
  pval = pf(Fstat, (g-1), ((n - 1) * (g - 1)), lower.tail = FALSE)
  
  list(
    SST = SST, SSM = SSM, SSS = SSS, SSE = SSE,
    MST = MST, MSM = MSM, MSS = MSS, MSE = MSE,
    Fstat = Fstat, pval = pval)
}

nonspher.rmANOVA = function(X){
  xbarj = colMeans(X)
  xbari = rowMeans(X)
  xbarbar = mean(X)
  s2all = var(X)
  
  n = nrow(X)
  g = ncol(X)
  Y = X - rowMeans(X)
  c = cov(Y)           
  
  SST = s2all * (n*g-1)
  SSM = n*sum((xbarj - xbarbar)^2)
  SSS = g*sum((xbari - xbarbar)^2)
  E = X - xbari - matrix(xbarj, n, g, byrow=T) + xbarbar
  SSE = sum(E^2)
  
  # severe sphericity violation (Greenhouse-Geisser)
  esp_GG = ((sum(diag(c)))^2) / ((g - 1) * sum(c^2))
  MSM_GG = SSM / ((g-1) * esp_GG)
  MSE_GG = SSE / ((n-1) * (g-1))
  Fstat_GG = MSM_GG / MSE_GG
  
  # mild sphericity violation (Huynh-Feldt)
  esp_HF = (n * (g - 1) * esp_GG - 2) / ((g - 1) * (n - 1) - (g - 1) * esp_GG)
  MSM_HF = SSM / ((g-1) * esp_GG)
  MSE_HF = SSE / ((n-1) * (g-1))
  Fstat_HF = MSM_HF / MSE_HF
  
  list(
    esp_GG = esp_GG, MSM_GG = MSM_GG, MSE_GG = MSE_GG, 
    Fstat_GG = Fstat_GG, 
    esp_HF = esp_HF, MSM_HF = MSM_HF, MSE_HF = MSE_HF, 
    Fstat_HF = Fstat_HF)
}


## Bootstrap Sampling ##
onesamp_bootSM = function(X, S, N, alpha){
  set.seed(S)
  mymean = c()
  
  for(k in 1:N){
    sboot = sample(X, length(X), replace=T)
    mymean = c(mymean, mean(sboot))
  }
  
  cimean = quantile(mymean, c(alpha/2, 1-alpha/2))
  return(cimean)
} #sample mean

twosamp_bootCC = function(X, Y, S, N, alpha){
  set.seed(S)
  mycor = c()
  
  n = length(X)
  myindex = seq(1,n,1)
  
  for(k in 1:N){
    sboot = sample(myindex, n, replace=T)
    Xboot = X[sboot]
    Yboot = Y[sboot]
    
    mycor = c(mycor, cor(Xboot,Yboot))
  }
  
  cirxy = quantile(mycor, c(alpha/2, 1-alpha/2))
  return(cirxy)
} #correlation coefficient

twosamp_bootLSR = function(X, Y, S, N, alpha){
  set.seed(S)
  n = length(X)
  
  slope = c()
  intercept = c()
  r2 = c()
  rmse = c()
  
  myindex = seq(1,n,1)
  
  for(k in 1:N){
    sboot = sample(myindex, n, replace=T)
    Xboot = X[sboot]
    Yboot = Y[sboot]
    
    b1 = cor(Xboot, Yboot) * sd(Yboot)/sd(Xboot)
    b0 = mean(Yboot) - b1*mean(Xboot)
    
    yhat = b0 + b1 * Xboot
    res = Yboot - yhat
    
    slope = c(slope, b1)
    intercept = c(intercept, b0)
    r2 = c(r2, 1 - sum(res^2) / sum((Yboot - mean(Yboot))^2))
    rmse = c(rmse, sqrt(mean(res^2)))
  }
  
  ci.slope = quantile(slope, c(alpha/2, 1-alpha/2))
  ci.intercept = quantile(intercept, c(alpha/2, 1-alpha/2))
  ci.r2 = quantile(r2, c(alpha/2, 1 - alpha/2))
  ci.rmse = quantile(rmse, c(alpha/2, 1 - alpha/2))
  
  list(
    slope = ci.slope, intercept = ci.intercept,
    CoefficientDetermination = ci.r2, 
    RootMeanSquareError = ci.rmse)
} #least squares regression


## Outlier Detection ##
grubbs = function(X,Y, alpha){
  n = length(X)
  T = qt(alpha/(2*n), n-2, lower.tail=F)
  
  xstar = X[which.max(abs(X - mean(X)))]
  ystar = Y[which.max(abs(X - mean(X)))]
  
  gstat = max(abs(X - mean(X))) / sd(X)
  gcrit = ((n-1)/ sqrt(n)) * sqrt(T^2 / (T^2 + (n-2)))

  list(
    xstar = xstar, 
    ystar = ystar, 
    gstat = gstat, 
    gcrit = gcrit)
}

TietjenMoore = function(X, num){
  X = sort(X)
  n = length(X)
  xstar = X[(n - num + 1):n]
  xor = X[1:(n - num)]
  
  xbar = mean(X)
  xbaror = mean(xor)
  
  TMstat_top = sum((xor - xbaror)^2)
  TMstat_bot = sum((X - xbar)^2)
  TMstat = TMstat_top / TMstat_bot
  
  TMstat.m = -log(TMstat)
  
  zstat_top = TMstat.m - log((n-1)/ (n - num - 1))
  zstat_bot =  sqrt((2 / (n - num - 1)) + (2 / (n-1)))
  zstat = zstat_top/ zstat_bot
  
  pval = 2*pnorm(zstat, lower.tail=F)
  
  list(
    TMstat = TMstat, TMstat.Modified = TMstat.m,
    zstat = zstat, pval = pval)
}


## Non-Parametric Methods ##
sign_percentiles = function(X, mu0, p0){
  n = length(X)
  phat = sum(X <= mu0) / n
  qhat = quantile(X, p0)
  SE = sqrt((phat * (1 - phat)) / n)
  Z = (phat - p0) / SE
    
  list(
    percentile.estimate = phat, 
    quantile.estimate = qhat,
    z_stat = Z)
}

two_rank_based = function(s1, s2, alpha=0.05){
  L = list('s1' = s1, 's2' = s2)
  nj = sapply(L,length)
  n = sum(nj)
  
  Sall = unlist(L)
  idx = rep(names(L), times = nj)
  Rall = rank(Sall)
  Rj = split(Rall, idx)
  Tj = sapply(Rj, sum)
  avg_rank = sapply(Rj, mean)
  
  CI_stat = abs((Tj["s1"] - Tj["s2"]) / 
                  sqrt(((n * (n+1)/12)) * ((1/nj["s1"]) + (1/nj["s2"]))))
  
  CI_stat.m = abs((avg_rank["s1"] - avg_rank["s2"]) / 
                    sqrt(((n * (n+1)/12)) * ((1/nj["s1"]) + (1/nj["s2"]))))
  
  Uj = Tj - ((nj * (nj+1)) / 2)
  MW_stat = min(Uj["s1"],Uj["s2"])
  
  if (prod(nj) > 20000) {
    MW_crit = "SKIP"
    MW_pval = "SKIP"
  } else {
    Z = (MW_stat - (nj["s1"] * nj["s2"]) / 2) / sqrt((nj["s1"] * nj["s2"] * (n + 1)) / 12)
    MW_crit = qwilcox(alpha, nj["s1"], nj["s2"], lower.tail = T)
    MW_pval = pnorm(abs(Z), lower.tail = F)  
  }
  
  list(
    totals = nj,
    total_rank = Tj,avg_rank = avg_rank,
    CI_stat = CI_stat, CI_stat.m = CI_stat.m,
    MW = Uj, MW_stat = MW_stat, 
    MW_crit = MW_crit, MW_pval = MW_pval)
}


multi_rank_based = function(..., alpha=0.05){
  L = list(...)
  k= length(L)
  nj = sapply(L,length)
  n = sum(nj)
  
  Sall = unlist(L)
  idx = rep(names(L), times = nj)
  Rall = rank(Sall)
  Rj = split(Rall, idx)
  Tj = sapply(Rj, sum)
  Rbar_j = sapply(Rj, mean)
  Rbar = mean(Rall)
  
  tie_table = table(Rall)
  has_ties = any(tie_table > 1)
  
  if (!has_ties) {
    KW_stat_o = (12 / (n * (n + 1))) * sum(nj * (Rbar_j - Rbar)^2)
  } else {
    num = sum(nj * (Rbar_j - Rbar)^2)
    den = sum((Rall - Rbar)^2)
    KW_stat_o = (n - 1) * num / den
  }
  p_val = 1 - pchisq(KW_stat_o, k-1)
  KW_crit = qchisq(1 - alpha, k - 1)
  
  den = 0
  for (i in 1:k) {
    den = den + sum((Rj[[i]] - Rbar_j[i])^2)
  }
  KW_stat_m = (n - 1) * sum(nj * (Rbar_j - Rbar)^2) / den

  list(
    totals = nj, total_rank = Tj,
    avg_rank = Rbar_j, grand_rank = Rbar,
    KW_stat_o = KW_stat_o, p_val = p_val,
    KW_crit = KW_crit, KW_stat_m = KW_stat_m)
}








