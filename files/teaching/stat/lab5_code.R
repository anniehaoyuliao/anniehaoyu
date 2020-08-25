
# 107-1 Statistics Lab7: Sampling distributions and parameters


# 1) Example 9.4_Sample proportions --------------------------------------

# Generate random variables (phat: proportion)
bino.x = function(n, p) {
  
  pp = p*10
  samp = sample(1:10, n, replace = T)
  
  response = c()
  for (i in 1:length(samp)) {
    if (samp[i] <= pp) {
      response[i] = 1
    } else {
      response[i] = 0
    }
  }
  
  #another way to write
  samp = sample(x = c(0,1), n, replace = T, prob = c(1-p,p))
  response = samp
  #
  
  x = sum(response)
  phat = x/n
  return(phat)
}


# Sampling multiple times (for sample distribution)
simu.phat = c()
for (j in 1:1000) {
  x = bino.x(n = 2400, p = 0.4)
  simu.phat[j] = x
}


# Draw the sampling distribution
hist(simu.phat, probability = T, breaks = 20, 
     main = "Example 9.4_Sample distribution", xlab = "Sample proportions (p hat)", 
     col = "gold3", border = "white")


# calculate the simulated "mean of proportions" and "sd of proportions"
simu.mean = mean(simu.phat); simu.mean
simu.sd = sd(simu.phat); simu.sd


# calculate theoretical "mean of proportions" and "sd of proportions"
n = 2400
p = 0.4
theo.mean = p; theo.mean
theo.sd = sqrt(p*(1-p)/n); theo.sd

# Overlapping theoretical results
curve(dnorm(x, mean = theo.mean, sd = theo.sd), add = T, col = "navy", lwd = 2)


# Calculating z-score
simu.z = (simu.phat - theo.mean) / (theo.sd)

#distribution of z-score
hist(simu.z, probability = T, breaks = 20, 
     main = "Example 9.4_Sample distribution (standardized)", xlab = "Standard Sample proportions (p hat)", 
     col = "azure3", border = "white")


# 3) Example 9.8_Sample means --------------------------------------

# Generate random variables (samp: each observation; xbar = mean of samples)
norm.x = function(mu, sigma, n) {
  samp = rnorm(n, mean = mu, sd = sigma)
  xbar = mean(samp)
  
  return(xbar)
}

# Sampling multiple times (for sample distribution)
simu.xbar = c()
for (j in 1:1000) {
  x = norm.x(mu = 8, sigma = 5, n = 25)
  simu.xbar[j] = x
}


# Draw the sampling distribution
hist(simu.xbar, probability = T, breaks = 20, 
     main = "Example 9.8_Sample distribution", xlab = "Sample means (x bar)", 
     col = "olivedrab3", border = "white")


# calculate the simulated "mean of means" and "sd of means"
simu.mean = mean(simu.xbar); simu.mean
simu.sd = sd(simu.xbar); simu.sd


# calculate theoretical "mean of means" and "sd of means"
mu = 8
sigma = 5
n = 25

theo.mean = mu; theo.mean
theo.sd = sigma/sqrt(n); theo.sd


# Overlapping theoretical results
curve(dnorm(x, mean = theo.mean, sd = theo.sd), add = T, col = "navy", lwd = 2)


# Calculating z-score
simu.z = (simu.xbar - theo.mean) / (theo.sd)

#distribution of z-score
hist(simu.z, probability = T, breaks = 20, 
     main = "Example 9.8_Sample distribution (standardized)", xlab = "Standardized Sample means (x bar)", 
     col = "azure3", border = "white")













