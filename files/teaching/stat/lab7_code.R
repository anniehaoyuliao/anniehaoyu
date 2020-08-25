
# 107-1 Statistics Lab7: Estimating means with confidence


# Lab#1: t distribution with different df ---------------------------------

# t-distribution with df = 30
curve(dt(x, df=30), from = -3, to = 3, lwd = 4, ylab = "y")

# t-distribution with different df
curve(dt(x, df=2), from = -3, to = 3, lwd = 2, ylab = "y", add = T, col = "gray86")
curve(dt(x, df=5), from = -3, to = 3, lwd = 2, ylab = "y", add = T, col = "gray35")

ind = c(1,2,3,5,10,100)
for (i in ind) {
  curve(dt(x, df=i), from = -3, to = 3, lwd = 1.5, ylab = "y", add = T, col = "gray")
}

# standard normal distribution
curve(dnorm(x), from = -3, to = 3, lwd = 3, ylab = "y", add = T, col = "red")



# Lab#2: t-distribution functions ---------------------------------------

qt(0.01, df = 10, lower.tail = FALSE)
qt(0.01, df = 200, lower.tail = FALSE)
qnorm(0.01, lower.tail = FALSE)

pt(2, df = 10, lower.tail = FALSE)
pt(2, df = 200, lower.tail = FALSE)
pnorm(2, lower.tail = FALSE)

#rt


# Lab#3: Find the confidence interval -------------------------------------

x <- rnorm(20) #one sampling
cc <- t.test(x, alternative = "two.sided", conf.level=0.95) #t.test(): t檢定函數
cc
CI_Low <- cc$conf.int[1]; CI_Low
CI_High <- cc$conf.int[2]; CI_High


# Lab#4: Simulating multiple confidence intervals ----------------------------

Sim_CI <- function (n, mu, sd, m, conf.level){
  
  prob<-(1-conf.level)/2
  t.value=qt(prob, df = n-1, lower.tail = FALSE)
  
  mean.x = c()
  se.x = c()
  
  CI.half = c()
  CI.Up = c()
  CI.Lower = c()
  Check = c()
  
  # Multiple sampling
  for (i in 1:m) {
    
    # One sampling
    x <- rnorm(n, mean = mu, sd = sd)
    mean.x[i]<-mean(x)
    se.x[i]<-sd(x)/sqrt(n)
    
    # Calculate one confidence interval
    CI.half[i] <- t.value * se.x[i]
    CI.Up[i] <- mean.x[i] + CI.half[i]
    CI.Lower[i] <-mean.x[i] - CI.half[i]
    if (CI.Up[i]>mu & CI.Lower[i]<mu) Check[i]<-1 else Check[i]<-0
  }
  
  plot(c(CI.Up,CI.Lower),type="n",pch=19, xlim=c(1,m),
       xlab="Trial", ylab=expression(mu))
  
  abline(h = mu, col = "blue")
  
  # Check whether each confidence interval captures the population mean (mu)
  for (i in 1:m) {
    if (Check[i]==1) {
      
      points(i, mean.x[i], col = "green", pch = 10)
      points(i, CI.Up[i], col = "green", pch = 20)
      points(i, CI.Lower[i], col = "green", pch = 20)
      lines(c(i,i), c(CI.Lower[i],CI.Up[i]), col = "green", pch = 19)
      
    } else {
      
      points(i, mean.x[i], col = "red", pch = 10)
      points(i, CI.Up[i], col = "red", pch = 20)
      points(i, CI.Lower[i], col = "red", pch = 20)
      lines(c(i,i), c(CI.Lower[i],CI.Up[i]), col = "red", pch = 19)
    }
  }
  
  title(expression(paste("Simulating confidence interval for ", mu)))
  
  legend("bottomright", cex=0.6, bty = "n", ncol = 2, 
         c(expression(paste(mu," Captured")), expression(paste(mu," Not Captured"))),
         fill = c("green","red"))
  
  No.Caputured <- m-sum(Check) #Check(captured)
  RESULT <- list(Trial=m, Sample.Size=n, 
                 Population.mean = mu, Population.sd = sd, 
                 Confidence=conf.level,
                 No.Caputured=No.Caputured)
  
  return(RESULT)
}

Sim_CI(n = 30, m = 100, mu = 50, sd = 5, conf.level =0.95)  







