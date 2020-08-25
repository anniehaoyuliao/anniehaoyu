
# Lab12: Correlation analysis - 2

setwd("G:/我的雲端硬碟/0_R05 Graduate School/107-1/107-1_Statistics/Statistics_lab/Lab 12")

student = read.table("Student.csv", sep = ",", header = T)


# 0. Interpretation of the regression report ------------------------------

# Simple linear regression
RESULTS = lm(PartyDays ~ StudyHrs, data = student)
summary(RESULTS)

# ANOVA table
anova(RESULTS)

# Correlation coefficient
cor.test(student$PartyDays, student$StudyHrs)


# 1. Inference about the slope of regression, CI of slope ---------------------------------------

summary(RESULTS)

## Hypothesis test of the slope

# STEP1: 
#H0: beta1 = 0
#Ha: beta1 != 0
alpha = 0.05

# STEP2: 
b1 = -0.07197
se_b1 = 0.02177
t = -3.306

# STEP3: 
pvalue = 0.000995

# STEP4: 
if (pvalue <= alpha) {
  print("Reject H0. beta1 != 0")
} else {
  print("Do not reject H0. beta1 = 0")
}

#STEP5: 
print("The slope is significantly not equal to 0.")


## manual calculation examples: 

# (data cleaning before calculation)
student = student[,c("PartyDays","StudyHrs")]
student = na.omit(student)

#
x.mean = mean(student$StudyHrs)
y.mean = mean(student$PartyDays)

student$xx = student$StudyHrs - x.mean
student$yy = student$PartyDays - y.mean
student$xxyy = student$xx*student$yy
student$xx2 = student$xx^2

# b1
b1 = sum(student$xxyy) / sum(student$xx2); b1


# SSE
RESULTSS = lm(PartyDays ~ StudyHrs, data = student)
summary(RESULTSS)

yhat = RESULTSS$fitted.values
SSE = sum((student$PartyDays - yhat)^2); SSE


# standard error of residual
s = sqrt(SSE/(nrow(student)-2)); s




# 2. Prediction interval and Confidence interval -----------------------------

xx.test = c(10,20,30)

# PI
PI = predict(RESULTS, data.frame(StudyHrs = xx.test), interval = "prediction", level = 0.95)
PI

# CI
CI = predict(RESULTS, data.frame(StudyHrs = xx.test), interval = "confidence", level = 0.95)
CI

# Plotting
fit = PI[,1]

PI.low = PI[,2]
PI.high = PI[,3]

CI.low = CI[,2]
CI.high = CI[,3]


par(mfrow=c(1,2))
# plot PI
plot(PartyDays ~ StudyHrs, 
     data = student, pch = 20, col="gray50",
     main="Prediction interval", xlab="Study_Hrs", ylab="Party_Days", 
     ylim = c(-5,32),
     cex.main = 2, cex.lab = 1.2)

abline(RESULTS, col="navy") #regression line

for (i in 1:length(xx.test)) {
  lines(c(xx.test[i],xx.test[i]), c(PI.low[i],PI.high[i]), col = "red", lwd = 3)
  points(xx.test[i], PI.low[i], col = "red", pch = 15) #lower point
  points(xx.test[i], PI.high[i], col = "red", pch = 15) #upper point
  points(xx.test[i], fit[i], col = "red", pch = 8) # estimated value
}

# plot CI
plot(PartyDays ~ StudyHrs, 
     data = student, pch = 20, col="gray50",
     main="Confidence interval", xlab="Study_Hrs", ylab="Party_Days", 
     ylim = c(-5,32), 
     cex.main = 2, cex.lab = 1.2)

abline(RESULTS, col="navy") #regression line

for (i in 1:length(xx.test)) {
  lines(c(xx.test[i],xx.test[i]), c(CI.low[i],CI.high[i]), col = "green", lwd = 3)
  points(xx.test[i], CI.low[i], col = "green", pch = 15) #lower point
  points(xx.test[i], CI.high[i], col = "green", pch = 15) #upper point
  points(xx.test[i], fit[i], col = "green", pch = 8) # estimated value
}



# 3. Checking conditions --------------------------------------------------

student = read.table("Student.csv", sep = ",", header = T)

# Check na value and data cleaning
PartyDays = student$PartyDays
StudyHrs = student$StudyHrs
length(PartyDays[is.na(PartyDays)])  #0
length(StudyHrs[is.na(StudyHrs)]) #4

student = student[,c("PartyDays","StudyHrs")]
student = na.omit(student)

PartyDays = student$PartyDays
StudyHrs = student$StudyHrs


res = RESULTS$residuals

dev.off()
# Scatterplot of x vs. y
plot(PartyDays ~ StudyHrs, 
     pch = 16, cex = 1, col = "navy", 
     main="PartyDays vs. StudyHrs",
     xlab="Study_Hrs", ylab="Party_Days", 
     cex.main = 2, cex.lab = 1.2)


# Scatterplot of residuals vs. x
plot(res ~ StudyHrs, 
     pch = 16, cex = 1, col = "gold3", 
     main="PartyDays vs. StudyHrs",
     xlab="Study_Hrs", ylab="Residuals",ylim = c(-24,24),
     cex.main = 2, cex.lab = 1.2)

abline(h = 0, col = "red")


# Histogram of residuals
par(mfrow=c(1,2))

hist(res, breaks=20, border = "white", col = "olivedrab3", 
     main = "Historgram of residual", xlab = "Residuals", 
     cex.main = 2, cex.lab = 1.2)

#QQ plot
qqnorm(res, cex.main = 2, cex.lab = 1.2)
qqline(res,col="red" )




