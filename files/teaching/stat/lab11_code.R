
rm(list = ls())

setwd("G:/我的雲端硬碟/0_R05 Graduate School/107-1/107-1_Statistics/Statistics_lab/Lab 11")

setwd("C:/Users/Annie/Downloads")

student = read.table("Student.csv", sep = ",", header = T)
car = read.table("Vehicles.csv", sep = ",", header = T)


# Lab 11 ------------------------------------------------------------------


head(student)

PartyDays = student$PartyDays
StudyHrs = student$StudyHrs


# Scatterplot
plot(PartyDays ~ StudyHrs, 
     pch = 16, cex = 1, col = "navy", 
     main="PartyDays vs. StudyHrs",
     xlab="Study_Hrs", ylab="Party_Days")

#or
plot(StudyHrs, PartyDays, 
     pch = 16, cex = 1, col = "navy", 
     main="PartyDays vs. StudyHrs",
     xlab="Study_Hrs", ylab="Party_Days")


# Correlation coefficient
cor.test(PartyDays, StudyHrs)

#Is there any NA value in the data?
length(PartyDays[is.na(PartyDays)])
length(StudyHrs[is.na(StudyHrs)])



# Simple linear regression
RESULTS = lm(PartyDays ~ StudyHrs)
summary(RESULTS)


coeff = coefficients(RESULTS) #coefficients
RESULTS$coefficients

res = residuals(RESULTS) #residuals
RESULTS$residuals

yhat = fitted.values(RESULTS) #estimated y (yhat)
RESULTS$fitted.values


dev.off()

plot(PartyDays ~ StudyHrs, pch = 16, col="blue",
     main="PartyDays vs. StudyHrs", xlab="Study_Hrs",
     ylab="Party_Days")

abline(RESULTS, col="red") #regression line


# ANOVA
anova(RESULTS)

#
SSR = 320.6
SSE = 20062.6
r.square = SSR / (SSR+SSE); r.square



# Homework ----------------------------------------------------------------

head(car)
veh = car$Vehicle
gdp = car$GDP

# Scatterplot
plot(veh ~ gdp, main="Number of vehicle vs. GDP",
     xlab="GDP", ylab="Nuber of vehicles")

plot(gdp, veh, main="Number of vehicle vs. GDP",
     xlab="GDP", ylab="Nuber of vehicles")

# Correlation coefficient
cor.test(veh, gdp)

dev.off()


# Simple linear regression
RESULTS = lm(veh ~ gdp)
summary(RESULTS)

coeff = coefficients(RESULTS) #coefficients
res = residuals(RESULTS) #residuals
yhat = fitted.values(RESULTS ) #estimated y (yhat)


plot(veh ~ gdp, pch = 16, col="blue",
     main="Number of vehicle vs. GDP",
     xlab="GDP", ylab="Nuber of vehicles")

abline(RESULTS, col="red") #regression line


