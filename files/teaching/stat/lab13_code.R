
# Lab13: Correlation analysis - Categorical variables

rm(list = ls())

# Read data
setwd("G:/我的雲端硬碟/0_R05 Graduate School/107-1/107-1_Statistics/Statistics_lab/Lab 13")

data = "heartatk.csv"
Heart_Table = read.table(data, header=TRUE, sep=",")
head(Heart_Table)


# Generate a two-way table
MyTable = xtabs(~ SEX + DIED, data = Heart_Table) # generate a 2x2 table
MyTable

# total number of observations:
sum(MyTable)



# 1. Risk and Odds --------------------------------------------------------

MyTable


# 1(1) By manual calculation: 

## risk
risk.F = MyTable[1,2] / sum(MyTable[1,]); risk.F
risk.M = MyTable[2,2] / sum(MyTable[2,]); risk.M

## relative risk
relarisk.M_F = risk.M / risk.F; relarisk.M_F  #base: Female

## percent increase in risk
incrisk.M_F = (rela.risk.M_F-1); incrisk.M_F
incrisk.M_F = (risk.M-risk.F) / risk.F; incrisk.M_F

## odds
odds.F = MyTable[1,2] / MyTable[1,1]; odds.F
odds.M = MyTable[2,2] / MyTable[2,1]; odds.M

## odds ratio
oddsratio.M_F = odds.M / odds.F; oddsratio.M_F  #base: Female
oddsratio.F_M = odds.F / odds.M; oddsratio.F_M  #base: Male


# 1(2) By functions: 
#install.packages("epitools")
library(epitools)

epitab(MyTable, method = "riskratio")  #relative risk
relarisk.M_F = risk.M / risk.F; relarisk.M_F  #base: Female

epitab(MyTable, method = "oddsratio")  #odds ratio
oddsratio.M_F = odds.M / odds.F; oddsratio.M_F  #base: Female

epitab(MyTable, method = "oddsratio", rev = "row")  #change the base to M
oddsratio.F_M = odds.F / odds.M; oddsratio.F_M  #base: Male



# 2. Chi-square test for two-way table ------------------------------------
#    (homogeneity test, independence test)

# 2(1) By manual calculation: 

# row total, column total, total
total.0 = sum(MyTable[,1]); total.0
total.1 = sum(MyTable[,2]); total.1
total.F = sum(MyTable[1,]); total.F
total.M = sum(MyTable[2,]); total.M

total = sum(MyTable)

# generate an expected table
ExpTable = MyTable
ExpTable[1,1] = total.0*total.F / total
ExpTable[1,2] = total.1*total.F / total
ExpTable[2,1] = total.0*total.M / total
ExpTable[2,2] = total.1*total.M / total

ExpTable


# calculate chi-square statistic
chi.11 = ((MyTable[1,1] - ExpTable[1,1])^2) / ExpTable[1,1]
chi.12 = ((MyTable[1,2] - ExpTable[1,2])^2) / ExpTable[1,2]
chi.21 = ((MyTable[2,1] - ExpTable[2,1])^2) / ExpTable[2,1]
chi.22 = ((MyTable[2,2] - ExpTable[2,2])^2) / ExpTable[2,2]

chi.square = sum(chi.11, chi.12, chi.21, chi.22); chi.square


# p-value
df = (nrow(MyTable)-1) * (ncol(MyTable)-1); df

leftp = pchisq(chi.square, df = df)
p.value = 1-leftp
p.value



# 2(2) By functions: 
RESULTS = chisq.test(MyTable, correct = F) 
RESULTS

# expected table
exp.table = RESULTS$expected
exp.table


##z test
prop.test(x = c(767,643), n = c(5065,7779), alternative = "two.sided", correct = F)



# 3. Goodness-of-fit test: Testing uniform distribution ---------------------------

## step1:

## step2:

#observed values
obs = c(47,50,55,46,53,39,55,55,44,56)
n = 500
p = 0.1

#expected values
exp = rep(n*p, length = length(obs))

tbl = data.frame(obs,exp)

#check conditions

#calculate the chi-square statistic
tbl$chii = (tbl$obs-tbl$exp)^2/ tbl$exp
chi.square = sum(tbl$chii); chi.square


## step3:
df = length(obs) - 1
p.value =  1 - pchisq(chi.square, df = df); p.value

## step4: 
alpha = 0.05
if (p.value <= alpha) {
  print("Reject H0.")
} else {
  print("Do not reject H0.")
}

## step5:



# By chisq.test()
chisq.test(tbl$obs, p = rep(0.1,length = 10), correct = F)





# HW1 hint: Write a function for Chi-square test (2*2) ---------------------------

MyChiSq <- function(table) {
  
  
  
  # generate an expected table
  
  
  
  # calculate each (observed-expected)^2/expected
  # hint: You can use 2 for-loops to specify a single cell in the table
  
  
  
  # calculate Chi-square statistics
  
  
  
  # calculate p-value
  # hint: pchisq()
  
  
  
  # results of the function
  # hint: results = list(X.squared = Chi, df = df, p.value = p.value)
  # hint: return(results)
  
  
}


MyChiSq(MyTable)


# HW2 hint: testing binomial distribution ----------------------------------


## step1:


## step2:
x = c(0,1,2,3,4,5,6,7)

#observed values
obs = c(5,13,26,19,20,7,0,0)
n = 90
pp =


#expected values
exp = c()
for (i in 1:length(obs)) {
  exp[i] = dbinom() * n
}

tbl = data.frame(x, obs, exp)


#check conditions

#combining 6,7,8 rows
tbl.678 = tbl[6,] + tbl[7,] + tbl[8,] #add up 6,7,8 rows
tbl = tbl[-c(6,7,8),] #remove 6,7,8 rows
tbl = rbind(tbl, tbl.678) #add the combined row

#calculate the chi-square statistic



## step3:
df = nrow(tbl)-1-1 #k-1-r


  
## step4:
alpha = 0.05


## step5:






# HW3 hint: Testing normal distribution ---------------------------------------------


## step1:

## step2:
pm2.5 = c(18.8, 14.6, 14.0, 15.8, 12.4, 13.2, 16.1, 13.8, 16.2, 
          16.1, 17.8, 18.7, 15.8, 13.3, 13.6, 16.4, 13.8, 16.6, 
          15.3, 19.0, 18.4, 15.0, 18.8, 18.1, 17.3, 16.3, 17.5, 
          18.1, 14.2, 18.0, 13.0, 13.3, 12.4, 16.6, 14.1, 20.6, 
          16.8, 13.3, 18.2, 16.9)


#defining thresholds to categorize continuous data
mean = mean(pm2.5)
sd = sd(pm2.5)

n = length(pm2.5)

thres = c(mean-3*sd, mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd, mean+3*sd)


#observed values (continuous --> discrete)
obs = rep(0, length = 8)

for (i in 1:length(pm2.5)) {
  if () {
    
  } else if () {
    
  } else if () {
    
  }
  
  
  else {
    
  }
}


#expected values
cumu.p = c(pnorm(), 
           pnorm() - pnorm(), 
           
           
           )

exp = cumu.p * n


cate = c("< m-3d", "m-3d ~ m-2d", "m-2d ~ m-d", "m-d ~ m", "m ~ m+d", "m+d ~ m+2d", "m+2d ~ m+3d", "> m+3d")

tbl = data.frame(obs, exp)
rownames(tbl) = cate

#check conditions



## step3:
df = nrow(tbl)-1-2  # k-1-r


## step4:
alpha = 0.05


## step5:













