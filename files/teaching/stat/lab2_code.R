
# R basics ----------------------------------------------------------------

setwd("G:/我的雲端硬碟/0_R05 Graduate School")
getwd()
setwd("./106-1/106-1 Statistics/week 2/Week2 Lab")

# read data
data <- read.table("Student.csv", header = T, sep = ",")


# Data structure in R -----------------------------------------------------

# Numbers
x<- 1+5
x
rm(x)
?rm
class(x)
is.numeric(x)

# Characters
y<-"MYNTU"
class(y)
nchar(y)

# Date
z<-as.Date ("2015-02-01")
class(z)
NN<-as.numeric(z)
# since 1970,Jan, 1
NN


# Vector
X1<-c(1,2,3,4)
X1

# Matrix
X2<-matrix(1:12, nrow=2)
X2

#Array
X3<-array(1:12, dim=c(2,3,2))
X3

#Data Frame
x1 <- c(1,2,3,4)
y1 <- c("A", "B", "C", "D")
z1 <- c(12, 13, 14, 15)

DF1<-data.frame(x1,y1,z1)
DF1

#List
L1<-list(TheDataFrame=DF1,TheVECTOR=1:10)
L1

#Factor
status <- c("Poor", "Improved", "Excellent")
FA <- factor(status, level=c("Poor", "Improved", "Excellent"), order=T)
FA
rev(FA)

aa = factor(status, levels=c("Poor", "Improved", "Excellent"))
aa
rev(aa)

# Read data  --------------------------------------------------------------

getwd() # get working directory
setwd("G:/我的雲端硬碟/0_R05 Graduate School") # set working directory (d:/Labs/data/ )

data<- "./106-1/106-1 Statistics/week 2/Week2 Lab/Student.csv"
Stu_Table <-read.table(data, header=TRUE, sep=",")

head(Stu_Table)
class(Stu_Table)

save(Stu_Table, file = "Lab2.Rdata")
rm(Stu_Table) # remove
load("./Lab2.Rdata")

length(Stu_Table) # number of fields
nrow(Stu_Table) # number of records

data = Stu_Table

# Pie charts --------------------------------------------------------------

counts <- table(Stu_Table$ReligImp)
counts
slices <- counts
pct <- round(slices/sum(slices)*100)
pct

cnt = table(data$ReligImp)
cnt
names(cnt)


lbls <- rownames(counts)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(3), main="ReligImp")

pie(slices)

relig = data$ReligImp
pie(relig)


# Bar plot ----------------------------------------------------------------

barplot(counts, main="Students", col=rainbow(3),
        legend = rownames(counts),xlab="ReligImp", 
        args.legend = list(x = "topright", inset=c(-0.07,-0.4)))

barplot(counts, main = "title", col = rainbow(3), legend = rownames(counts), xlab = "x title", 
        args.legend = list(x = "topright", inset = c(-0.1, -0.4)))
barplot(relig)

# Cross tabs --------------------------------------------------------------

# Crosstabs
Sex<-Stu_Table$Sex
ReligImp<-Stu_Table$ReligImp

mytable <- xtabs(~ Sex + ReligImp)
mytable


tab = xtabs(PartyDays ~ Sex + ReligImp, data)
tab
t(tab)
sum(tab)

sum(data$PartyDays)

table(Sex, ReligImp)


# Stacked Bar Plot
barplot(mytable, col=c("red","blue"), 
        main="Comparisons of ReligImp",
        xlab="ReligImp", ylab="No. of Students",legend = rownames(mytable), 
        args.legend = list(x = "topright", inset=c(-0.07,-0.4)))

# Grouped Bar Plot
barplot(mytable, col=c("red","blue"), 
        main="Comparisons of ReligImp",
        xlab="ReligImp", ylab="No. of Students",legend = rownames(mytable),
        args.legend = list(x = "topright", inset=c(-0.07,-0.4)), 
        beside=TRUE)

mytable


# Histogram ---------------------------------------------------------------

hist(Stu_Table$GPA, breaks=10, col="blue", main="GPA Distribution", xlab="GPA")

d<-density(Stu_Table$GPA, na.rm=TRUE)
plot(d, col="red", main="GPA Distribution", xlab="GPA")



# Boxplot -----------------------------------------------------------------

GPA<- Stu_Table$GPA
Sex <-Stu_Table$Sex

boxplot(GPA~Sex, data=Stu_Table, col=(c("red","blue")), 
        main="Comparisons of GPA", xlab="Gender", ylab="GPA" )

boxplot(GPA~Sex, data = data)

# Stem-and-leaf plot ------------------------------------------------------

GPA<- Stu_Table$GPA
summary(GPA)

stem(GPA, scale = 2)
stem(GPA, scale = 2)

# Dot plot ----------------------------------------------------------------

GPA<- Stu_Table$GPA
dotchart(GPA, cex=1, lcolor = NULL)

# or
stripchart(GPA, method = "stack", offset = .5, at = .15, pch = 19,
           main = "Dotplot of GPA", xlab = "GPA")

hist(GPA, breaks = 50)

# Desciptive statistics ---------------------------------------------------

summary(GPA) # mean, median, Q25 and Q75, min, max

mean(GPA)
mean(GPA, na.rm = T)
var(GPA, na.rm = T) # sample variance
sd(GPA, na.rm = T) # sample SD
IQR(GPA, na.rm = T)


# How about var of population?





