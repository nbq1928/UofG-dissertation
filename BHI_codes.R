BHI<- read.table('/Users/chinonsondukauba/Desktop/UofG/CSV BHI manual', sep=",", header = 1)
head(data2, 5)

# Define the file path
file_path <- '/Users/chinonsondukauba/Desktop/UofG/CSV BHI manual'

# Read all lines from the file
lines <- readLines(file_path)

# Extract the third line for column names
col_names <- unlist(strsplit(lines[3], split=","))

# Combine the third line with the rest of the data
data_lines <- lines[-(1:2)]

# Create a temporary file to store the adjusted content
temp_file <- tempfile()
writeLines(data_lines, temp_file)

# Read the adjusted data into a data frame
IHB <- read.table(temp_file, sep=",", header=TRUE, col.names=col_names)

# Clean up the temporary file
unlink(temp_file)

# Display the first few rows of the data frame
head(IHB)

#having a look at the data before use

summary(IHB)
rm(BHI)
BHI<-IHB
any(is.na(BHI))
str(BHI)

#summary of relevant packages loaded

library(foreign)
library(haven)
library(tidyr)
library (tidyverse)
library(readxl)
library (ggplot2)
library(dplyr)
library (mosaic)
library (descr)
library(knitr)
library(kableExtra)
library(truncnorm)
library(readxl)
library(broom)
library(stats)
library(MASS)
library (epitools)
library(coin)
library(DescTools)
library (AER)
library(survival)

#visualising data to know distributions and ascertain if parametric or non-parametric tests are bettere

print (boxplot(BHI$X.BHI_score., main="boxplot of BHI", ylab="BHI"))
summary(BHI$X.BHI_score.)
num.cor<- sapply(BHI, is.numeric)
cor.data<- cor(BHI[,num.cor])
print(corrplot(cor.data, method="color"))
print(corrplot(cor.data, method="color"))
search ()

summary(BHI$X.age.)
boxplot(BHI$X.age., main= "boxplot of age", ylab= "age")

summary(BHI$X.BHI_score.)

boxplot(BHI$X.BHI_score., main= "boxplot of BHI score", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.sex, main= "boxplot of BHI score by sex", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.smoking, main= "boxplot of BHI score by smoking", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.reg.drinking, main= "boxplot of BHI score by reg drinking", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.medication, main= "boxplot of BHI score by medication use", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.HTN, main= "boxplot of BHI score by HTN", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.DM, main= "boxplot of BHI score by DM", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.hypothyroidism, main= "boxplot of BHI score by hypothyroidism", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.AF, main= "boxplot of BHI score by AF", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.anaemia, main= "boxplot of BHI score by anaemia", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.OSA, main= "boxplot of BHI score by OSA", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.CHD, main= "boxplot of BHI score by CHD", ylab= "BHI score")
boxplot(BHI$X.BHI_score.~f.PVD, main= "boxplot of BHI score by PVD", ylab= "BHI score")

sd((BHI$X.age.))
any.na((BHI$X.age.))
print(frequency(f.smoking))
freq(f.sex, na.rm=TRUE, plot=FALSE)
sd(BHI$X.BHI_score.)


# Creating a factor variable: some examples

f.CHD<- factor(IHB$X.dx_CHD.)
f.alcstopped<- factor(IHB$X.stopped_alcohol.)
f.alcuse<- factor(IHB$X.alcohol_intake.)
f.sex<- factor(IHB$X.sex.)
f.smoking <- factor(IHB$X.smoking_status.)
f.reg.drinking <- factor(IHB$X.regular_drinking.)
f.medication <- factor(IHB$X.medication.)
f.HTN <- factor(IHB$X.dx_HI_BP.)
f.DM <- factor(IHB$X.dx_T2D.)
f.hypothyroidism <- factor(IHB$X.Hypothyroidism.)
f.AF <- factor(IHB$X.Atrial.)
f.anaemia <-factor(IHB$X.Anemia.)
f.OSA <- factor(IHB$X.OSA.)
f.CHD <- factor(IHB$X.dx_CHD.)
f.PVD <- factor(IHB$X.PVD.)

BHI<-IHB

# BHI based on different categories after conversion to factor variables

favstats(BHI$X.BHI_score.)
favstats(BHI$X.BHI_score.~f.sex)
favstats(BHI$X.BHI_score.~f.smoking)
favstats(BHI$X.BHI_score.~f.reg.drinking)
favstats(BHI$X.BHI_score.~f.medication)
favstats(BHI$X.BHI_score.~f.HTN)
favstats(BHI$X.BHI_score.~f.DM)
favstats(BHI$X.BHI_score.~f.hypothyroidism)
favstats(BHI$X.BHI_score.~f.AF)
favstats(BHI$X.BHI_score.~f.anaemia)
favstats(BHI$X.BHI_score.~f.OSA)
favstats(IHB$X.BHI_score.~f.CHD)
favstats(BHI$X.BHI_score.~f.PVD)
favstats(BHI$X.BHI_score.~f.alc.use)
favstats(BHI$X.BHI_score.~f.alc.stop)

# anova test 
summary(aov(IHB$X.BHI_score. ~ f.alcuse))

summary(aov(IHB$X.BHI_score.~f.alcstopped, data = IHB))
pairwise_results <- pairwise.t.test(IHB$X.BHI_score., f.alcuse, p.adjust.method = "bonferroni")
summary(pairwise_results)

#linear regression modelling

model4<- lm(BHI$X.BHI_score.~f.sex + BHI$X.age.)
summary(model4)

model3<-lm(BHI$X.BHI_score.~f.sex + f.CHD + f.HTN + f.DM + f.AF + f.anaemia + f.hypothyroidism + f.OSA + f.PVD + BHI$X.BMI.)

summary(model3)

model1<-lm(BHI$X.BHI_score.~f.sex + f.smoking + f.reg.drinking + f.medication + f.CHD + f.HTN + f.DM + f.AF + f.anaemia + f.hypothyroidism + f.OSA + f.PVD + BHI$X.BMI. + BHI$X.age., data= BHI)

summary(model1)

model<-lm(BHI$X.BHI_score.~f.sex + f.smoking + f.reg.drinking + f.medication + f.CHD + f.HTN + f.DM + f.AF + f.anaemia + f.hypothyroidism + f.OSA + f.PVD + BHI$X.BMI.)

summary(model)
