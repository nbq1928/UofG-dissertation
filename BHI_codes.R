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

sd((BHI$X.age.))
any.na((BHI$X.age.))
print(frequency(f.smoking))
freq(f.sex, na.rm=TRUE, plot=FALSE)
sd(BHI$X.BHI_score.)


# creating a factor variable: some examples

f.CHD<- factor(IHB$X.dx_CHD.)
f.alcstopped<- factor(IHB$X.stopped_alcohol.)
f.alcuse<- factor(IHB$X.alcohol_intake.)

# BHI based on different catergoris after conversion to factor variables

favstats(BHI$X.BHI_score.)
favstats(BHI$X.BHI_score.~f.sex)
favstats(BHI$X.BHI_score.~f.smoking)
favstats(BHI$X.BHI_score.~f.alc.use)
favstats(BHI$X.BHI_score.~f.alc.stop)
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
favstats(BHI$X.BHI_score.~f.CHF)
favstats(BHI$X.BHI_score.~f.kidneyfailure)
favstats(BHI$X.BHI_score.~f.kidneynephropathy)

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



