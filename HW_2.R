library(readr)
Lab1<-read.csv("/Users/one/Desktop/Principles-of-Analy-500/Assignments/Lab1-Part2/Lab1.csv", header=TRUE)
Lab1
View(Lab1)

myframe<-as.data.frame(Lab1[c('typing_speed','course')])
View(myframe)
colnames(Lab1)<- c("typing_speed","course" )

# 1) What are the mean, standard deviation, and standard error for each group?
library(plyr)
ddply(Lab1, .(course), summarize,  typing_speed=mean(typing_speed)) #mean of each group
ddply(Lab1, .(course), summarize,  typing_speed=sd(typing_speed)) #sd of each group

install.packages("sciplot")
library(sciplot)
ddply(Lab1, .(course), summarize,  typing_speed=se(typing_speed)) #standard error of each group

# 2) Which group appears to have a better model fit using the mean as our model?
ddply(Lab1, .(course), summarize,  typing_speed=var(typing_speed))
# Ans: TaDa typing as the number observations for both groups have same and TaDa typing has a higher mean of 52.62.

# 3) What are the degrees of freedom (df) for each group?
table(Lab1$course) #number of rows per group
df<- table(Lab1$course)-1
df
#Ans: Total Sonic typing = 50, df= 50-1=49
#Ane: Total TaDa typing = 50, df= 50-1=49

# 4) What is the 95% confidence interval for each group mean?
#Sonic typing
m1<-49.34 # mean of sonic typing
sd1<- 19.62985  # sd of sonic typing
n1<-50  # number of observations for Sonic typing
# Confidence interval = (average) +- z*se
error1 <- qnorm(0.975)*sd1/sqrt(n1)
left1<-m1-error1
left1
right1<-m1+error1
right1 
#The true mean is within 43.89898 and 54.78102

m2<-52.62  # mean of TaDa typing
sd2<-12.68116  # sd of TaDa ty
n2<-50  # number of observations for TaDa typing
error2 <- qnorm(0.975)*sd2/sqrt(n2)
left2<-m2-error2
left2
right2<-m2+error2
right2
#The true mean is within 49.10503 and 56.13497.

# 5) Calculate the effect size between the two programs using the MOTE library.
install.packages("githubinstall")
library(githubinstall)
gh_install_packages("MOTE")
library(MOTE)
sd<-sd(Lab1$typing_speed)
sd<-sqrt((sd1^2+sd2^2)/2) #spooled standard deviation
dcohen<-(m2-m1)/sd  # Calculating conhens d to find the effect size
dcohen


#6) Calculate the number of participants you would need if you wanted to repeat this study. 
# To calcultae the number of participants I use the cohen's d calculated above, a significance level of 0.05 and the amount of power I want for my study to have.
install.packages("pwr") #Installing the power package
library(pwr)
pwr.t.test(n = NULL, d = 0.1984887 , sig.level = 0.05, power = 0.8, alternative = "two.sided", type = "two.sample")