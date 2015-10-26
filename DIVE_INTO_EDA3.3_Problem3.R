setwd("C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/")
getwd()
list.files()

## list of packages
library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)getwd()
library(gridExtra)

pseudo_facebook <-  read.delim("https://github.com/UmaSuresh/Data-Science/blob/master/Data/pseudo_facebook.tsv")
View(pseudo_facebook)
pf <- read.csv('pseudo_facebook.tsv',sep = '\t')
names(pf)
# histograms for User's BDay
qplot(x=dob_day, data = pf)+
  scale_x_discrete(breaks = 1:31)
qplot(x=dob_day,data=pf)+scale_x_discrete(1:31)+facet_wrap(~dob_month,ncol=3)
ggplot(data=pf,aes(x=dob_day))+geom_histogram()+scale_x_discrete(breaks=1:31)+facet_wrap(~dob_month)
ggplot(data=pf,aes(x=dob_day))+geom_histogram()+scale_x_discrete(breaks=1:31)
ggplot(data=pf,aes(x=dob_day))+geom_histogram()+scale_x_discrete(breaks=1:31)+facet_wrap(~dob_month,ncol=2)
ggplot(data=pf,aes(x=dob_day),xlab= 'day of Birth',ylab='number of count')+geom_histogram()+scale_x_discrete(breaks=1:31)+facet_wrap(~dob_month,ncol=2)
ggplot(data=pf,aes(x=dob_day))+geom_histogram()+scale_x_discrete(breaks=1:31)+facet_wrap(~dob_month,ncol=4)
ggplot(data=pf,aes(x=friend_count))+geom_histogram()+scale_x_discrete(breaks=1:31)+facet_wrap(~dob_month,ncol=4)
qplot(x=friend_count ,data=pf)+scale_x_continuous(limits = c(0,1000))
## Adjusting the bin width Solution
qplot(x=friend_count ,data=pf , xlim = c(0,1000),
      xlab = seq(0,1000,50), binwidth=25 ) +
  facet_wrap(~gender)
##Statistics 'by' Gender
table(pf$gender)
by(pf$friend_count, pf$gender, summary)
## Tenure
qplot(x =tenure , data = pf)
qplot(x =tenure , data = pf, 
      color = I('black'), fill = I('#099DD9'))
qplot(x =tenure , data = pf, binwidth = 30,
      color = I('black'), fill = I('#099DD9'))
## Labeling Plots
qplot(x =tenure/365 , data = pf, 
      xlab = 'Number of years using Facebook',
      ylab =  'Number of users in sample' ,
     color = I('black'), fill = I('#099DD9'))+
  scale_x_continuous(breaks = seq(1, 7, 1), lim = c(0,7))
qplot(x =age , data = pf, binwidth = 1,
           color = I('black'), fill = I('#099DD9'))
qplot(x =age , data = pf, binwidth = 1,
      color = I('black'), fill = I('#099DD9'))+
  scale_x_discrete(breaks = seq(0, 115, 5))
summary(pf$age)
# Transforming Data
qplot(x = friend_count , data = pf)
summary(pf$friend_count)
summary(log10(pf$friend_count +1))
summary(sqrt(pf$friend_count ))

p1 <-qplot(x = friend_count , data = pf)
p2 <-qplot(x = log10(friend_count + 1) , data = pf)
p3 <-qplot(x = sqrt(friend_count + 1) , data = pf)
grid.arrange(p1 ,p2 ,p3 , ncol=1)
## Use Scales ggplot
p1 <-ggplot(aes(x = friend_count) , data = pf) + geom_histogram()
p2 <-p1 + scale_x_log10()
p3 <-p1 + scale_x_sqrt()

## Add a scale Layer
logScale <- qplot(x = log10(friend_count), data = pf)
countScale <-ggplot(aes(x=friend_count), data = pf)+
  geom_histogram()+
  scale_x_log10()
grid.arrange(logScale , countScale , ncol=2)

## Frequency Polygons (before we had histograms)
qplot(x= friend_count ,data = subset(pf , !is.na(gender)),
      binwidth = 10) +
  scale_x_continuous(lim = c(0,1000),breaks = seq(0,1000,50))

## Likes on the web

qplot(x = www_likes, data = subset(pf,!is.na(gender)),
      geom = 'freqpoly' , color = gender) +
  scale_x_continuous() + scale_x_log10()
qplot(x = friend_count, data = subset(pf,!is.na(gender)),
      stat = "bin" , geom = 'histogram' , horiz = TRUE , color = gender) +
  scale_x_continuous() + scale_x_log10()