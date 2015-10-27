
## My project examines the final sold prices of homes versus the "Zestimate" sales prices for Bay Area ,California
## area that were sold on Febrauary ,2015. The data shown below was extracted from www.zillow.com. 

setwd("C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/Capstone Project")
getwd()
list.files()
install.packages("stringi") ## Missing package for my R Studio. Sujit , I think you might not need this.

## list of packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")
install.packages("data.table")

library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)

# Reading Data set.
zdat <- read.csv('C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/Data/Zillow.csv')
names(zdat)

qplot(x=zesFeb , data= zdat)
qplot(x=zesFeb, y=soldFeb , data = zdat)

##fit a line between Sold price Vs Zestimates

zdat1<-data.frame(x=zesFeb, y=soldFeb)
attach(zdat1)
plot(x,y,xlab="Zestimate(inDollars)" , ylab="Actual Sold Price(in Dollars)" , main="Zestimates  Vs Actual Selling Price") 
m1 <- abline(lm(y ~ x))
fit <- lm( y~x)

summary(fit)
zillowcoef <- coef(fit)
abline(coef(fit)[1:2])
coef(fit)


##Simple linear regression results of the price sold vs the Zestimate 
## shows a linear correlation coefficient(r) = 0.9233699.
## Looks like a Large positive linear correlation .
## all Data points are close to the trend line except few outliers.

##The coefficient of determination (R2) for the Zestimate price is 0.8526121,
## so 85.3% of the variability of selling price can be explained by
## the linear relation between the Zestimate price and the final selling price; 

## My next step would be find the residuals and going to see any linear relation ship. 

## finding residuals

zdat$residuals <- x-y
r <- cor(x,y)
rsquare <- r * r
r*r

## zestimate Vs Residuals

plot(x = zdat$zesFeb ,y = zdat$residuals ,xlab="Zestimate(inDollars)" , ylab="Residuals" , main="Zestimates  Vs Residuals") 
m2 <- abline(lm(y ~ x))
fit <- lm( y~x)

## The scatter plot of Zestimate versus Residuals shows no significance pattern. 
## It looks like Outliers are the important factors affecting the star rating.
## As I told you by the phone major affecting areas are Cupertino ,San Francisco (Metro) and 
##San jose are those three major outliers affecting the star rating.
## 
## My next step would be removing those outliers and see.
## I will do it tomorrow and email it to you.


 ## below are my tomorrow's plan to justify 
unique(zdat$City)
SFO <- subset(zdat , City== 'San Francisco')
SJC <- subset(zdat , City== 'San Jose')
nrow(SFO)
nrow(SJC)
mean(SFO$zesFeb)
mean(SJC$zesFeb)
median(SJC$soldFeb)
median(SFO$soldFeb)


