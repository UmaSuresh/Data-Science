
## My project examines the final sold prices of homes versus the "Zestimate" sales prices for Bay Area ,California
## area that were sold on Febrauary ,2015. The data shown below was extracted from www.zillow.com. 


##setwd("C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/Capstone Project")
##setwd("https://github.com/UmaSuresh/Data-Science/tree/master/Capstone%20Project")
##getwd()
##list.files()
## list of packages
install.packages("stringi") 
install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")
install.packages("reshape2")
install.packages("data.table")
install.packages("sciplot") 
install.packages("rattle")
install.packages("data.table")
install.packages("sm")
install.packages("RCurl")

library(sm)
library(sciplot)
library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)
library(plyr)
library(SparseM)
library(RCurl)


# Reading Data set.

#zzdat <- read.csv('https://github.com/UmaSuresh/Data-Science/blob/master/CapstoneProject/zillow.csv')

zdat <- read.csv('C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/Data/Zillow.csv')
names(zdat)

##fit a line between Sold price Vs Zestimates

zdat1<-data.frame(x=zdat$zesFeb, y=zdat$soldFeb)
attach(zdat1)
plot(x,y,xlab="Zestimate(inDollars)" , ylab="Actual Sold Price(in Dollars)" , main="Zestimates Vs Actual Sold Price ,California Metro, Feb,2015") 
m <- abline(lm(y ~ x))
fit <- lm( y~x)

## summary of overall data Coefficients.
summary(zdat)
coef(zdat)
summary(fit)
zillowcoef <- coef(fit)
abline(coef(fit)[1:2])
coef(fit)
cordat <- cor(zdat$zesFeb ,zdat$soldFeb)

str(zdat)


##Simple linear regression results of the price sold vs the Zestimate 
## shows a linear correlation coefficient(r) = 0.934520.
## Looks like a Large positive linear correlation .
## all Data points are close to the trend line except few outliers.

##The coefficient of determination (R2) for the Zestimate price is 0.8526121,
## so 87.3% of the variability of selling price can be explained by
## the linear relation between the Zestimate price and the final selling price; 

##  separating  by Metro SFO,Los Angeles, San Jose.

unique(zdat$Metro)
SFO <- subset(zdat , Metro == 'San Francisco')
SJC <- subset(zdat , Metro== 'San Jose')
LAS <- subset(zdat , Metro== 'Los Angeles-Long Beach-Anaheim, CA')
nrow(SFO)
nrow(SJC)
nrow(LAS)
se(SFO$zesFeb)
se(LAS$zesFeb)
se(SJC$zesFeb)

mean(SFO$zesFeb)
mean(SJC$zesFeb)
mean(LAS$zesFeb)
mean(SFO$soldFeb)
mean(SJC$soldFeb)
mean(LAS$soldFeb)

median(SJC$zesFeb)
median(SFO$zesFeb)
median(LAS$zesFeb)
median(SFO$soldFeb)
median(SJC$soldFeb)
median(LAS$soldFeb)

## Finding Correlation for SFO, San Hose and Los Angeles

corSFO <- cor(SFO$zesFeb , SFO$soldFeb)
corLAS <- cor(LAS$zesFeb , LAS$soldFeb)
corSJC<- cor (SJC$zesFeb , SJC$soldFeb)

## Error Coef - R squared
errSFO <- corSFO*corSFO
errSJC <- corSJC*corSJC
errLAS <- corLAS*corLAS
errdat <- cordat * cordat


## Finding error value or R2

se(SFO$soldFeb,SFO$zesFeb)
sd(zdat$zesFeb)
se(zdat$zesFeb)
se(zdat$soldFeb)

## Plotting against only  LA Data

plot(x= LAS$zesFeb , y = LAS$soldFeb ,xlab="Zestimate(inDollars)" , ylab="Actual Sold Price(in Dollars)" , 
      main="Zestimates  Vs Actual Sold Price on Feb,2015-Los Angeles")
m1 <- abline(lm(y ~ x))
fitLA <- lm( y~x)
summary(fitLA)
coef(fitLA)
cor(x,y)

## SFO Area

plot(x= SFO$zesFeb , y = SFO$soldFeb ,xlab="Zestimate(inDollars)" , ylab="Actual Sold Price(in Dollars)" , 
     main="Zestimates  Vs Actual Sold Price on Febr,2015-San Francisco")
m2 <- abline(lm(y ~ x))
fitSFO <- lm( y~x)

 ## San Jose Data

plot(x= SJC$zesFeb , y = SJC$soldFeb ,xlab="Zestimate(inDollars)" , ylab="Actual Sold Price(in Dollars)" , 
     main="Zestimates  Vs Actual Sold Price on February ,2015 - San Jose")
m3 <- abline(lm(y ~ x))
fitSJC <- lm( y~x)



## My next step would be find the residuals and going to see any linear relation ship. 

## finding residuals

 a <- zdat$zesFeb
 b <- zdat$soldFeb
zdat$residuals <- a-b
r <- cor(a,b)
rsquare <- r * r
r*r

## zestimate Vs Residuals

plot(x = zdat$zesFeb ,y = zdat$residuals ,xlab="Zestimate(inDollars)" , ylab="Residuals" , main="Zestimates  Vs Residuals") 
fit <- lm( y~x)

## The scatter plot of Zestimate versus Residuals shows no significance pattern. 
## It looks like percentage of error value determines the star rating .
## Los Angeles shows 4 star value where the error percentage falss under 6 %.
## San francisco median error percentage falls 8.5% .It fals between 8 and 10.

## Showing boxplot rating estimates

boxplot(SFO$zesFeb, SFO$soldFeb, xlab="Zestimate" , ylab = "sold Price" ,main="Boxplot of Zestimate and Actual Sold Price in SFO" ) 
boxplot(LAS$zesFeb, LAS$soldFeb, xlab="Zestimate" , ylab = "sold Price" ,main="Boxplot of Zestimate and Actual Sold Price in LAS")
boxplot(SJC$zesFeb, SJC$soldFeb, xlab="Zestimate" , ylab = "sold Price" ,main="Boxplot of Zestimate and Actual Sold Price in San Jose") 


ggplot(aes(x = zdat$zesFeb, y = zdat$soldFeb),
       data = subset(zdat, !is.na(zdat$Metro))) +
  geom_line(aes(color = Metro),  stat = 'summary' , fun.y =median )


## Plotting Metro and County Names.

ggplot(zdat, alpha = 0.5,
       aes(x = zdat$zesFeb, group = zdat$Metro, fill = zdat$Metro))+
  stat_bin(aes(y=..density..), position='dodge')

ggplot(aes(x = zesFeb, y = soldFeb),
       data = subset(zdat, !is.na(CountyName))) +
  geom_bar(aes(color = CountyName),  stat = 'summary' , fun.y =median )

ggplot(aes(x = zdat$zesFeb, y = zdat$soldFeb , xlab="Zestimate(inDollars)" , ylab="Actual Sold Price(in Dollars)" , main="Zestimates  Vs Actual Sold Price on February ,2015"),
       data = subset(zdat, !is.na(zdat$Metro))) +
  xlab("Zestimate (in Dollars)") +
  ylab("Actual Sold Price (in Dollars)")+
  geom_bar(aes(color = zdat$Metro ) ,  stat = 'summary' , fun.y = median )

## density plot for the Zestimates
## Kernal density plots are usually more effective way to view the distribution of a variable.

d <- density(zdat$zesFeb)
plot(d , xlab = "Zestimates" , main = " Density of Zestimates Values for the month Feb,2015" )+
  polygon(d, col="red", border="blue")

metro.f <- factor(zdat$Metro , levels = c("San Francisco" , "San Jose" , "Los Angeles-Long Beach-Anaheim, CA"))
sm.density.compare(zdat$zesFeb , metro.f , xlab = "Zestimates Values(in Dollars)" )+
  title(main = " Density Plots for Metro Area for Feb , 2015")
   
# add legend via mouse click 

require(sm)
colfill<- c(2:(2+length(levels(metro.f)))) 
legend("topright", levels(metro.f), fill=colfill)