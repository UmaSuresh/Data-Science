setwd("C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/Capstone Project")
getwd()
list.files()

## list of packages
library(ggplot2)
library(dplyr)
library(reshape2)

PVP2015 <-  read.csv("C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/Data/PVP1.csv")
View(PVP2015)

## fit a line using R

PVP <- data.frame(x = PVP2015$J0.08 , y = PVP2015$YS)
attach(PVP)
plot(x,y) 
abline(lm(y ~ x))
## equation of line
coef(lm(y~x))
