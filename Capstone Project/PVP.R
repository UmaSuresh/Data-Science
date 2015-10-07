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

PVP <- data.frame(x = PVP2015$YS , y = PVP2015$J0.08)
attach(PVP)
plot(x,y) 
abline(lm(y ~ x))
## equation of line
p <- ggplot(data = PVP2015, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()

lm_eqn <- function(PVP){
  m <- lm(y ~ x, PVP);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(PVP), parse = TRUE)