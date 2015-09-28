setwd("C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/")
getwd()
list.files()
## list of packages
library(ggplot2)
library(ggplot2)
library(dplyr)
require(reshape2)
library(reshape2)
install.packages("data.table")
library(data.table)

pseudo_facebook <-  read.delim("C:/Users/usureshkumar/Documents/R-Software/Dive into EDA -Project files/Data-Science/pseudo_facebook.tsv")
View(pseudo_facebook)
pf <- read.csv('pseudo_facebook.tsv',sep = '\t')
names(pf)
## Third Qualitative variable
ggplot(aes(x = gender, y = age),
       data = subset(pf, !is.na(gender))) + geom_boxplot() +
  stat_summary(fun.y =mean , geom = 'point' , shape = 4)
ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) +
  geom_line(aes(color = gender),  stat = 'summary' , fun.y =median )

## chain function together %.%
pf.fc_by_age_gender <- pf %>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n= n()) %>%
  ungroup() %>%
  arrange(age)
head(pf.fc_by_age_gender)
## Plotting Conditional Summaries

pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender,
                                  age ~ gender ,
                                  value.var = 'median_friend_count')
head(pf.fc_by_age_gender.wide)
## Ratio Plot Solution

ggplot(aes(x = age, y = female / male),
        data = pf.fc_by_age_gender.wide) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = 0.3 , linetype = 2)

## Cut a variable

pf$year_joined <- floor(2014 - pf$tenure/365)
summary(pf$year_joined)
table(pf$year_joined)
?cut
pf$year_joined.bucket <- cut(pf$year_joined,
                            c(2004,2009,2011,2012,2014))
 ## Plotting it All Together
table(pf$year_joined.bucket , useNA = 'ifany')
ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket),  stat = 'summary' , fun.y =median )
ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) +
  geom_bar(aes(color = gender),  stat = 'summary' , fun.y =median )

pf$ages.bucket <- cut(pf$age, breaks = c(18,29,50,64,100))
table(pf$ages.bucket ,useNA = 'ifany')

ggplot(aes(x = age, y = friend_count),
       data =subset(pf, !is.na(ages.bucket))) +
  geom_histogram(aes(color = ages.bucket),  stat = 'summary' , fun.y =median)
ggplot(aes(x = ages.bucket , y = friend_count),
       data =subset(pf, !is.na(ages.bucket))) +
  geom_histogram(aes(color = ages.bucket),  stat = 'summary' ,fun.y =median)

ggplot(aes(x = ages.bucket , y = friend_count),
       data =subset(pf, !is.na(pf$ages.bucket))) +
  geom_histogram(aes(color = ages.bucket),  stat = 'summary' ,fill = 'darkblue' ,  fun.y =median)

summary(pf)
summary(pf$ages.bucket)
table(pf$ages.bucket ,useNA = 'ifany')
qplot(x=friend_count ,data=pf)+scale_x_continuous(limits = c(0,1000))
## Adjusting the bin width Solution
qplot(x=friend_count ,data=pf , xlim = c(0,1000),
      xlab = seq(0,1000,50), binwidth=5 ) +
  facet_wrap(~ages.bucket)



