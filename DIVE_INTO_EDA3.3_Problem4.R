load("~/R-Software/.RData")
getwd()
setwd('C:/Users/usureshkumar/Documents/R-Software')
list.files()
install.packages(('ggplot2'))
library(ggplot2)
summary(pf)
summary(dat)
summary(uma)
uma <- read.csc(file='pseudo_facebook.tsv', sep='\t')
uma <- read.csv(file='pseudo_facebook.tsv', sep='\t')
summary(uma)
qplot(x=age,y=friend_count,data=pf)
ggplot(data=pf,x=age,y=friend_count)
ggplot(data=uma,aes(x=age,y=friend_count))
qplot(age.friend_count,data=uma)
qplot(x=age, y=friend_count,data = uma)       
ggplot(aes(x=age,y=friend_count),data = uma)+geom_point()
summary(uma$age)
ggplot(aes(x=age,y=friend_count),data = uma)+geom_point()+xlim(13,90)
ggplot(aes(x=age,y=friend_count),data = uma)+
geom_point(alpha=1/20)+xlim(13,90)
+ coord_trans(y ='sqrt')
ggplot(aes(x=age,y=friend_count),data = uma)+geom_point()+xlim(13,90)
ggplot(aes(x=age,y=friend_count),data = uma)+geom_point()+xlim(13,90)
+ coord_trans(y = 'sqrt')
?sqrt
require(stats) # for spline
require(graphics)
ggplot(aes(x=age,y=friend_count),data = uma)+geom_point(alpha = 1/20)+
xlim(13,90) +
coord_trans(y = 'sqrt')
ggplot(aes(x=age,y=friend_count),data = uma)+geom_point(alpha = 1/20)+
  xlim(13,90) +
  coord_trans(y = 'sqrt')
ggplot(aes(x=age,y=friendships_initiated),data = uma)+geom_point()+
  xlim(13,90) 
ggplot(aes(x=age,y=friendships_initiated),data = uma)+geom_point(alpha = 1/20)+
  xlim(13,90) +
  coord_trans(y = 'sqrt') 
ggplot(aes(x=age,y=friendships_initiated),data = uma)+geom_point(alpha = 1/10 ,position = position_jitter(h=0))+
  xlim(13,90) +
  coord_trans(y = 'sqrt') 

ggplot(aes(x=age,y=friendships_initiated),data = uma)+geom_point(alpha = 1/10 ,position = position_jitter(h=0))+
  xlim(13,90) +
  coord_trans(y = 'sqrt') 
require(dplyr)
age_groups <- group_by(uma , age)
uma.fc_byage <- summarise(age_groups , friend_count_mean =mean(friend_count), friend_count_median=median(friend_count),n=n())
head(uma.fc_byage)
uma %>% group_by(age)%>%
  +   summarise(friend_count_mean =mean(friend_count),
                +             friend_count_median=median(friend_count)
ggplot(aes(x=age,y=friend_count),data = uma)+
  xlim(13,90)+
  geom_point(alpha = .05 ,
             position = position_jitter(h=0),
             color = 'orange') +
  coord_trans(y = 'sqrt') + 
geom_line(stat='summary', fun.y= mean)+
  geom_line(stat='summary', fun.y= quantile,probs=.1,linetype=2,color='blue')+
  geom_line(stat='summary', fun.y= quantile,probs=.5,linetype=2,color='blue')+
  geom_line(stat='summary', fun.y= quantile,probs=.9,linetype=2,color='blue')
?cor.test
        
cor.test(uma$age,uma$friend_count ,method = 'pearson')
with(subset(uma, age <= 70),cor.test(age, friend_count,method='pearson'))
cor.test(uma$age,uma$friend_count ,method = 'pearson')
with(subset(uma, age <= 70),cor.test(age, friend_count,method='kendall'))
ggplot(aes(x=www_likes_received,y=likes_received),data = uma)+geom_point(alpha = 1/5) +
  xlim(0,quantile(uma$www_likes_received,0.95)) +
  ylim=(0,quantile(uma$likes_received,0.95)) +
  geom_smooth((method= 'lm' ,color='red'))

ggplot(aes(x = www_likes_received,y = likes_received),data = uma) +
     geom_point() +
      xlim(0, quantile(uma$www_likes_received,0.95)) +
      ylim(0, quantile(uma$likes_received,0.95)) +
      geom_smooth(method= 'lm',color='red')
      
cor.test(uma$www_likes_received,uma$likes_received ,method='pearson')

require('alr3')
data("Mitchell")
?Mitchell
cor.test(Mitchell$Month,Mitchell$Temp,method='pearson')
## Smoothing Conditional Means
ggplot(aes(x=age ,y= friend_count_mean),data = uma.fc_byage)+
geom_line()


