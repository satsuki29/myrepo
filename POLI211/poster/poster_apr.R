library(foreign)
library(readstata13)
library(dplyr)
library(ggplot2)
#install.packages("describedata")
library(describedata)
library(haven)
#library(LEAP)
library(leaps)
library(factoextra)
library(stargazer)
library(expss)
library(coefplot)
library(car)
library(datasets)
library(reshape2)
library(margins)
library(mfx)
library(expss)
library(stargazer)
library(arm)
library(readr)


df <- read_csv("249 データ(CSV).csv")
View(df)

names(df)
head(df)
glimpse(df)
#################################################
scat1<- ggplot(data =df,
               mapping = aes(x= aircraft ,y= total_coverage))
scat1<- scat1 + geom_point()

scat1<- scat1 +
  labs(x= "飛行機の数", y= "報道数", 
       title= "飛行機の数と報道")
print(scat1)

#################################################
scat2<- ggplot(data =df,
               mapping = aes(x= "military/ else(情報収集機)" ,y= total_coverage))
scat2<- scat2 + geom_point()

scat2<- scat2 +
  labs(x= "戦闘機", y= "報道数", 
       title= "飛行機の種類と報道")
print(scat2)

#################################################

scat3<- ggplot(data =df,
               mapping = aes(x= year ,y= total_coverage))
scat3<- scat3 + geom_point()

scat3<- scat3 +
  labs(x= "年", y= "報道数", 
       title= "年と報道")
print(scat3)
#################################################

