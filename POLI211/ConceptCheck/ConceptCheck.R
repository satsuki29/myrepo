library(foreign)
library(readstata13)
library(dplyr)
library(ggplot2)
#install.packages("describedata")
library(describedata)
library(haven)

df <- read_dta("Dystop_abridged.dta")
View(Dystop_abridged)

df2<- df[, c(1,2,3,4,6, 9, 11, 12, 13)]
show(df2)
sapply(df2, attr,"label")

plot(df2$dys_treat, df2$rel_violence)

plot(df2$ideo, df2$rel_violence)
plot(df2$gender, df2$rel_violence)
plot(df2$educ, df2$rel_violence)
describedata::gladder(df2$educ)
plot(df2$income, df2$rel_violence)
describedata::gladder(df2$income) ##log
plot(df2$age, df2$rel_violence)
describedata::gladder(df2$age)
plot(df2$white, df2$rel_violence)

df$log_income<- log(df$income)
df2$log_income<- log(df2$income)


df2$edu_dummy<- as.integer(df2$educ %in% c(3, 4, 5, 6))
hist(df2$edu_dummy)

