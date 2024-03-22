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

library("car")



qqplot(df2$dys_treat, df2$rel_violence)


df$log_income<- log(df$income)
df2$log_income<- log(df2$income)


df2$edu_dummy<- as.integer(df2$educ %in% c(3, 4, 5, 6))
hist(df2$edu_dummy)

lm_df2<- lm(rel_violence~dys_treat+ideo+gender+edu_dummy+log_income+age+white, df2, na.action = na.omit)
summary(lm_df2)

#heteroskedasticity test
library(lmtest)
bpmodel<- rel_violence~ dys_treat+ideo+gender+edu_dummy+log_income+age+white
bptest(bpmodel, data=df2, studentize=FALSE)

qqPlot(df2$ideo)
qqPlot(df2$gender)
qqPlot(df2$educ)
qqPlot(df2$income)
qqPlot(df2$age)
qqPlot(df2$white)

library(sandwich)
library(sandwich)
new_model_robust<-coeftest(lm_df2, vcov = vcovHC(lm_df2, type="HC0"))
new_model_robust

df2$Hunger_dummy<- as.integer(df2$treatment==1)
df2$Divergent_dummy<- as.integer(df2$treatment==2)
df2$control_dummy<- as.integer(df2$treatment==0)
df2

lm_df3<- lm(rel_violence~Hunger_dummy+Divergent_dummy+ideo+gender+edu_dummy+log_income+age+white, df2, na.action = na.omit)
summary(lm_df3)


new_model_robust<-coeftest(lm_df3, vcov = vcovHC(lm_df2, type="HC0"))
new_model_robust
