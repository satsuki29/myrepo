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
cor(df)
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
df_new <- df[,-which(names(df) == "country_RU")]
df_new1 <- rename(df_new, country_no = "country number",
                  military = " military/ else(情報収集機)",
                  invasion = "invasion（領海侵犯報告）")
##################################################
##BLUE check
lm_viftest<- lm(total_coverage ~ aircraft+year+country_no+country_CN+military+Year_2022+invasion+Yomiuri+Nikkei,data=df_new1)
vif(lm_viftest)
#invasion、Yomiuri、Nikkei が、モデルの精度に悪影響を与えている可能性があります。
lm_viftest2<- lm(total_coverage ~ aircraft+country_no+country_CN+Year_2022,data=df_new1)
#invasionとNikkeiを抜く
vif(lm_viftest2)

#check heterosked~~(不均一分散)
library("tidyverse")

res_plt<- tibble(res = lm_viftest2$residuals,
                 fitted = lm_viftest2$ fitted.values) %>%
  ggplot(aes( x= fitted, y= res))+
  geom_point() +
  geom_hline(yintercept = 0)+
  labs ( x= " fitted values", y= "residuals")
print (res_plt)

# qqplot
res<- lm_viftest2$ residuals
df_res<- tibble(z_res =(res - mean(res))/sd(res))

qqplot<- ggplot(df_res,aes(sample = z_res))+
  geom_abline(intercept = 0, slope=1 ,linetype="dashed")+
  geom_qq()+
  labs(x= "標準正規分布", y="標準化した残差の分布")
print(qqplot)

##########
##run a regression
summary(lm_viftest2)
reg.or<-exp(coefficients(lm_viftest2))
table_reg1<-stargazer(lm_viftest2, type="latex", coef=list(reg.or), p.auto=FALSE, out="logitor.tex")

#####################################
## regression including invasion data

lm_3<- lm(total_coverage ~ aircraft+country_no+country_CN+Year_2022+invasion,data=df_new1)
vif(lm_3)
#  Independence of error
res_plt<- tibble(res = lm_3$residuals,
                 fitted = lm_3$ fitted.values) %>%
  ggplot(aes( x= fitted, y= res))+
  geom_point() +
  geom_hline(yintercept = 0)+
  labs ( x= " fitted values", y= "residuals")
print (res_plt)
# check heterosked~~(不均一分散)
bptest(lm_3)
# qqplot
res<- lm_3$ residuals
df_res<- tibble(z_res =(res - mean(res))/sd(res))

qqplot<- ggplot(df_res,aes(sample = z_res))+
  geom_abline(intercept = 0, slope=1 ,linetype="dashed")+
  geom_qq()+
  labs(x= "標準正規分布", y="標準化した残差の分布")
print(qqplot)
##########
##run a regression
summary(lm_3)
reg.or<-exp(coefficients(lm_3))
table_reg1<-stargazer(lm_3, type="latex", coef=list(reg.or), p.auto=FALSE, out="logitor.tex")

## figure
df_new2<-

