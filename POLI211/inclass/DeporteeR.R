setwd("~/Teaching/Grad methods/2022 POLI 211/POLI 211 Exercises S22/Deportees")

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

df <- read_dta("DeporteePanelTrimmed.dta")

View(df)



##Create labels##
df <- apply_labels(df, own_econ_guat = "Econ Guat", own_danger_guat="Danger Guate", own_danger_us="Danger US", own_econ_us="Econ US",
                own_travel= "Travel Danger", risk="Risk", female="Female", age="Age", indigenous="Indigenous", 
                ln_timeus="Ln Time US", num_migration_new="Migration times", 
                r1bin_migrate="R1 Migration Choice", r2bin_migrate="R2 Migration Choice", english="English", 
                highschool="High School", assets="US Assets", 
                victim_us="Victim in US", new_kidsgt="Kids Guat", new_kidsus="Kids US")



##OLS for comparison　まずやってみよう##
model_ols <- lm( df$r2bin_migrate ~ risk + own_econ_guat +  own_danger_guat  +  own_econ_us 
                    +  own_danger_us  + own_travel  + ln_timeus  + indigenous  + new_kidsgt  + new_kidsus 
                    +   num_migration_new  + assets +  victim_us + female + age + english + highschool,
                  data=df,  na.action=na.omit)
summary(model_ols)
#Parsimony vs data fidelity...  What are the tradeoffs here?



##Logit model##
model_logit <- use_labels(df, glm(r2bin_migrate ~ risk + own_econ_guat +  own_danger_guat  +  own_econ_us 
                    +  own_danger_us  + own_travel  + ln_timeus  + indigenous  + new_kidsgt  + new_kidsus 
                    +   num_migration_new  + assets +  victim_us + female + age + english + highschool, 
                    data=df,  na.action=na.omit, family=binomial(link='logit')))
summary(model_logit)


#picture's worth a thousand words よくある表で可視化#
coefplot(model_logit, intercept=FALSE, title="TITLE HERE", )

#transform log odds (current form of coefficients) to odds ratio: オッズ比にして、マイナス値をなくそう#
logit.or<-exp(coefficients(model_logit))
logit.or 


#make a table!#
#Note, can edit code to add better title, variable lables, etc. 出てきたコードをラテックにコピペ#
table1<-stargazer(model_logit, type="latex", coef=list(logit.or), p.auto=FALSE, out="logitor.tex")

# quick and dirty workaround for removing backticks  
remove_backticks = function(text){
  text = gsub("([^A-z]+)`", "\\1", text, perl = TRUE)
  text = gsub("`([^A-z]+)", "\\1", text, perl = TRUE)
  text = gsub("(^`)|(`$)", "", text, perl = TRUE)
  text
}

table1 = remove_backticks(table1)

writeLines(table1, "logitor.tex")



##Probit model##
model_probit <- use_labels(df, glm(  r2bin_migrate ~ risk + own_econ_guat +  own_danger_guat  +  own_econ_us 
                    +  own_danger_us  + own_travel  + ln_timeus  + indigenous  + new_kidsgt  + new_kidsus 
                    +   num_migration_new  + assets +  victim_us + female + age + english + highschool,
                      na.action=na.omit, family=binomial(link='probit')))
summary(model_probit)
# econ Guat の解釈方法（係数0.67）：もし状況が１ポイントよいと事故報告した場合、移住するオッズは0.33低くなる
# ネガティブエフェクトの場合、0.67倍というのは反直観的なので、オッズ比（1- 係数）を使って表す
# 写真参照


#make a table with two columns!違うモデルを用いて２つコラムのある表を作ってみよう#
#Note, can edit code to add better title, variable lables, etc. 
stargazer(model_logit, model_probit, type="latex", p.auto=FALSE, out="logitor.tex")


#Marginal effects to help with interpretation　averageの人についての効果を調べるため調べるため（確認作業）
m <- glm(  r2bin_migrate ~ risk + own_econ_guat +  own_danger_guat  +  own_econ_us 
                                     +  own_danger_us  + own_travel  + ln_timeus  + indigenous  + new_kidsgt  + new_kidsus 
                                     +   num_migration_new  + assets +  victim_us + female + age + english + highschool,
                                     na.action=na.omit, data=df, family=binomial(link='logit'))
shinym<-margins(m)　#これがmagrginal effectを計算中
shinym

#lots of variations you can do
# see documentation:
# https://cran.r-project.org/web/packages/margins/vignettes/TechnicalDetails.pdf

#also note interpretation of logit interactions here!
#https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html