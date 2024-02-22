
#setwd("~/Teaching/Grad methods/2024 POLI 211/POLI 211 Exercises S22/Wine")


library(foreign)
library(readstata13)
library(dplyr)
library(ggplot2)
#install.packages("describedata")
library(describedata)
library(haven)

#new:
#install.packages("olsrr")
library(olsrr)

dataframe <- read_stata("wine.dta")
head(dataframe)

#visually assess for outliers.  What do you see?
plot(dataframe$alcohol, dataframe$liver)

dataframe %>%
  ggplot(aes(x = alcohol, y = liver)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", fill = NA)
# 最後のgeom_smoothは、以下のような候補もあります
#“loess”: ローカル回帰スムージングを使用します。データの局所的な傾向を考慮して滑らかな曲線を描画します。
#“gam”: 一般化加法モデルを使用します。非線形な傾向を捉えるために使用されます。
#“rlm”: ロバスト線形モデルを使用します。外れ値に対して頑健な回帰直線を描画します。

#model
livermodel <- lm(liver ~ alcohol, data=dataframe, na.action = na.omit)
summary(livermodel)

#cooks distance
#examines how much all of the fitted values change when the ith observation is deleted.
ols_plot_cooksd_bar(livermodel)
ols_plot_cooksd_chart(livermodel)


#large residuals (standardized)
#and large leverage
ols_plot_resid_lev(livermodel)

#dffits
#scaled difference between the ith fitted value obtained from the full data and the ith fitted value obtained by deleting the ith observation. DFFIT - difference in fits, is used to identify influential data points. It quantifies the number of standard deviations that the fitted value changes when the ith data point is omitted.
ols_plot_dffits(livermodel)

#dfbeta
#difference in each parameter estimate with and without the influential point. There is a DFBETA for each data point i.e if there are n observations and k variables, there will be n*k DFBETAs
ols_plot_dfbetas(livermodel)


#now look at outliers for a model regressing heart disease on alcohol consumption.


heartmodel <- lm(heart ~ alcohol, data=dataframe, na.action = na.omit)
summary(heartmodel)

#cooks distance
#examines how much all of the fitted values change when the ith observation is deleted.

ols_plot_cooksd_chart(heartmodel)


#large residuals (standardized)
#and large leverage
ols_plot_resid_lev(heartmodel)


#dffits
#scaled difference between the ith fitted value obtained from the full data and the ith fitted value obtained by deleting the ith observation. DFFIT - difference in fits, is used to identify influential data points. It quantifies the number of standard deviations that the fitted value changes when the ith data point is omitted.
ols_plot_dffits(heartmodel)

#dfbeta
#difference in each parameter estimate with and without the influential point. There is a DFBETA for each data point i.e if there are n observations and k variables, there will be n*k DFBETAs
ols_plot_dfbetas(heartmodel)


