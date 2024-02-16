library(haven)
df <- read_dta("HPRICE_24subset.dta")
View(df)
sapply(df, attr,"label")
df2<- df[, c(1,2,3,4,5,9)]
sapply(df2, attr,"label")

#price measures median housing price, unit is dollar.
#crime measures crimes committed. unit is times per capita.
#rooms measures average numbers of room per house.
#dist measures weighted distance to 5 employ centers. unit is miles.
#nox measures Nitrogen oxide concentration . unit is parts per 100m.
#lowstat measures percentage of people of 'lower status'. unit is percentage.

#2
describedata::gladder(df$price)
# logged form is appropreate because when using OLS, obserbvations are clustered among left side of the figure.
describedata::gladder(df$crime)
# logged is appropriate because when using OLS observations are clustered among left side of the figure.
describedata::gladder(df$nox)
# logged is appropriate because when using OLS observations are clustered among left side of the figure.
describedata::gladder(df$rooms)
# logged is appropriate because when using OLS observations are clustered among left side of the figure.
describedata::gladder(df$dist)
# logged is appropriate because when using OLS observations are clustered among left side of the figure.
describedata::gladder(df$lowstat)
# logged is appropriate because when using OLS observations are clustered among left side of the figure.


df$log_price<- log(df$price)
df$log_crime<- log(df$crime)
df$log_nox<- log(df$nox)
df$log_rooms<- log(df$rooms)
df$log_dist<- log(df$dist)
df$log_lowstat<- log(df$lowstat)

#4

cor(df[, c( "log_crime", "log_nox", "log_rooms", "log_dist")])
# the biggest is 0.8589437, which is the correlation between log_dist and log_nox.
# We should be worried because the number is near to  1 and shows that log_dist and log_nox are storongly corralated.
# Multicollinearity leads to missing the actual affect to dependent variable.

#5

lm_df<- lm(log_price~ log_crime+ log_rooms+ log_dist+ log_dist+ log_nox, data = df, na.action = na.omit)
summary(lm_df)
# increasing 1% change in crimes commited per capita change predicts an an average of 7% decrease in median housing price.
# 
# increasing 1  capita of change predicts an an average of 7 dollars decrease in median housing price.

#6
lm_df_lowstat<- lm(lowstat ~ log_crime+ log_rooms+ log_dist+ log_dist+ log_nox, data = df, na.action = na.omit)

