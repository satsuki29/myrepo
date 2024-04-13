setwd("~/Teaching/Grad methods/2022 POLI 211/POLI 211 Exercises S22/Guatemala")

library(foreign)
library(readstata13)
library(dplyr)
library(ggplot2)
#install.packages("describedata")
library(describedata)
library(haven)

dfwide <- read_stata("Guate panel wide.dta")

head(dfwide)


#Creating a first differences model
#(Easier with wide data)

#CODE IF DATASET IS "WIDE" (one obs per row; different column for data in each time period)
dfwide$dif_recentecon = dfwide$recent_econ2 - dfwide$recent_econ1

dfwide$dif_emp = dfwide$employed2 - dfwide$employed1

model_fd = lm(dif_recentecon ~ dif_emp, data = dfwide, na.action = na.omit)
summary(model_fd)

# compared to people who didn't get a job,
#people who became employed showed a change in econ welfare that is 0.42 less pessimistic than those who didn't get a job
# (higher econ values = more pessimistic)


#Note employment = 0 for all in t1, 
#so in this case, employed2 is actually the "difference" in employment from t1 to t2

model_econ <- lm( dif_recentecon ~ employed2, data=dfwide, na.action = na.omit)
summary(model_econ)


#what happens if you take the difference of a static variable, like male1 and male2??


#what would you do if you wanted to look at how 
#change in perceived economic wellbeing correlates with intent to migrate (not change in intent to migrate)?

# try writing the code.

# Consider how to run a "difference in differences" model.  
# How is/isn't it different from a "first differences" model?

##############################################

# Most panel analysis is done using LONG data
# Note on how to reshape data.... ("stubs" are important)

dflong <- read_stata("Guate panel long.dta")

head(dflong)


# Example of Difference in Differences:
dflong$period <- dflong$period -1

model_dd <- lm( recent_econ ~ period + employedperson + employedT, data=dflong, na.action = na.omit)
summary(model_dd)
#How do we interpret this result?  (Think interaction terms)

##

# Fixed Effects analysis
#install.packages("plm")
library(plm)


data(Cigar)
# explanation of variables here:
# https://search.r-project.org/CRAN/refmans/panelvar/html/Cigar.html


fxn <-  sales ~  price 


#2-way fixed effects
fixed <- plm(fxn, data=Cigar, index=c("state", "year"), model="within", effect = "twoways")
summary(fixed)
#remember to cluster your standard errors!  
#This code is for robust standard errors clustered by id.
coeftest(fixed, vcovHC(fixed, type = 'HC1', cluster = "group"))

#another approach -- one-way fixed effects in the code, plus unit fixed effects in the model
fixed2 <- plm( sales ~  price + factor(state), data=Cigar, index=c("year"), model="within")
summary(fixed2)

#Compare FE to dummy variable model
#Dummy variable model

model_dum <-  lm( sales ~  price + factor(year) + factor(state), data=Cigar )
summary(model_dum)


# Compare to RE model:
random <- plm(fxn, data=Cigar, index=c("state", "year"), model="random", effect = "twoways")
summary(random)
coeftest(random, vcovHC(random, type = 'HC1', cluster = "group"))


#Benefits of RE here?  (thinking about efficiency)
# But... what about bias?

# Do we pick random effects or fixed effects for panel data?
#Hausman test
phtest(fixed, random)



# Consider:
# How to create a pre/post treatment variable in panel data (see time series exercise)
# How to create a running time variable (see time series exercise)

# Ashenfelter's Dip  & Loess model

