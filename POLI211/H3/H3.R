yi=5.25+ 1.34*Bxi

x<- c(24,9,13,18,16,8,11,15,16,10)
y<- c(39, 12, 23, 32, 14,24,20,25, 34, 17)
df<- data.frame(x,y)

line<- lm(y~x, data=df)
summary(line)
mean(24,9,13,18,16,8,11,15,16,10)
mean(y)
###########################################################################
library(readxl)
> movie <- read_excel("movie.xls")
> View(movie)

line_movie<- lm(x1~x3+x4, data = movie,  na.action=na.omit)
summary(line_movie)

line_movie3<- lm(x1~x3, data = movie,  na.action=na.omit)
summary(line_movie3)
e1<-residuals(line_movie3)

line_movie43<- lm(x4~x3, data = movie,  na.action=na.omit)
summary(line_movie4)
e2<-residuals(line_movie43)

line_movie_e12<- lm(e1~e2, data = movie,  na.action=na.omit)
summary(line_movie_e12)

######################################################################
library(haven)
> HPRICE_24subset <- read_dta("~/GitHub/myrepo/POLI211/H2/HPRICE_24subset.dta")
> View(HPRICE_24subset)

lm_imperfect<- lm(crime~ +price+ proptax+ lproptax + stratio+ dist + radial + rooms+ lowstat + lnox, data = HPRICE_24subset, na.action = na.omit)
summary(lm_imperfect)

library(car)
vif(lm_imperfect)

lm_afteromit<- lm(crime~ +price+ lproptax + stratio+ dist + radial + rooms+ lowstat + lnox, data = HPRICE_24subset, na.action = na.omit)
summary(lm_afteromit)
vif(lm_afteromit)

qqPlot(HPRICE_24subset$price)

library(lmtest)
bpmodel<- crime~ +price+ lproptax + stratio+ dist + radial + rooms+ lowstat + lnox
bptest(bpmodel, data=HPRICE_24subset, studentize=FALSE)
