
#setwd("~/Teaching/Grad methods/2024 POLI 211/POLI 211 Exercises S24/Soda")

library(foreign)
library(readstata13)
library(dplyr)
library(ggplot2)
#install.packages("describedata")
library(describedata)
library(haven)

dataframe <- read_dta("discrim.dta")

#summarize first 5 observations of each variable in the dataframe
head(dataframe)

#summarize first 5 obs for one specific variable
head(dataframe$emp2)

#generate a binary (0/1) variable from continuous variable
dataframe$majblk <- ifelse(dataframe$prpblck>.5,1,0)
library(fastDummies)
dataframe_2 <- fastDummies::dummy_cols(dataframe, select_columns = "chain")

#histogram
hist(dataframe$majblk)

View(dataframe$majblk)

####################

#MULTICOLLINEARITY (AND OTHER USEFUL THINGS)

#What happens if you have perfect collinearity (same variable twice)
dataframe$nmgrs_double<-dataframe$nmgrs
broken_model<-lm( psoda ~ nmgrs+ nmgrs_double, data = dataframe, na.action=na.omit)
summary(broken_model)


#What determines soda prices?

#Say you start with a "kitchen sink" model (not scientific or hypothesis driven; not recommended!)
big_model<-lm( psoda ~ nmgrs+ nregs+ hrsopen +emp+ prpblck +prppov+ prpncar +hseval+ nstores + lincome, data = dataframe, na.action=na.omit)
summary(big_model)

#How calculate R2 if the following are true:
#ESS = 1.0449 estimated sum of squares
#RSS = 1.9558 risidual sum of squares
#TSS = 3.0008 total sum of squares
#R^2= ESS/ TSS OR 1- RSS/ TSS= 0.36


#what's the variance inflation factor (VIF) for our model?
install.packages("car")
library(car)
vif(big_model)



# Correlation shows which X variables are most correlated with each other
# need to create a sub matrix of the variables you're interested in
# But first, going to drop all observations with missing data, as that affects PCA below
subset_y <- dataframe[, c(1,5,6,7,8,22,23,24,25,26,32)]
subset_y  <- na.omit(subset_y)
subset <- subset_y[, c(2:11)]
res<-cor(subset,  use = "complete.obs")
round(res, 2)

# One option for multicollinearity = remove one of the highly correlated variables
# Controversial! Why?
new_model<-lm( psoda ~ nmgrs+ nregs+ hrsopen +emp+ prpblck +hseval+ nstores + lincome, data = dataframe, na.action=na.omit)
summary(new_model)
vif(new_model)

#or, use our new binary variable for easier interpretation:
new_model2<-lm( psoda ~ nmgrs+ nregs+ hrsopen +emp+ majblk +hseval+ nstores + lincome, data = dataframe, na.action=na.omit)
summary(new_model2)

#how to interpret results for lincome? majblk? prpblck?
#does this suggest discrimination exists?

#Another option: Automate search for most parsimonious model
#Will give best model up to the # of vars you specify (here = 8)
install.packages("LEAP")
library(leaps)
models <- regsubsets(subset_y$psoda~., data = subset, na.action=na.omit, nvmax = 8)
##データフレームから特定の列を抽出して新しいデータフレームを作成
summary(models)
#Note which variable remains omitted!  Why, do you think?


#Another option: create an index.  
#but what does the index mean?  And what is the approprate (logic & theory driven) way to make an index?
dataframe$prpvars<-(dataframe$prpblck+dataframe$prppov+dataframe$prpncar)
new_model3<-lm( psoda ~ nmgrs+ nregs+ hrsopen +emp+ prpvars +hseval+ nstores + lincome, data = dataframe, na.action=na.omit)
summary(new_model3)
vif(new_model3)

#Another option: Principal Component Analysis (PCA) combines variables more systematically
install.packages("factoextra")
library(factoextra)

res.pca <- prcomp(na.omit(subset), scale = TRUE)
fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

eig.val <- get_eigenvalue(res.pca)
eig.val

#variable representation in each dimension
res.var <- get_pca_var(res.pca)
head(res.var$coord )         # Coordinates
head(res.var$contrib     )   # Contributions to the PCs
head(res.var$cos2         )  # Quality of representation 

#representation by individual
res.ind <- get_pca_ind(res.pca)
head(res.ind$coord)          # Coordinates
head(res.ind$contrib)        # Contributions to the PCs
head(res.ind$cos2)           # Quality of representation 

scores<-res.pca$x
model_pca <- lm( subset_y$psoda ~ scores[,1:2])
summary(model_pca)
#How do we interpret these results?

####################

#Review on calculating y_hat; residuals 

#predicted values for y (price)

summary(new_model3)
y_hat  <- predict(new_model3) 

#calculated residuals (unexplained part of y)
resid <- residuals(new_model3)       

##################

#HETEROSKEDASTICITY

#using dataframe="dataframe", from above.

# A QQ plot to test for normal distribution of variable
library("car")
qqPlot(dataframe$lincome)
qqPlot(dataframe$prpblck)
#if all the points fall within the lines, we can assume normality

# Visually inspect the relationship between my data
# Y and X
scatterplot(dataframe$prpblck, dataframe$psoda)
# X and X
scatterplot(dataframe$hseval, dataframe$lincome)

# Do my residuals vary with my predicted Y values?
scatterplot(resid, y_hat)

#Breusch-Pagan Test
library(lmtest)
bpmodel<- psoda ~ nmgrs+ nregs+ hrsopen +emp+ prpvars +hseval+ nstores + lincome
bptest(bpmodel, data=dataframe, studentize=FALSE)
# low p value rejects null of Homoskedasticity.

# Correct heteroskedasticity using ROBUST STANDARD ERRORS
# library(lmtest)
library(sandwich)
 new_model3_robust<-coeftest(new_model3, vcov = vcovHC(new_model3, type="HC0"))
 new_model3_robust
 
 #compare to uncorrected SEs:
 summary(new_model3)
 #note coefficients stay same but SEs are bigger.
 
 