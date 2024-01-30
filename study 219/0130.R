library(foreign)
 library(survey)
library(knitr)
Oct16 <- read.spss("ATP W116.sav", to.data.frame = TRUE, use.value.labels = TRUE, max.value.labels = Inf, trim.factor.names = TRUE, reencode = "UTF-8")
colnames(Oct16)
Oct16_clean <- Oct16[, c("DRLEAD_W116", "THERMTRUMP_W116", "THERMBIDEN_W116", "CANDHOUSAT_W116")]
