library(haven)
df <- read_dta("HPRICE_24subset.dta")
View(df)
sapply(df, attr,"label")
