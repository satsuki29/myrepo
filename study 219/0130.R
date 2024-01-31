library(foreign)
 library(survey)
library(knitr)
Oct16 <- read.spss("ATP W116.sav", to.data.frame = TRUE, use.value.labels = TRUE, max.value.labels = Inf, trim.factor.names = TRUE, reencode = "UTF-8")
colnames(Oct16)

Oct16_clean <- Oct16[, c("DRLEAD_W116", "THERMTRUMP_W116", "THERMBIDEN_W116", "CANDHOUKN_W116")]



Oct16_clean %>%
  mutate(party= case_when(DRLEAD_W116 =="The Republican Party" ~ -1,
                          DRLEAD_W116 =="The Democratic Party" ~ 1,
                          DRLEAD_W116 == "Not sure" ~ 0
  )
  )

Oct16_clean_2 <-  Oct16_clean %>%
  mutate(party = case_when(
    DRLEAD_W116 == "The Republican Party" ~ -1,
    DRLEAD_W116 == "The Democratic Party" ~ 1,
    DRLEAD_W116 == "Not sure" ~ 0
  )) %>%
mutate(CANDHOUKN_W116 = case_when(
  CANDHOUKN_W116 == "Nothing at all" ~ 0,
  CANDHOUKN_W116 == "Not too much" ~ 1,
  CANDHOUKN_W116 == "A fair amount" ~ 2,
  CANDHOUKN_W116 == "A great deal" ~ 3,
))

lm(THERMBIDEN_W116 ~ party, data = Oct16_clean_2)
lm(THERMBIDEN_W116 ~ CANDHOUKN_W116, data = Oct16_clean_2)