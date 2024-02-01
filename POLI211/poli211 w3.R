library(foreign)
library(readstata13)
library(dplyr)
library(ggplot2)
#install.packages("describedata")
library(describedata)
library(readr)
messerli <- read_csv("messerli.csv")


nobel_data <- messerli
summary(nobel_data$chocolate)
summary(nobel_data$nobel)
hist(nobel_data$gdppc)

# Is a normal distribution the best fit distribution for the data?
ladder(nobel_data$gdppc)
gladder(nobel_data$gdppc)

nobel_data %>%
  ggplot(aes(x = chocolate, y = nobel)) +
  geom_point(colour = "red")


cor(nobel_data$chocolate, nobel_data$nobel)

simple_model <- lm( nobel ~ chocolate , data = nobel_data, na.action=na.omit )

summary(simple_model)

nobel_data %>%
  ggplot(aes(x= chocolate, y= nobel)) +
  geom_point(colour = " blue") +
  geom_smooth(method = "lm", se = T)

multiple_model <- lm( nobel ~ chocolate + gdppc + education, data = nobel_data, na.action=na.omit )
summary(multiple_model)

