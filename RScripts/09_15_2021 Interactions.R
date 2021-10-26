library(tidyverse)
library(ggplot2)

options(scipen = 999)

#Working Data
LOL_df <- LOL %>%
  select( blueKills, blueDeaths, blueAssists, blueTotalGold,blueTotalMinionsKilled, blueCSPerMin, )

#Relationship between Deaths and Gold Valuation
ggplot(LOL_df)+
  geom_point(aes(x=blueDeaths, y=blueTotalGold),colour='blue', size=3)+
  xlab("Deaths")+
  ylab("Gold")

#Linear Model
lm1 <- lm(blueTotalGold ~ blueDeaths, data=LOL_df)
summary(lm1)

#Multivariate Model
lm2 <- lm(blueTotalGold ~ blueDeaths + blueCSPerMin, data=LOL_df)
summary(lm2)

#Multivariate Model with interaction
lm3 <- lm(blueTotalGold ~ blueDeaths + blueCSPerMin + blueDeaths:blueCSPerMin, data=LOL_df)
summary(lm3)




