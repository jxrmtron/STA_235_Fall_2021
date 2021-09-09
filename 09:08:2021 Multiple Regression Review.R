#Load Packages

library(tidyverse)

#Change Significant Figures
options(scipen = 999)

#Create New Variable - Ratio of Ward Advantage for Blue team:Red Team. Measure of Vision Control
LOL_df <- LOL %>%
  mutate(blueWardRatio = (blueWardsPlaced+blueWardsDestroyed)/(redWardsPlaced+redWardsDestroyed))

#Filtering dataset to variables that we care about
LOL_df <- LOL_df %>%
  select(blueWardRatio, blueKills, blueDeaths, blueAssists, blueTotalGold, blueTotalMinionsKilled, blueTowersDestroyed
  )    

#View first view observations
glimpse(LOL_df)

##############
##################
#####################
#This code shows you how to rename your variables. It's useful when building your script, but it is important to understand your variables
LOL_df <- LOL_df%>%
  rename("BWR" = blueWardRatio, 
         "BK" = blueKills, 
         "BD" = blueDeaths, 
         "BA" = blueAssists,
         "BG" = blueTotalGold, 
         "BMK" = blueTotalMinionsKilled, 
         "BTD" = blueTowersDestroyed
         
  )

#Multiple Regression
lm1 <- lm(BG ~ BK + BD + BWR, data=LOL_df)


summary(lm1)

coefs <- coef(lm1)

coefs

confint(lm1)

#Prediction Intervals 
predict(lm1, list(`BK` = 5,
                  `BD` = 4,
                  `BWR` = 3.5), interval = "prediction")
predict(lm1, list(`BK` = 5,
                  `BD` = 4,
                  `BWR` = 3.5), interval = "confidence")

        