library(ggplot2)
library(tidyverse)
library(mosaic)
library(olsrr)

options(scipen = 999)

#Working Data
LOL_df <- LOL %>%
  select(blueWins,blueFirstBlood,blueGoldPerMin,blueWardsPlaced, blueWardsDestroyed, blueKills, blueDeaths, blueAssists,blueAvgLevel,blueTotalJungleMinionsKilled, blueDragons, blueHeralds,blueGoldDiff,blueExperienceDiff,redAvgLevel)

LOL_df <- LOL_df %>%
  mutate(AvgLevelDiff = (blueAvgLevel-redAvgLevel))
#Renaming Variables

LOL_df <- LOL_df %>%
  rename("Dragon" = blueDragons,
         "Herald" = blueHeralds,
         "FirstBlood" = blueFirstBlood,
         "GoldPerMin" = blueGoldPerMin,
         "WardsPlaced" = blueWardsPlaced,
         "WardsDestroyed" = blueWardsDestroyed,
         "Kills" = blueKills,
         "Deaths" = blueDeaths, 
         "Assists" = blueAssists,
         "AvgLevel" = blueAvgLevel,
         "JungleKilled" = blueTotalJungleMinionsKilled,
         "GoldDiff" = blueGoldDiff,
         "ExperienceDiff" = blueExperienceDiff,
         
         
  )
########Take a look at your working dataset

#Turning the numerical variables into categorical variables
LOL_df$Dragon <- as.factor(LOL_df$Dragon)

LOL_df$Herald <- as.factor(LOL_df$Herald)

LOL_df$FirstBlood <- as.factor(LOL_df$FirstBlood)

class(LOL_df$Dragon)

# Working Model
lm1 <- lm(GoldPerMin ~ Kills + FirstBlood + Deaths + Assists + AvgLevel + GoldDiff + AvgLevelDiff, data=LOL_df)
summary(lm1)

lm2 <- lm(GoldPerMin ~ Kills + Deaths + Assists + AvgLevel + GoldDiff + AvgLevelDiff, data=LOL_df)
summary(lm2)

########## Anova Table
source('https://tinyurl.com/y4krd9uy') # simple_anova function

simple_anova(lm1)

simple_anova(lm2)

#Add Fitted Values to dataset
LOL_df<- LOL_df %>%
  mutate(EGoldPerMin = lm2$fitted.values)


#Add Residuals to dataset
LOL_df<- LOL_df %>%
  mutate(Residuals = lm2$residuals)


#Assumption Plots
#1 is Residuals vs Fitted
#2 is Normality Plot
#3 is Studentized Residuals
#4 Cook's Distance
#5 Reiduals vs Leverage

ggplot(LOL_df, aes(x = GoldDiff, y = GoldPerMin)) +
  geom_point() +
  labs(x = "GoldDiff", 
       y = "GoldPerMin",
       title = "Scatterplot of relationship of Gold and Kills")



plot(lm2, which = 1:5)

#Better Cooks Distance plot
ols_plot_cooksd_bar(lm2)




