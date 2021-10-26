library(ggplot2)
library(tidyverse)

options(scipen = 999)

#Creating Red Ward Ratio
LOL_df <- LOL %>%
  mutate(redWardRatio = (redWardsPlaced+redWardsDestroyed)/(blueWardsPlaced+blueWardsDestroyed))

#Selecting variables
LOL_df <- LOL_df %>%
  select(blueWins,blueFirstBlood,blueGoldPerMin,blueWardsPlaced, blueWardsDestroyed, blueKills, blueDeaths, blueAssists,blueAvgLevel,blueTotalJungleMinionsKilled, blueDragons, blueHeralds,blueGoldDiff,blueExperienceDiff,blueTotalGold,redWardRatio,redAvgLevel,redGoldDiff,)

# Creating AvgLevelDiff Variable
LOL_df <- LOL_df %>%
  mutate(AvgLevelDiff = (blueAvgLevel-redAvgLevel))

#Rename
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
         "Win" = blueWins,
         "10MinGold" = blueTotalGold
         
         
  )

#Factor
LOL_df$Dragon <- as.factor(LOL_df$Dragon)

LOL_df$Herald <- as.factor(LOL_df$Herald)

LOL_df$FirstBlood <- as.factor(LOL_df$FirstBlood)

LOL_df$Win <- as.factor(LOL_df$Win)

class(LOL_df$Dragon)



#First model
lm1 <- lm(GoldPerMin ~ redGoldDiff, data = LOL_df)
summary(lm1)

#Poor Linear Fit
ggplot(LOL_df, aes(x = redGoldDiff, y = GoldPerMin)) +
  geom_jitter(width = .01,height = .01) +
  labs(x = "redGoldDiff", 
       y = "GoldPerMin",
       title = "Scatterplot of relationship of Red Gold Difference and Blue Gold Per Min") +
  geom_smooth(method = "lm", se = FALSE)

plot(lm1,1:5)

#Quadratic Fit
ggplot(LOL_df, aes(x = redGoldDiff, y = GoldPerMin)) +
  geom_jitter(width = .01,height = .01) +
  labs(x = "redGoldDiff", 
       y = "GoldPerMin",
       title = "Scatterplot of relationship of Red Gold Difference and Blue Gold Per Min") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

#Creating Squared Variable for Model
LOL_df <- LOL_df %>%
  mutate(redGoldDiffSquared = (redGoldDiff^2))

#Different Ways to incorporate it into your model
lm2 <- lm(GoldPerMin ~ redGoldDiff + redGoldDiffSquared, data = LOL_df)
summary(lm2)

lm2 <- lm(GoldPerMin ~ redGoldDiff + I(redGoldDiff^2), data = LOL_df)

lm2 <- lm(GoldPerMin ~ poly(redGoldDiff,2, raw = TRUE), data = LOL_df)

source('https://tinyurl.com/y4krd9uy') # simple_anova function

#Does it help us out?
anova(lm1,lm2)

simple_anova(lm2) 

# Log Transformation and Doubling

ggplot(LOL_df, aes(x = redGoldDiff, y = GoldPerMin)) +
  geom_jitter(width = .01,height = .01) +
  labs(x = "redGoldDiff", 
       y = "GoldPerMin",
       title = "Scatterplot of relationship of Red Gold Difference and Blue Gold Per Min") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)


lm4 <- lm(log(GoldPerMin) ~ redGoldDiff, data = LOL_df)

summary(lm4)

# If we wanted to double our gold per min, we would have to increase our gold difference by 22K
log(2)/-0.0000306958




#Expanding a dataset

ggplot(LOL_df, aes(x = redWardRatio, y = GoldPerMin)) +
  geom_jitter(width = .01,height = .01) +
  labs(x = "redWardRatio", 
       y = "GoldPerMin",
       title = "Scatterplot of relationship of Red Gold Difference and Blue Gold Per Min") +
  geom_smooth(method = "lm", se = FALSE)



ggplot(LOL_df, aes(x = log(redWardRatio), y = log(GoldPerMin))) +
  geom_jitter(width = .01,height = .01) +
  labs(x = "log(redWardRatio)", 
       y = "log(GoldPerMin)",
       title = "Scatterplot of relationship of Red Gold Difference and Blue Gold Per Min") +
  geom_smooth(method = "lm", se = FALSE)



lm2 <- lm(log(GoldPerMin) ~ log(redWardRatio) , data = LOL_df)

summary(lm2)







