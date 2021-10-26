library(ggplot2)
library(tidyverse)
library(mosaic)

#Working Data
LOL_df <- LOL %>%
  select(blueWardsPlaced, blueWardsDestroyed, blueKills, blueDeaths, blueAssists, blueTotalGold, blueDragons, blueHeralds)

#Renaming Variables
LOL_df <- LOL_df%>%
  rename("Dragon" = blueDragons,
         "Herald" = blueHeralds,
         
         
  )

#Turning the numerical variables into categorical variables
LOL_df$Dragon <- as.factor(LOL_df$Dragon)

LOL_df$Herald <- as.factor(LOL_df$Herald)

# remember our old model
lm1 <- lm(blueTotalGold ~ blueKills, data=LOL_df)
summary(lm1)

#Gold valuation distributions for teams that killed a dragon and teams that did not
ggplot(LOL_df) + 
  geom_boxplot(aes(y=blueTotalGold, x=Dragon),size=2) + 
  theme_grey(base_size=24)

#The difference in gold valuation averages between 
diffmean(blueTotalGold ~ Dragon, data=LOL_df)

#Gold valuation distributions for teams that killed a herald and teams that did not
ggplot(LOL_df) + 
  geom_boxplot(aes(y=blueTotalGold, x=Herald),size=2) + 
  theme_grey(base_size=24)
#The difference in gold valuation averages between 
diffmean(blueTotalGold ~ Herald, data=LOL_df)

#How much is a dragon worth
lm2 <- lm(blueTotalGold ~ Dragon, data=LOL_df)
summary(lm2)

# How much is a herald worth
lm2 <- lm(blueTotalGold ~ Herald, data=LOL_df)
summary(lm2)


#Model with both categorical variables
lm3 <- lm(blueTotalGold ~ blueKills +Herald + Dragon , data=LOL_df)
summary(lm3)

# Smaller dataset to visualize the data easier
LOL_df_1000 <- data_frame(resample(LOL_df,1000))


lm3 <- lm(blueTotalGold ~ blueKills+ Herald + Dragon , data=LOL_df_1000)
summary(lm3)

#How gold valuatin changes based on kills seperated by whether the tean killed a herald or not
L <- ggplot(LOL_df_1000, aes(x=blueKills, y=blueTotalGold, colour=Herald)) +
  geom_jitter(size=1) +
  theme_grey(base_size=18)

L+ geom_line(aes(y=predict(lm3)), size=2)

#Interaction between Herald and Dragon 
lm4 <- lm(blueTotalGold ~ blueKills + Dragon + Herald:Dragon, data=LOL_df)
summary(lm4)

summary( lm(blueTotalGold ~ Herald*Dragon, data=LOL_df) )

summary( lm(blueTotalGold ~ Herald:Dragon, data=LOL_df) )

summary( lm(blueTotalGold ~ 0+ Herald:Dragon, data=LOL_df) )
