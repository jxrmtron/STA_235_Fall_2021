library(ggplot2)
library(tidyverse)
library(mosaic)


LOL_df <- LOL %>%
  select(blueWardsPlaced, blueWardsDestroyed, blueKills, blueDeaths, blueAssists, blueTotalGold, blueDragons, blueHeralds)

LOL_df <- LOL_df%>%
  rename("Dragon" = blueDragons,
         "Herald" = blueHeralds,
         
         
  )

LOL_df$Dragon <- as.factor(LOL_df$Dragon)

LOL_df$Herald <- as.factor(LOL_df$Herald)

# remember our old model
lm1 <- lm(blueTotalGold ~ blueKills, data=LOL_df)
summary(lm1)

ggplot(LOL_df) + 
  geom_boxplot(aes(y=blueTotalGold, x=Dragon),size=2) + 
  theme_grey(base_size=24)

diffmean(blueTotalGold ~ Dragon, data=LOL_df)


ggplot(LOL_df) + 
  geom_boxplot(aes(y=blueTotalGold, x=Herald),size=2) + 
  theme_grey(base_size=24)

diffmean(blueTotalGold ~ Herald, data=LOL_df)

lm2 <- lm(blueTotalGold ~ Dragon, data=LOL_df)
summary(lm2)


lm2 <- lm(blueTotalGold ~ Herald, data=LOL_df)
summary(lm2)


  
lm3 <- lm(blueTotalGold ~ blueKills +Herald + Dragon , data=LOL_df)
summary(lm3)

LOL_df_1000 <- data_frame(resample(LOL_df,1000))

lm3 <- lm(blueTotalGold ~ blueKills+ Herald + Dragon , data=LOL_df_1000)
summary(lm3)

L <- ggplot(LOL_df_1000, aes(x=blueKills, y=blueTotalGold, colour=Herald)) +
  geom_jitter(size=1) +
  theme_grey(base_size=18)

L+ geom_line(aes(y=predict(lm3)), size=2)


lm4 <- lm(blueTotalGold ~ blueKills + Dragon + Herald:Dragon, data=LOL_df)
summary(lm4)

summary( lm(blueTotalGold ~ Herald*Dragon, data=LOL_df) )

summary( lm(blueTotalGold ~ Herald:Dragon, data=LOL_df) )

summary( lm(blueTotalGold ~ 0+ Herald:Dragon, data=LOL_df) )



