### pretty professors placeholder

library(tidyverse) 
library(moderndive) # for data
data(evals)

library(ggplot2) # for plotting

options(scipen=999)
#Working DataSet
LOL_df <- LOL %>%
  select(blueWardsPlaced, blueWardsDestroyed, blueKills, blueDeaths, blueAssists, blueTotalGold, blueTotalMinionsKilled, blueTowersDestroyed)


# plot Gold Valuation vs Kill Count
ggplot(LOL_df, aes(x = blueTotalGold, y = blueKills)) +
  geom_point() +
  labs(x = "Total Gold", 
       y = "Kills",
       title = "Scatterplot of relationship of Gold and Kills")

# add some noise the the points so that they don't overlap
ggplot(LOL_df, aes(x = blueTotalGold, y = blueKills)) +
  geom_jitter() +
  labs(x = "Total Gold", 
       y = "Kills",
       title = "Scatterplot of relationship of Gold and Kills")


# strength of the linear relationship between score and beauty
# we use the $ to access the columns of a data frame
# dataframe name: evals
# column name: score
# column data we can work with: evals$score
cor(LOL_df$blueTotalGold, LOL_df$blueKills)


# fit a simple linear model
# note we don't need $ since we are providing evals in the 'data'
# argument
# we could do this though
# lm(evals$score ~ evals$bty_avg)

lm1 <- lm(blueTotalGold ~ blueKills, data=LOL_df)
# view some summary statistics
summary(lm1)

#note the R^2 is equal to the correlation squared
cor(LOL_df$blueTotalGold, LOL_df$blueKills)^2


# confidence interval
confint(lm1)



# bootstrapping
library(mosaic)
lmboot <- do(1000)*lm(blueTotalGold ~ blueKills, data=resample(LOL_df))
confint(lmboot)
hist(lmboot$blueKills)
