library(tidyverse)
library(mosaic)
library(olsrr)

LOL <- read.csv("C:/Users/jtt2284/Desktop/LOL.csv", sep=";")

options(scipen = 999)

LOL <- LOL %>%
  select(t1_earnedgold,t1_damagetochampions,t1_kills,t1_elementaldrakes, t1_heralds,t1_goldat15,t2_goldat25,t1_earned.gpm,t1_wardsplaced,t1_wardskilled,t2_wardsplaced,t2_wardskilled)

LOL <- LOL %>%
  mutate(GoldDiff_15 = t1_goldat15 - t2_goldat25,
         WardRatio = (t1_wardsplaced+t1_wardskilled)/(t2_wardsplaced+t2_wardskilled) )




LOL$GoldDiff_15[1]


LOL <- na.omit(LOL)


LOL_df <- LOL%>%
  rename("Gold" = t1_earnedgold,
         "Dmg" = t1_damagetochampions,
         "Kills" = t1_kills,
         "Dragon" = t1_elementaldrakes,
         "Herald" = t1_heralds,
         "GoldPerMin"= t1_earned.gpm
         
         
         
  )



#Observing Response Variable

hist(LOL_df$Gold)

ggplot(data=LOL_df) +
  geom_histogram(aes(x=Gold)) 

#BootStrapping 
Gold_mean <- do(1000)*mean(~Gold, data=resample(LOL_df,2))

Gold_mean1 <- do(1000)*mean(~Gold, data=resample(LOL_df,30))

ggplot(data=Gold_mean) +
  geom_histogram(aes(x=mean)) 

ggplot(data=Gold_mean1) +
  geom_histogram(aes(x=mean)) 

#Linear Regression

ggplot(data = LOL_df) +
  geom_point(aes(x = Dmg, y = Gold))

cor(LOL_df$Gold, LOL_df$Dmg)

lm1 <- lm(Gold~I(Dmg/1000), data = LOL_df)

summary(lm1) 

confint(lm1)

plot(lm1)



#Bootstrapping Regression

lm1_boot <- do(1000)*lm(Gold~I(Dmg/1000), data = resample(LOL_df))

confint(lm1_boot)

#Multiple Regression

ggplot(data = LOL_df) +
  geom_point(aes(x = Dmg, y = Gold))

ggplot(data = LOL_df) +
  geom_point(aes(x = Dmg, y = Kills))

lm2 <- lm(Gold~I(Dmg/1000)+Kills, data = LOL_df)

summary(lm2)

predict(lm2, list(`Dmg` = 100000,
                  `Kills` = 30), interval = "confidence")

predict(lm2, list(`Dmg` = 100000,
                  `Kills` = 30), interval = "prediction")

#Categorical Variables 
class(LOL_df$Dragon)
class(LOL_df$Herald)

LOL_df$Dragon <- as.factor(LOL_df$Dragon)

LOL_df$Herald <- as.factor(LOL_df$Herald)

ggplot(LOL_df) + 
  geom_boxplot(aes(y=Gold, x=Dragon),size=2) + 
  theme_grey(base_size=24)

ggplot(LOL_df) + 
  geom_boxplot(aes(y=Gold, x=Herald),size=2) + 
  theme_grey(base_size=24)

lm3 <- lm(Gold ~ Dragon, data=LOL_df)

summary(lm3)

lm4 <- lm(Gold ~ Herald, data=LOL_df)

summary(lm4)

lm5 <- lm(Gold ~ I(Dmg/1000)+Kills+Herald+Dragon, data=LOL_df)

summary(lm5)

lm6 <- lm(Gold ~I(Dmg/1000) + Kills + Dragon + Herald:Dragon, data=LOL_df)
summary(lm6)

#Assumptions
plot(lm6)
ols_plot_cooksd_bar(lm6)

#Non-linear Transformations
ggplot(LOL_df, aes(x = GoldDiff_15, y = GoldPerMin)) +
  geom_point() +
  labs(x = "GoldDiff_15", 
       y = "Gold",
       title = "Scatterplot of relationship of Red Gold Difference and Blue Gold Per Min") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(LOL_df, aes(x = GoldDiff_15, y = GoldPerMin)) +
  geom_point() +
  labs(x = "GoldDiff_15", 
       y = "Gold",
       title = "Scatterplot of relationship of Red Gold Difference and Blue Gold Per Min") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

lm7 <- lm(GoldPerMin ~ GoldDiff_15, data = LOL_df)
summary(lm7)

lm8 <- lm(GoldPerMin ~ I(GoldDiff_15^2), data = LOL_df)
summary(lm8)

#Expanding Data using log

ggplot(data = LOL_df) +
  geom_point(aes(x = WardRatio, y = Gold))

ggplot(data = LOL_df) +
  geom_point(aes(x = log(WardRatio), y = log(Gold)))

lm9 <- lm(log(Gold) ~ log(WardRatio) , data = LOL_df)

summary(lm9)


                           