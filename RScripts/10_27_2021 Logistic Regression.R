library(tidyverse)
library(mosaic)
library(ggplot2)

options(scipen = 999)

#Loading data
LOL <- read.csv("C:/Users/jdots/OneDrive/STA 235/STA_235_Fall_2021-R-Sessions-and-Scripts/LOL.csv", sep=";")
#Removing missing values
LOL <- na.omit(LOL)

#Selecting the variables
LOL_df <- LOL %>%
  select(t1_earnedgold,
         t1_golddiffat15,
         t1_earned.gpm,
         t1_damagetochampions,
         t1_kills,
         t1_deaths,
         t1_assists,
         t1_barons,
         t1_elementaldrakes,
         t1_heralds,
         t1_towers,
         t1_inhibitors,
         t1_visionscore,
         t1_minionkills,
         t1_monsterkills,
         t1_monsterkillsenemyjungle,
         t1_xpat15,
         t2_xpat25,
         t1_csdiffat15,
         t1_result,
         t1_killsat15
         )
#Creating new Variables
LOL_df <- LOL_df %>%
  mutate(KDA = (t1_kills+t1_assists)/t1_deaths,
         CS = t1_minionkills+t1_monsterkills,
         AvgLevelDiff_15 = (t1_xpat15/5)-(t2_xpat25/5))

#Renaming Variables
LOL_df <- LOL_df%>%
  rename("Gold" = t1_earnedgold,
         "Dmg" = t1_damagetochampions,
         "Dragon" = t1_elementaldrakes,
         "Herald" = t1_heralds,
         "GoldPerMin"= t1_earned.gpm,
         "GoldDiff_15" = t1_golddiffat15,
         "CounterJungle" = t1_monsterkillsenemyjungle,
         "CSDiff" = t1_csdiffat15,
         "Baron" = t1_barons,
         "Towers" = t1_towers,
         "Inhibitors" = t1_inhibitors,
         "VisionScore" = t1_visionscore,
         "Kills_15" = t1_killsat15,
         "Win" = t1_result
         
         
         
         
         
  )




#Gold Difference Distribution for winning and losing teams
ggplot(data = LOL_df)+
  geom_histogram(aes(x = GoldDiff_15))+
  facet_wrap(~Win)


#Linear Model
lm1 <- lm(Win~I(GoldDiff_15/100), data = LOL_df)
summary(lm1)
plot(lm1)

#Lets looks at the linear fit
ggplot(data = LOL_df, aes(x = GoldDiff_15, y = Win)) + 
  geom_point(color = '#006847', fill = '#006847', size = 3)+
  geom_smooth(method = "lm", color = "#ce1126", se = FALSE) +
  geom_hline(aes(yintercept = 0), lty = 2, lwd=0.5)+
  geom_hline(aes(yintercept = 1), lty = 2, lwd=0.5)+
  scale_x_continuous(name = "Gold Difference at 15 mins", breaks=seq(-15000,15000,2000)) + 
  scale_y_continuous(name = "Win")


#First Logistic Model
logm1 = glm(Win ~ I(GoldDiff_15/100), data=LOL_df, family = 'binomial')
summary(logm1)
confint(logm1)

#Probability of winning
predict(logm1, list(GoldDiff_15 = 3000), type = 'response')

#Save our probability of winning
LOL_df = LOL_df %>% 
  mutate(`Win%` = predict(logm1, type = 'response'))

#Lets look at the Logistic fit
ggplot(data = LOL_df, aes(x = GoldDiff_15, y = Win)) + 
  geom_point(color = "#006847", fill = "#006847", pch=21, size = 3) + 
  geom_line(aes(x = GoldDiff_15, y = `Win%`), color = "#ce1126", lty = 1) + #This are the fitted probabilities
  geom_hline(aes(yintercept = 0), color="dark grey", lty = 2) +
  geom_hline(aes(yintercept = 1), color="dark grey", lty = 2) +
  scale_x_continuous(name = "Gold Difference at 15 mins", breaks=seq(-15000,15000,2000)) + 
  scale_y_continuous(name = "Chance of Win", breaks=seq(0,1,1))

summary(logm1)

LOL_df$GoldDiff_15[100]
LOL_df$`Win%`[100]

.8334/(1-.8332)

1/6

exp(.056836)

predict(logm1, list(GoldDiff_15 = 2737), type = "response")

#Confusion Matrix
LOL_df = LOL_df %>% 
  mutate(predicted = ifelse(predict(logm1, type="response") >=0.5, 
                            "Win", "Loss"),
         actual = ifelse(Win==1, 
                         "Win", "Loss"))

xtabs(~predicted + actual, data =LOL_df)

(1112+1387)/(1112+394+447+1387)

sum(LOL_df$predicted == LOL_df$actual)/nrow(LOL_df)

sum(LOL_df$actual == "Loss")/ nrow(LOL_df)

xtabs(~predicted + actual, data =LOL_df) %>%
  prop.table(margin=2)


#Logistic Model 2
logm2 = glm(Win ~ I(GoldDiff_15/100)+CSDiff, data=LOL_df, family = 'binomial')
summary(logm2)

LOL_df$CSDiff[100]
LOL_df$`Win%`[100]


predict(logm2, list(GoldDiff_15 = 2737,
                    CSDiff = 65 ), type = "response")

LOL_df = LOL_df %>% 
  mutate(predicted2 = ifelse(predict(logm2, type="response") >=0.5, 
                            "Win", "Loss"),
         actual = ifelse(Win==1, 
                         "Win", "Loss"))

xtabs(~predicted2 + actual, data =LOL_df)

xtabs(~predicted2 + actual, data =LOL_df) %>%
  prop.table(margin=2)


