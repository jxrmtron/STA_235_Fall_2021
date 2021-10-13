
library(tidyverse)
library(ggplot2)
library(PerformanceAnalytics)
library(quantmod)
library(zoo)
library(tidyquant)

options(scipen = 999)


#Clean up date Data
LOL_DF <- LOL %>%
  mutate(Date = str_sub(date, 1,10))

LOL_DF <- LOL_DF %>%
  select(Date,t1_earnedgold)

LOL_DF$Date <- as.Date(LOL_DF$Date) 

#Aggregate daily gold earnings
LOL_DF <- LOL_DF %>%
  group_by(Date) %>% 
  summarise(DailyGoldEarned=sum(t1_earnedgold)) %>% 
  na.omit()



#Save as TS
LOL_TS <- xts(LOL_DF$DailyGoldEarned, LOL_DF$Date)

plot(LOL_TS)

#Look at Data Lags
nrow_LOL <- NROW(LOL_DF)

LOL_DF <- LOL_DF %>%
  mutate( Time = 1:nrow_LOL)

ggplot(LOL_DF, aes(x=Time, y=DailyGoldEarned)) +
  geom_line(color = "gray65")+
  geom_point(size = 1.5, color = "#00674b")

ggplot(LOL_DF, aes(x=lag(DailyGoldEarned), y=DailyGoldEarned)) +
  geom_line(color = "gray65")+
  geom_point(size = 1.5, color = "#00674b")

LOL_DF <- LOL_DF %>%
  mutate(lagDailyGoldEarned = lag(DailyGoldEarned))


#Build Autoregression model for Random Sample

LOL_LM <- lm(DailyGoldEarned~lagDailyGoldEarned, data = LOL_DF)

summary(LOL_LM)

plot(LOL_LM)

acf(residuals(LOL_LM))

mean(LOL_DF$DailyGoldEarned)
sd(LOL_DF$DailyGoldEarned)




predict(LOL_LM, list(lagDailyGoldEarned=491806), interval = "predict")



#Random Walk


getSymbols(Symbols = c("TSLA"), from="2020-01-03", to="2021-01-03",
           auto.assign=TRUE, warnings=FALSE, periodicity = 'daily')

TSLA_DF<- as.data.frame(`TSLA`)

TSLA_DF <- TSLA_DF %>%
  rename("Price" = TSLA.Adjusted)


TSLA_DF <- TSLA_DF %>%
  select(Price
  )

TSLA_DF <- TSLA_DF %>%
  mutate(lagPrice = lag(Price),
         intraday = Price-lagPrice)

#Does Tn-1 predict T1
ggplot(TSLA_DF, aes(x=Time, y=Price)) +
  geom_line(color = "gray65")+
  geom_point(size = 1.5, color = "#00674b")

ggplot(TSLA_DF, aes(x=lagPrice, y=Price)) +
  geom_line(color = "gray65")+
  geom_point(size = 1.5, color = "#00674b")



TSLA_LM <- lm(Price~lagPrice, data = TSLA_DF)

summary(TSLA_LM)

plot(TSLA_LM)
acf(residuals(TSLA_LM))

#Geometric Random Walk

logTSLA_LM <- lm(log(Price)~log(lagPrice), data = TSLA_DF)

summary(logTSLA_LM)
plot(logTSLA_LM)
acf(residuals(logTSLA_LM))

plot(TSLA_DF$intraday)

naTSLA_DF <- na.omit(TSLA_DF)
acf(naTSLA_DF$intraday)


mean(naTSLA_DF$intraday)

sd(naTSLA_DF$intraday)

TSLA_Pred <- 705.670 + mean(naTSLA_DF$intraday)

TSLA_Pred

TSLA_Pred + sd(naTSLA_DF$intraday)

TSLA_Pred - sd(naTSLA_DF$intraday)

predict(TSLA_LM, list(lagPrice=705.670), interval = "predict")


#Market Beta

getSymbols(Symbols = c(""), from="2016-01-01", to="2021-01-01",
           auto.assign=TRUE, warnings=FALSE, periodicity = 'monthly')

getSymbols(Symbols = c("^GSPC"), from="2016-01-01", to="2021-01-01",
           auto.assign=TRUE, warnings=FALSE, periodicity = 'monthly')

_DF<- as.data.frame(``)

_DF <- _DF %>%
  mutate( returns = monthlyReturn() )


_DF <- _DF %>%
  mutate( SPreturns = monthlyReturn(GSPC) )

Beta_Model <- lm(returns~SPreturns, data = _DF)

summary(Beta_Model)




 