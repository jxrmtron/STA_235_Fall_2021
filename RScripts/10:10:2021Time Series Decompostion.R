library(tidyverse)
library(ggplot2)
library(PerformanceAnalytics)
library(quantmod)
library(zoo)

options(scipen = 999)

#Grabbing Apple Stock Data
getSymbols(Symbols = c("AAPL"), from="2010-01-03", to="2021-01-03",
           auto.assign=TRUE, warnings=FALSE, periodicity = 'monthly')

#Converting TS object to a Data Frame
AAPL1<- as.data.frame(AAPL)

#Calculating Returns
AAPL1 <- AAPL1 %>%
  mutate( returns = monthlyReturn(AAPL) )

#Condensing Data Frame
AAPL1 <- AAPL1 %>%
  select(returns
         )
#Creating Month Variable
AAPL1 <- AAPL1 %>% mutate(month = strftime(row.names(AAPL1),format = "%m"))

#Number of Rows
nrow <- NROW(AAPL1)

#Time Variable
AAPL1 <- AAPL1 %>%
  mutate( Time = (1:nrow) )



plot(AAPL1$returns)  

#Making model for the trend component
lmtrend <- lm(returns ~ Time, data=AAPL1)
summary(lmtrend)

plot(predict(lmtrend), type='l')

#Creating detrended data
AAPL.detrend <-  AAPL1$returns - predict(lmtrend)


ggplot(mapping=aes(x=1:nrow, y=AAPL.detrend)) + geom_point() + geom_line() +geom_smooth(method='lm', se=FALSE)

AAPL1$month <- as.factor(AAPL1$month)

#Making model for seasonal component
lmseason <- lm(AAPL.detrend ~ month, data=AAPL1)

summary(lmseason)

lmseason$month <- as.factor(lmseason$month)

ggplot(mapping=aes(x=1:nrow, y=predict(lmseason)))+geom_point()+geom_line()

#Irregulars

Rt = AAPL.detrend - predict(lmseason)

ggplot(mapping=aes(x=1:nrow, y=Rt)) + geom_point() + geom_line()

acf(Rt)



ggplot(AAPL1, aes(x=Time, y=returns)) + geom_line()

ggplot(AAPL1, aes(x=Time, y=predict(lmtrend))) + geom_line()

ggplot(AAPL1, aes(x=Time, y=predict(lmseason)))+geom_line()

ggplot(AAPL1, aes(x=Time, y=Rt)) + geom_line()


#Prediction
T.pred <- predict(lmtrend, list(Time=200))

S.pred <- predict(lmseason, list(month="10"))

Y.pred <- T.pred + S.pred
Y.pred

 #Full Model
lmcombo <- lm(returns ~ Time + month, data=AAPL1)
summary(lmcombo)


predict(lmcombo, list(`Time` = 200,
                      month= "10"), interval = "prediction")


ggplot() +
  geom_line(data=AAPL1, mapping=aes(x=Time, y=returns), size=1, col='gray') +
  geom_line(aes(x=1:nrow, y=predict(lmcombo)), col='blue', size=1) + 
  geom_line(aes(x=1:nrow, y=predict(lmtrend)+predict(lmseason)), col='red', size=1)


