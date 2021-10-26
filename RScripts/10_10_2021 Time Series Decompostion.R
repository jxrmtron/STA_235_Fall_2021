library(tidyverse)
library(ggplot2)
library(PerformanceAnalytics)
library(quantmod)
library(zoo)

options(scipen = 999)
options(digits = 2)

#Grabbing Apple Stock Data
getSymbols(Symbols = c("LNG"), from="2010-01-03", to="2021-01-03",
           auto.assign=TRUE, warnings=FALSE, periodicity = 'monthly')

#Converting TS object to a Data Frame
LNG1<- as.data.frame(`LNG`)

LNG1 <- LNG1 %>%
  mutate( returns = monthlyReturn(LNG) )

#Condensing Data Frame
LNG1 <- LNG1 %>%
  select(returns
         )

#Creating Month Variable
LNG1 <- LNG1 %>% mutate(Season = strftime(row.names(LNG1),format = "%m"))
LNG1$Season[LNG1$Season=="03"] <- "Spring"
LNG1$Season[LNG1$Season=="04"] <- "Spring"
LNG1$Season[LNG1$Season=="05"] <- "Spring"
LNG1$Season[LNG1$Season=="06"] <- "Summer"
LNG1$Season[LNG1$Season=="07"] <- "Summer"
LNG1$Season[LNG1$Season=="08"] <- "Summer"
LNG1$Season[LNG1$Season=="09"] <- "Fall"
LNG1$Season[LNG1$Season==10] <- "Fall"
LNG1$Season[LNG1$Season==11] <- "Fall"
LNG1$Season[LNG1$Season==12] <- "Winter"
LNG1$Season[LNG1$Season=="01"] <- "Winter"
LNG1$Season[LNG1$Season=="02"] <- "Winter"

LNG1$Season <- as.factor(LNG1$Season)


#Number of Rows
nrow <- NROW(LNG1)

#Time Variable
LNG1 <- LNG1 %>%
  mutate( Time = (1:nrow) )



ggplot(LNG1, aes(x=Time, y=returns)) + geom_line() +  
  scale_x_continuous(breaks = seq(0, 144, by = 12))

#Making model for the trend component
lmtrend <- lm(returns ~ Time, data=LNG1)
summary(lmtrend)

residuals(lmtrend)[49]

plot(predict(lmtrend), type='l')

#Creating detrended data
NG_F.detrend <-  LNG1$returns - predict(lmtrend)


ggplot(mapping=aes(x=1:nrow, y=NG_F.detrend)) + geom_point() + geom_line() +geom_smooth(method='lm', se=FALSE)



#Making model for seasonal component
lmseason <- lm(NG_F.detrend ~ Season, data=LNG1)

summary(lmseason)

residuals(lmseason)[16]

lmseason$Season <- as.factor(lmseason$Season)

ggplot(mapping=aes(x=1:nrow, y=predict(lmseason)))+geom_line() + 
  scale_x_continuous(breaks = seq(0, 144, by = 12))

#Irregulars

Rt = NG_F.detrend - predict(lmseason)

ggplot(mapping=aes(x=1:nrow, y=Rt)) + geom_line()

acf(Rt)



ggplot(LNG1, aes(x=Time, y=returns)) + geom_line() +  
  scale_x_continuous(breaks = seq(0, 144, by = 12))

ggplot(LNG1, aes(x=Time, y=predict(lmtrend))) + geom_line() +
  scale_x_continuous(breaks = seq(0, 144, by = 12))

ggplot(LNG1, aes(x=Time, y=predict(lmseason)))+geom_line() +
  scale_x_continuous(breaks = seq(0, 144, by = 12))

ggplot(LNG1, aes(x=Time, y=Rt)) + geom_line()+
  scale_x_continuous(breaks = seq(0, 144, by = 12))


T.pred <- predict(lmtrend, list(Time=120))

T.pred

S.pred <- predict(lmseason, list(Season="Winter"))

S.pred

Y.pred <- T.pred + S.pred 
Y.pred

 #Full Model
lmcombo <- lm(returns ~ Time + Season, data=LNG1)
summary(lmcombo)

predict(lmcombo, list(`Time` = 120,
                      Season= "Winter"), interval = "prediction")


ggplot() +
  geom_line(data=LNG1, mapping=aes(x=Time, y=returns), size=1, col='gray') +
  geom_line(aes(x=1:nrow, y=predict(lmcombo)), col='blue', size=1) + 
  geom_line(aes(x=1:nrow, y=predict(lmtrend)+predict(lmseason)), col='red', size=1)




