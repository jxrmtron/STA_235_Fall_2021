library(fpp2) # we need this just for some data
data(ausbeer)

beer <- ausbeer %>% window(start=1992) # grab a snippet of the data starting with 1992 quarter 1
autoplot(beer)
nobs <- NROW(beer) # get the number of observations in our dataset
nobs
# this next step takes our data that is in "time series" format, and adds a Time variable, and a Season variable
beer <- cbind(Production=beer, Time=(1:nobs), Seas = rep((1:frequency(beer)))) %>% as.data.frame()
beer

glimpse(beer)

# this next step takes the values in season and turns them into dummies
beer2 <- beer %>%mutate(value=1) %>%
  pivot_wider(names_from = Seas, values_from = value, names_prefix='Q', values_fill=0)

acf(beer2$Production)

# does this data have a trend?
ggplot(beer, aes(x=Time, y=Production)) +
  geom_line(size=2) +
  geom_smooth(method='lm',  formula='y ~ x', se=FALSE, size=2)
  
# fit a long term trend using time as the sole predictor
lmtrend <- lm(Production ~ Time, data=beer)
summary(lmtrend)
plot(predict(lmtrend), type='l') # quick dirty plot of our trend. => yep its a line

# create detrended data by subtracting out the fitted trend values 
beer.detrend <- beer$Production - predict(lmtrend)

# lets visualize this detrended data, note the LM fit is now a flat line => no trend
ggplot(mapping=aes(x=1:nobs, y=beer.detrend)) + geom_point() + geom_line() +geom_smooth(method='lm', se=FALSE)

acf(beer.detrend)
 # now we fit our seasonal component using dummies. we need to leave on dummy out to be the baseline. 
lmseason <- lm(beer.detrend ~ Q2+Q3+Q4, data=beer2)
summary(lmseason)
# visualizing the seasonal model
ggplot(mapping=aes(x=1:nobs, y=predict(lmseason)))+geom_point()+geom_line()

# subtracting out seasonal from our detrended model gives deasonalized, detrended data, the irregular/error/residuals
Rt = beer.detrend - predict(lmseason)
# looks pretty plain to me:
ggplot(mapping=aes(x=1:nobs, y=Rt)) + geom_point() + geom_line()
acf(Rt)


# make 4 row plot showing components, first we assemble the dataframe

plotdf <- rbind(data.frame(list(Production=beer$Production, Name="Data", Time=beer$Time)),
  data.frame(list(Production=predict(lmtrend), Name="Trend", Time=beer$Time)),
      data.frame(list(Production=predict(lmseason), Name="Seasonal", Time=beer$Time)),
      data.frame(list(Production=Rt, Name="Irregular", Time=beer$Time)))

# then we facet wrap, changing the order to follow the component steps we used
ggplot(plotdf, aes(x=Time, y=Production)) + geom_point() +
  geom_line()+
  facet_wrap(~factor(Name, levels=c('Data', 'Trend', 'Seasonal', 'Irregular')), ncol=1, scales='free')



# Prediction:
# predict trend component
T.pred <- predict(lmtrend, list(Time=50))
T.pred
# Predict season component
# the 50th period is quarter 2, since 50 divided by 4 has remainder 2
50 %% 4 # equals 2
S.pred <- predict(lmseason, list(Q1=0, Q2=1, Q3=0, Q4=0))
S.pred
# Predict combined
Y.pred <- T.pred + S.pred
Y.pred

# works for future values too
T.pred <- predict(lmtrend, list(Time=77))
# Predict season component

S.pred <- predict(lmseason, list(Q1=1, Q2=0, Q3=0, Q4=0))
# Predict combined
Y.pred <- T.pred + S.pred
Y.pred

# combined model

lmcombo <- lm(Production ~ Time + Q2 + Q3 + Q4, data=beer2)
summary(lmcombo)

# combined model output compared with separate model output
ggplot() +
  # geom_point(data=beer, mapping=aes(x=Time, y=Production)) +
  geom_line(data=beer, mapping=aes(x=Time, y=Production), size=1, col='gray') +
  # geom_point(aes(x=1:nobs, y=predict(lmcombo)), col='blue') + 
  geom_line(aes(x=1:nobs, y=predict(lmcombo)), col='blue', size=1) + 
  # geom_point(aes(x=1:nobs, y=predict(lmtrend)+predict(lmseason)), col='red') + 
  geom_line(aes(x=1:nobs, y=predict(lmtrend)+predict(lmseason)), col='red', size=1)


predict(lmcombo, list(`Time` = 50,
                  `Q2` = 1,
                  `Q3` = 0,
                  `Q4`  = 0), interval = "prediction")






