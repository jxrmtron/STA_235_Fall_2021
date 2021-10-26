library(tidyverse)


# easy way to do a histogram
hist(LOL$blueTotalGold)

ggplot(data=LOL) +
  geom_histogram(aes(x=blueTotalGold)) 
 
# load mosaic for some looping tools
library(mosaic)

# save results of runs 
Gold_ave <- do(1000)*mean(~blueTotalGold, data=resample(LOL,2))

ggplot(data=Gold_ave) +
  geom_histogram(aes(x=mean)) 

Gold_ave1 <- do(1000)*mean(~blueTotalGold, data=resample(LOL,5))

ggplot(data=Gold_ave1) +
  geom_histogram(aes(x=mean))



Gold_ave2 <- do(1000)*mean(~blueTotalGold, data=resample(LOL,10))

ggplot(data=Gold_ave2) +
  geom_histogram(aes(x=mean))

Gold_ave3 <- do(1000)*mean(~blueTotalGold, data=resample(LOL,30))

ggplot(data=Gold_ave3) +
  geom_histogram(aes(x=mean))

