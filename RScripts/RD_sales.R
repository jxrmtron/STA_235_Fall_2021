#################################################
### Week 11 - Regression discontinuity design ###
#################################################

library(tidyverse)
library(mosaic)
theme_set(theme_minimal())
options(scipen = 999)

# sales = read.csv("...sales.csv")
glimpse(sales)

      # id: identification for each person
      # time: time of arrival, since the store opened
      # age: age of the person
      # female: whether the customer is female or not
      # income: average income for the customer
      # sales: amount of the purchase
      # treat: whether the person received a discount or not 
               # (e.g. was within the first 1,000 customers)

# Plot the running variable on the x-axis and treatment variable on the y-axis.
   # What is the running variable? Can you obtain the cutoff for treatment assignment?
ggplot(data = sales, aes(x = time, y = factor(treat))) +
   geom_point() 

# Identify your cutoff score (c)
   # filter to include only treated units  >> select only the time variable >> 
   # get the max value for that variable >> transform it to a vector (pull())
c = sales %>% 
   filter(treat==1) %>% 
      select(time) %>% 
         summarize_all(max) %>% 
            pull()

# we could also look at the stats for time by treatment group:
favstats(sales$time ~ sales$treat) 

# Add to previous plot a fitted smooth line for each side of the cutoff 
# add random jitter so you can see more of the data clustered around the same value (1 or 0)
# use filter() in geom_smooth to color units below and above the cutoff
ggplot(data = sales, aes(x = time, y = factor(treat))) +
   geom_jitter(data = filter(sales, time<=c), size = 2, pch = 21, color = "white", fill="grey", height = 0.2) + 
   geom_jitter(data = filter(sales, time>c), size = 2, pch = 21, color = "white", fill="grey", height = 0.2)  +
   geom_smooth(data = filter(sales, time>c), method = "lm", color = "#F89441", se=TRUE, lwd=1.5) +
   geom_smooth(data = filter(sales, time<=c), method = "lm", color = "#900DA4", se=TRUE, lwd=1.5)
   

# create a distance variable (distance to the cutoff) 
# this is the RUNNING /FORCING variable in our RD design
sales = sales %>% 
   mutate(dist = time-c) 

# fit a linear model, allowing for different intercept and slopes 
# for the two groups by including an interaction.
lm1 = lm(sales ~ dist * treat, data = sales) 

# estimate the predictions of sales (fitted values) based on our model and add to data frame:
sales = sales %>%  
   mutate(fitted = predict(lm1))

ggplot(data = sales, aes(x = dist, y = sales)) +
   geom_point(size = 2, pch = 21, color = "white", fill="grey") +
   geom_line(data = filter(sales, time>c), aes(x = dist, y = fitted), color = "#F89441", lwd = 2) +
   geom_line(data = filter(sales, time<=c), aes(x = dist, y = fitted), color = "#900DA4", lwd = 2)

# RD model results
confint(lm1) %>%  round(3)
summary(lm1)

### FYI: alternative regression table summary in the Modern Dive library
### get confidence intervals along with p-values!
install.packages("moderndive")
library(moderndive)
get_regression_table(lm1)



########################################
########################################
# BONUS ANALYSIS
# RD designs do not require a LINEAR fit.
# Let's fit a quadratic model:

lm2 = lm(sales ~ dist + I(dist^2) + treat + dist*treat + treat*I(dist^2), data = sales) 

   # we need to include both the linear and quadratic term of the running variable
   # (as a main effect and interaction term)

sales = sales %>%  
   mutate(fitted2 = predict(lm2))

ggplot(data = sales, aes(x = dist, y = sales)) +
   geom_point(size = 3, pch = 21, color = "white", fill="grey") +
   geom_line(data = filter(sales, time>c), aes(x = dist, y = fitted2), color = "#F89441", lwd = 2) +
   geom_line(data = filter(sales, time<=c), aes(x = dist, y = fitted2), color = "#900DA4", lwd = 2) +
   theme_bw()

summary(lm2)
confint(lm2) %>% round(4) 


##################################
### BONUS --- rdrobust package ###

library(rdrobust)

# RD plot function
rdplot(y = sales$sales, x = sales$time, c = c, title = "RD plot",
       x.label = "time", y.label = "sales", col.dots = "#F89441", col.lines = "#900DA4")

# y: outcome variable
# x: running variable
# c: cutoff
# col.dots: color for the dots
# col.lines: color for the lines
