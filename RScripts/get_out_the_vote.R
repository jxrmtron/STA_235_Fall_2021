      #######################################
      #### Week 10                       ####
      #### Analysis of Experimental Data ####
      #######################################

library(tidyverse)
library(mosaic)
options(scipen = 999)

      ## Get Out the Vote voter mobilization experiment
      # GOTV = read.csv("...GOTV.csv")
glimpse(GOTV)

      # persons: Number of voters in the household (1 or 2)
      # competiv: Whether the district is competitive or not
      # vote*: Whether the person voted in year *
      # newreg: Whether the person is newly registered as a voter or not
      # age: Age of the person
      # contact: Whether the household was contacted (1) or not (0)
      # treat: Whether the household was randomized into treatment (1) or not (0). If NA, the household was not part of the randomization sample.
      # state: Michigan or Ohio
      # female2: Whether the person is female or not.

# Drop variables with unlisted phone numbers
GOTV = GOTV %>% 
   filter(!is.na(treatment))


#### How to analyze the results of an experiment? ####

# identify Treatment variable and Outcome variable
# treatment variable
xtabs(~GOTV$treatment)

ggplot(data = GOTV, mapping = aes(x=treatment)) +
   geom_bar(fill = 'navyblue') 
   
# Outcome variable
xtabs(~GOTV$vote02)

ggplot(data = GOTV, mapping = aes(x=vote02)) +
   geom_bar(fill = 'darkred') 

# does there seem to be an association between treatment and outcome?
xtabs(~GOTV$vote02 + GOTV$treatment) %>%  
   prop.table(margin=2) %>% 
   round(3)

ggplot(data = GOTV, mapping = aes(x=factor(vote02))) +
   geom_bar() +
   facet_wrap(~treatment) + theme(legend.position = 'none')


         #######################
         ###  SIMPLE DESIGN  ###
         ###  no blocking    ###
         #######################

### estimating the average treatment effect ###
prop(vote02 ~ treatment, data = GOTV) %>%  round(2)
diffprop(vote02 ~ treatment, data = GOTV) %>%  round(2)

   # - 59% voting rate for households that received a call
   # - 55% voting rate for households in control group
   # - difference in proportions is 4%

                  # THE GOTV DATA SET IS TOO LARGE TO BOOSTRAP DURING CLASS
                  # BUT YOU COULD USE THIS APPROACH ON HOMEWORK 10
      
                  # # convey uncertainty with a confidence interval 
                  # for difference in proportions via bootstrapping 
                  # vote_boot = do(1000) * diffprop(vote02 ~ treatment, data = resample(GOTV))
                  # 
                  # # bootstrapped sampling distribution
                  # ggplot(vote_boot) + 
                  #    geom_histogram(aes(x=diffprop), color = 'white', fill = 'navyblue') 
                  # 
                  # # confidence interval
                  # confint(vote_boot) %>% 
                  #    select(-level, -method) %>% 
                  #    mutate_if(is.numeric, round, digits=3)

# or use prop.test()
prop.test(vote02 ~ treatment, data = GOTV)

# note sequencing of groups in calculating the difference of proportions
1-prop(vote02 ~ treatment, data = GOTV) %>%  round(2)

         # propTreatment - propControl OR 
         # propControl - propTreatment
   # both are correct for calculating the difference in group proportions, 
   # but must be clear in reporting results to attribute group differences


# what effect do we see with a simple linear baseline/offset model
lm_vote = lm(vote02 ~ treatment, data=GOTV)
confint(lm_vote) %>%  round(3)
summary(lm_vote)

# we know now that a simple linear model with a binary outcome 
# variable does not meet LM assumptions
# logistic regression model with glm()

glm_vote = glm(vote02 ~ treatment, data = GOTV, family = 'binomial')
summary(glm_vote)  # coefficients in log odds

# confidence interval in terms of odds
100 * (confint(glm_vote) %>% 
   exp() %>% 
   round(3)) %>% 
   -1

# interval represents expected % change in odds of voting from treatment 

# predicted probability of voting given treatment?
predict(glm_vote, list(treatment="treatment"), type = 'response') 


      #######################
      ### BLOCKING DESIGN ###
      #######################

# the structure of our data set needs to match the experimental design 
# blocking variables were competiv and state --> 4 strata
xtabs(~GOTV$state)
xtabs(~GOTV$competiv)

      # mutate for readable levels of categorical variables
      GOTV = GOTV %>%  
         mutate(state = ifelse(state == 0, "Michigan", "Iowa"),
                competiv = ifelse(competiv == 1, "uncomp", "comp"))

xtabs(~GOTV$state + GOTV$competiv)

# distribution of sample by state
ggplot(data = GOTV, mapping = aes(x=state)) +
   geom_bar(fill = 'darkred')

# distribution of sample by competitive / non-competitive district
ggplot(data = GOTV, mapping = aes(x=competiv)) +
   geom_bar(fill = 'navyblue')

# generate a variable to represent the four block groups
# use the interaction() function to group by both variables
GOTV = GOTV %>%  
   mutate(block = interaction(state, competiv))
glimpse(GOTV)

# distribution of sample by block (State / Competitive district)
ggplot(data = GOTV, mapping = aes(x=block)) +
   geom_bar() 

### estimating the average treatment effect with logistic regression ###
glm_block = glm(vote02 ~ treatment + block, family = 'binomial', data = GOTV)
summary(glm_block)

# confidence interval in terms of odds
# interval represents expected % change in odds of voting from treatment 
odds_interval = confint(glm_block) %>%  exp() %>% -1 %>%  round(3)
odds_interval * 100

         #### predicted probabilities of voting given treatment?
         # predict(glm_block, list(treatment='treatment', block='Michigan.comp'), type = 'response')
         # predict(glm_block, list(treatment='treatment', block='Michigan.uncomp'), type = 'response')
         # predict(glm_block, list(treatment='treatment', block='Iowa.comp'), type = 'response')
         # predict(glm_block, list(treatment='treatment', block='Iowa.uncomp'), type = 'response')

#####################################
# logistic regression with covariates (i.e., additional predictors) 
# Does our estimate of the treatment effect change? 
glm_block2 = glm(vote02 ~ treatment + block + female + age + persons + newreg + vote98 + vote00, 
                 family = 'binomial', data = GOTV)
summary(glm_block2)


# Note that not every person that was assigned to treatment was actually contacted.
xtabs(~GOTV$treatment + GOTV$contact)

# What if instead of using treatment assignment we regressed vote02 on `contact`? 
# How does your estimate change?
glm_contact = glm(vote02 ~ contact + block + female + age + persons + newreg + vote98 + vote00, 
                  family = 'binomial', data = GOTV)
summary(glm_contact)

