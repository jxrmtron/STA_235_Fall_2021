library(tidyverse)
library(estimatr)
library(modelr)
library(caret)


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
         "Win" = t1_result,
         "Kills" = t1_kills
         
         
         
         
         
  )




set.seed(123)

n = nrow(LOL)

frac = 0.8
train = sample(1:n, round(n*frac))

train.data = LOL_df[train, ] 
test.data = LOL_df[-train, ] 

lm_simple = lm(Gold ~ GoldDiff_15 + VisionScore, data = train.data)

summary(lm_simple)

lm_complex <- lm(Gold ~ GoldDiff_15 + VisionScore + Baron + Dragon + Dmg, data = train.data)

summary(lm_complex)



rmse(lm_simple, test.data)


rmse(lm_complex, test.data)


AIC(lm_simple)
AIC(lm_complex)




train.control = trainControl(method = "cv", number = 10) 

lm_simple_cv = train(Gold ~ GoldDiff_15 + VisionScore, data = LOL_df, method="lm",
                     trControl = train.control) 




lm_simple_cv
rmse(lm_simple_cv, test.data)

lm_complex_cv = train(Gold ~ GoldDiff_15 + VisionScore + Baron + Dragon + Dmg, data = LOL_df, method="lm",
                      trControl = train.control) 

lm_complex_cv

rmse(lm_complex_cv, test.data)

AIC(lm_simple_cv)
AIC(lm_complex_cv)






fullmodel <- glm(Win ~ GoldDiff_15 + VisionScore + Baron + Dragon + Dmg + CounterJungle + Inhibitors + Towers + Kills , data = LOL_df, family = `binomial`)

summary(fullmodel)

nullmodel= glm(Win ~ 1, family=binomial, data=LOL_df)

summary(nullmodel)

table(LOL_df$Win)

exp(.13313)/(1+exp(.13313))

1781/3340

backwards = step(fullmodel)

summary(backwards)

forwards = step(nullmodel, scope=list(lower=formula(nullmodel), upper=formula(fullmodel)), direction="forward")

summary(forwards)

set.seed(2021)

n = length(LOL_df$Win)


Train_index = sample(1:n, size=0.5*n, replace=FALSE) #choose 50% at random for training
Test_index = -Train_index
Train_data = LOL_df[Train_index,]
Test_data = LOL_df[Test_index,]

full_train <- glm(Win ~ VisionScore + Dragon + Dmg + CounterJungle + Towers + Kills , data = Train_data, family = `binomial`)


train_accuracy = mean((predict(full_train, type="response") > 0.5) == (Train_data$Win))
table((predict(full_train, type="response")>0.5), Train_data$Win)

# test_accuracy:
predict_test = (predict(full_train, Test_data, type="response") > 0.5)
test_accuracy = mean(predict_test == Test_data$Win)
table(predict_test, Test_data$Win)

backward_train = step(full_train)
summary(backward_train )

# backward_train_accuracy:
train_accuracy_backward = mean((predict(backward_train, type="response") > 0.5) == (Train_data$Win))
table((predict(backward_train, type="response") > 0.5), (Train_data$Win))

# backward_test_accuracy:
predict_test = (predict(backward_train, Test_data, type="response")>0.5)
test_accuracy_backward = mean(predict_test == Test_data$Win)
table(predict_test, Test_data$Win)

train_accuracy
test_accuracy
train_accuracy_backward
test_accuracy_backward

