library(tidyverse)


## Loading the data from Dropbox
s_mobile <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/s_mobile.rds"))

### Split data to train, test and representative dataset
train <- s_mobile[which(s_mobile$training == 1), ]
test <- s_mobile[which(s_mobile$training == 0), ]
repre <- s_mobile[which(s_mobile$representative == 1), ]

### Train Decision tree model using all the variable
tree <- crtree(
  train,
  rvar = "churn",
  evar = c("changer", "changem", "revenue", "mou", "overage", "roam","conference", "months","uniqsubs",
           "custcare" ,"retcalls"  ,"dropvce" , "eqpdays","refurb", "smartphone", "highcreditr",  "mcycle" ,
           "car", "travel", "region" , "occupation"),
  type = "classification",
  lev = "yes",
  nodes = 38,
  cp = 0.0001,
  prior = 0.5
)

## See summary of model
summary(tree, prn = TRUE)

## Cross Validate
cv.crtree(result, fun = auc)

# Predict model
test$crtree_pred <- predict(tree, test)$Prediction

## Evaluate model
confusion <- count(test, churn == 'yes', crtree_pred > 0.5)
accuracy <- test %>% summarise(accuracy = mean(churn == ifelse(crtree_pred > 0.5, 'yes', 'no')))
test %>% summarise(chur = mean(crtree_pred > 0.5))



test <- test %>% mutate(refurb = 'yes')
test$pred <- predict(tree,test)$Prediction
test %>% summarise(chur = mean(pred > 0.5))

test <- test %>% mutate(refurb = 'no')
test$pred <- predict(tree,test)$Prediction
test %>% summarise(chur = mean(pred > 0.5))

