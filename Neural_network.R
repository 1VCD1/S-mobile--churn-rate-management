library(tidyverse)
library(gbm)
library(radiant)
library(nnet)
library(caret)

## Load data
s_mobile <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/s_mobile.rds"))

### split data to train, test and representative dataset
train <- s_mobile[which(s_mobile$training == 1), ]
test <- s_mobile[which(s_mobile$training == 0), ]
repre <- s_mobile[which(s_mobile$representative == 1), ]

### Set evar, rvar, lev for further usage
evar <- c("changer", "changem", "revenue", "mou", "overage", "roam","conference", "months","uniqsubs",
          "custcare" ,"retcalls"  ,"dropvce" , "eqpdays","refurb", "smartphone", "highcreditr",  "mcycle" ,
          "car", "travel", "region" , "occupation")
rvar <- "churn"
lev <- "yes"

## standardize data for use with the nnet package
df_train_scaled <- train %>%
  scaledf(sf = 2)
str(df_train_scaled)

df_test_scaled <- test %>%
  scaledf(sf = 2)

s_mobile_scaled <- s_mobile %>%
  copy_attr(df_train_scaled, c("radiant_ms","radiant_sds")) %>%
  scaledf(calc = FALSE)

##Grid search
grid <- expand.grid(size = 1:6, decay = seq(0, 1, 0.05))
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  verboseIter = TRUE
)

# tuning model
set.seed(1234)
result <- train(
  select(df_train_scaled, evar),
  df_train_scaled[[rvar]],
  method = "nnet",
  trControl = ctrl,
  tuneGrid = grid,
  rang = .1,
  skip = FALSE,
  linout = FALSE,
  trace = FALSE,
  maxit = 10000
)

## extract the uncovered tuning parameters
tuned <- result$bestTune
tuned

## Rerun to train the model based on best tune
set.seed(1234)
model <- nnet::nnet(
  churn == lev ~ changer + changem + revenue + mou + overage + roam + conference + months + uniqsubs +
    custcare + retcalls + dropvce + eqpdays + refurb + smartphone + highcreditr + mcycle +
    car + travel + region + occupation,
  data = df_train_scaled,
  size = 6,
  decay = 0.4,
  rang = .1,
  linout = FALSE,
  entropy = TRUE,
  skip = FALSE,
  trace = FALSE,
  maxit = 10000
)

# Predict
test$nnc <- predict(model, df_test_scaled)

# Evaluate model
confusion <- count(test, churn == 'yes', nnc > 0.5)
accuracy <- test %>% summarise(accuracy = mean(churn == ifelse(nnc > 0.5, 'yes', 'no')))
accuracy
