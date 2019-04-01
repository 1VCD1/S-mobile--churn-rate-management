library(tidyverse)
library(gbm)
library(radiant)

## Loading the data from Dropbox
s_mobile <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/s_mobile.rds"))

### split data to train, test and representative dataset
train <- s_mobile[which(s_mobile$training == 1), ]
test <- s_mobile[which(s_mobile$training == 0), ]
repre <- s_mobile[which(s_mobile$representative == 1), ]

### Set seed to control outcome
set.seed(1234)

### Set initial number of tree nodes to 1000
n.trees <- 1000

### Change response vairable to 1,0 to feed in gbm model
train_gbm <- train %>% mutate(churn = ifelse(churn == 'yes',1,0))
test_gbm <- test %>% mutate(churn = ifelse(churn == 'yes',1,0))
for_gbm <- s_mobile[which(s_mobile$representative == 0), ] %>% mutate(churn = ifelse(churn == 'yes',1,0))

### Start train gbm model, feed in all the variable
GB <- gbm(churn ~ changer + changem + mou + overage + roam + conference + months + uniqsubs +
        custcare + retcalls + dropvce + eqpdays + refurb + smartphone + highcreditr + mcycle +
        car + travel + region + occupation + revenue,
data = for_gbm,
distribution = "bernoulli",
n.trees = n.trees,
shrinkage = 0.01,
keep.data = FALSE,
cv.folds = 5,
interaction.depth = 20)

### Tune the number of tree nodes to use
n.trees <- gbm.perf(GB,
                   plot.it = TRUE,
                   oobag.curve = FALSE,
                   overlay = TRUE,
                   'cv')

train$gbm_pred <- predict(GB,
                          train_gbm,
                          n.trees,
                          type="response",
                          single.tree=FALSE)

accuracy <- train %>% summarise(accuracy = mean(churn == ifelse(gbm_pred > 0.5, 'yes', 'no')))
accuracy
train %>% summarise(chur = mean(gbm_pred > 0.5))

### Predict the estimation using gbm with tuned n.trees
test$gbm_pred <- predict(GB,
                      test_gbm,
                      n.trees,
                      type="response",
                      single.tree=FALSE)

# Evaluate model
confusion <- count(test, churn == 'yes', gbm_pred > 0.5)
accuracy <- test %>% summarise(accuracy = mean(churn == ifelse(gbm_pred > 0.5, 'yes', 'no')))
accuracy
test %>% summarise(chur = mean(gbm_pred))


vimp <- summary(GB, plotit = FALSE, n.trees = n.trees)
visualize(vimp, type = "bar", xvar = "var", yvar = "rel.inf")


rep_gbm <- repre %>% mutate(churn = ifelse(churn == 'yes',1,0))
repre$pred <- predict(GB,
                         rep_gbm,
                         n.trees,
                         type="response",
                         single.tree=FALSE)
repre <- repre %>% mutate(weight = pred/(pred + (1 - pred) * (1 - 0.02) / 0.02))
mean(repre$weight)

