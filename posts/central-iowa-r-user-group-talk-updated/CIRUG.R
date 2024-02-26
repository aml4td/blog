####################################################################################
### Code for "Applied Predictive Modeling" to the Central Iowa RUG on 1/21/16
### by Max Kuhn

install.packages(c("caret", "pROC"), 
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Imports", "Depends", "Suggests"))



####################################################################################
### Load and split the data
library(caret)
data(segmentationData)
dim(segmentationData)
str(segmentationData[, 1:9])


## remove the cell identifier
segmentationData$Cell <- NULL
seg_train <- subset(segmentationData, Case == "Train")
seg_test  <- subset(segmentationData, Case == "Test")

seg_train$Case <- NULL
seg_test$Case <- NULL


## ## NOT executed...
## ## make a balanced random split
## in_train <- createDataPartition(segmentationData$Class, p = 0.5, list = FALSE)
## 
## ## `in_train` is set of row indices that are selected to go
## ## into the training set
## train_data <- segmentationData[ in_train,]
## test_data  <- segmentationData[-in_train,]
####################################################################################
### LDA Model

library(MASS)
lda_fit <- lda(Class ~ ., data = seg_train, tol = 1.0e-15)

predict(lda_fit, newdata = seg_train[1:3,])

lda_test_pred <- predict(lda_fit, newdata = seg_test)
library(pROC)

lda_roc <- roc(response = seg_test$Class,
               predictor = lda_test_pred$posterior[, "PS"],
               ## we need to tell the function that the _first_ level
               ## is our event of interest
               levels = rev(levels(seg_test$Class)))
lda_roc

plot(lda_roc, print.thres = .5)

confusionMatrix(data = lda_test_pred$class, reference = seg_test$Class)

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

set.seed(20792)
lda_mod <- train(Class ~ ., data = seg_train,
                 method = "lda",
                 ## Add the metric argument
                 trControl = ctrl, metric =  "ROC",
                 ## Also pass in options to `lda` using `...`
                 tol = 1.0e-15)

## library(doMC)          # on unix, linux or OS X
## ## library(doParallel) # windows and others
## registerDoMC(cores = 2)

lda_mod

####################################################################################
### KNN model

## The same resamples are used 
set.seed(20792)
knn_mod <- train(Class ~ ., data = seg_train,
                 method = "knn",
                 trControl = ctrl,
                 ## tuning parameter values to evaluate
                 tuneGrid = data.frame(k = seq(1, 25, by = 2)),
                 preProc = c("center", "scale"),
                 metric =  "ROC")

knn_mod

ggplot(knn_mod) 

## to get the classes:
predict(knn_mod, newdata = head(seg_test))

## We choose `prob` to get class probabilities:
predict(knn_mod, newdata = head(seg_test), type = "prob")

## The same resamples are used 
set.seed(20792)
knn_yj_mod <- train(Class ~ ., data = seg_train,
                     method = "knn",
                     trControl = ctrl,
                     tuneGrid = data.frame(k = seq(1, 25, by = 2)),
                     preProc = c("center", "scale", "YeoJohnson"),
                     metric =  "ROC")

## What was the best area under the ROC curve?
getTrainPerf(knn_yj_mod)

## Conduct o a paired t-test on the resampled AUC values to control for 
## resample-to-resample variability:
compare_models(knn_yj_mod, knn_mod, metric = "ROC")
## Yes, but not by much

