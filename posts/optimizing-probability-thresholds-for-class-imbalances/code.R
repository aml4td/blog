## Code for 'Optimizing Probability Thresholds for Class Imbalances' on the 
## Applied Predictive Modeling blog at:
## http://appliedpredictivemodeling.com/blog/
## by Max Kuhn
## There is no warrenty on this code!

modelInfo <- list(label = "rf",
                  library = c("randomForest"),
                  type = c("Classification"),
                  parameters  = data.frame(parameter = c("mtry", "threshold"),
                                           class = c("numeric", "numeric"),
                                           label = c("#Randomly Selected Predictors",
                                                     "Probability Cutoff")),
                  grid = function(x, y, len = NULL) {
                    p <- ncol(x)
                    expand.grid(mtry = floor(sqrt(p)), 
                                threshold = seq(.01, .99, length = len))
                  },
                  loop = function(grid) {   
                    library(plyr)
                    loop <- ddply(grid, c("mtry"),
                                  function(x) c(threshold = max(x$threshold)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$threshold)) {
                      index <- which(grid$mtry == loop$mtry[i])
                      cuts <- grid[index, "threshold"] 
                      submodels[[i]] <- data.frame(threshold = cuts[cuts != loop$threshold[i]])
                    }    
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(length(levels(y)) != 2)
                      stop("This works only for 2-class problems")
                    randomForest(x, y, mtry = param$mtry, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    class1Prob <- predict(modelFit, 
                                          newdata, 
                                          type = "prob")[, modelFit$obsLevels[1]]
                    ## Raise the threshold for class #1 and a higher level of
                    ## evidence is needed to call it class 1 so it should 
                    ## decrease sensitivity and increase specificity
                    out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                                  modelFit$obsLevels[1], 
                                  modelFit$obsLevels[2])
                    if(!is.null(submodels))
                    {
                      tmp2 <- out
                      out <- vector(mode = "list", length = length(submodels$threshold))
                      out[[1]] <- tmp2
                      for(i in seq(along = submodels$threshold)) {
                        out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                                             modelFit$obsLevels[1], 
                                             modelFit$obsLevels[2])
                      }
                    } 
                    out  
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- as.data.frame(predict(modelFit, newdata, type = "prob"))
                    if(!is.null(submodels))
                    {
                      probs <- out
                      out <- vector(mode = "list", length = length(submodels$threshold)+1)
                      out <- lapply(out, function(x) probs)
                    } 
                    out 
                  },
                  predictors = function(x, ...) {
                    ## After doing some testing, it looks like randomForest
                    ## will only try to split on plain main effects (instead
                    ## of interactions or terms like I(x^2).
                    varIndex <- as.numeric(names(table(x$forest$bestvar)))
                    varIndex <- varIndex[varIndex > 0]
                    varsUsed <- names(x$forest$ncat)[varIndex]
                    varsUsed
                  },
                  varImp = function(object, ...){
                    varImp <- randomForest::importance(object, ...)
                    if(object$type == "regression")
                      varImp <- data.frame(Overall = varImp[,"%IncMSE"])
                    else {
                      retainNames <- levels(object$y)
                      if(all(retainNames %in% colnames(varImp))) {
                        varImp <- varImp[, retainNames]
                      } else {
                        varImp <- data.frame(Overall = varImp[,1])
                      }
                    }
                    
                    out <- as.data.frame(varImp)
                    if(dim(out)[2] == 2) {
                      tmp <- apply(out, 1, mean)
                      out[,1] <- out[,2] <- tmp  
                    }
                    out
                  },
                  levels = function(x) x$classes,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),])

library(caret)

set.seed(442)
training <- twoClassSim(n = 1000, intercept = -16)
testing <- twoClassSim(n = 1000, intercept = -16)

table(training$Class)

set.seed(949)
mod0 <- train(Class ~ ., data = training,
              method = "rf",
              metric = "ROC",
              tuneGrid = data.frame(mtry = 3),
              trControl = trainControl(method = "cv",
                                       classProbs = TRUE,
                                       summaryFunction = twoClassSummary))
getTrainPerf(mod0)

png("roc.png", width = 320, height = 320)
roc0 <- roc(testing$Class, 
            predict(mod0, testing, type = "prob")[,1], 
            levels = rev(levels(testing$Class)))
closest0 <- coords(roc0, x = "best", ret="threshold",
                   best.method="closest.topleft") 
plot(roc0, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, 
     legacy.axes = TRUE)
dev.off()

fourStats <- function (data, lev = levels(data$obs), model = NULL) {
  ## This code will get use the area under the ROC curve and the
  ## sensitivity and specificity values using the current candidate
  ## value of the probability threshold.
  out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
  
  ## The best possible model has sensitivity of 1 and specifity of 1. 
  ## How far are we from that value?
  coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), 
                   ncol = 2, 
                   byrow = TRUE)
  colnames(coords) <- c("Spec", "Sens")
  rownames(coords) <- c("Best", "Current")
  c(out, Dist = dist(coords)[1])
}

set.seed(949)
mod1 <- train(Class ~ ., data = training,
              ## 'modelInfo' is a list object found in the linked
              ## source code
              method = modelInfo,
              ## Minimize the distance to the perfect model
              metric = "Dist",
              maximize = FALSE,
              tuneLength = 20,
              trControl = trainControl(method = "cv",
                                       classProbs = TRUE,
                                       summaryFunction = fourStats))

mod1

mod1$times$everything[3]/mod0$times$everything[3]

metrics <- mod1$results[, c(2, 4:6)]
metrics <- melt(metrics, id.vars = "threshold", 
                variable.name = "Resampled",
                value.name = "Data")

ggplot(metrics, aes(x = threshold, y = Data)) + 
  geom_line() + facet_grid(~ Resampled) + 
  ylab("") + xlab("Probability Cutoff")

png("curves.png", width = 480, height = 320)
ggplot(metrics, aes(x = threshold, y = Data, color = Resampled)) + 
  geom_line() + 
  ylab("") + xlab("Probability Cutoff") +
  theme(legend.position = "top")
dev.off()

example <- predict(mod1, head(testing), type = "prob")
example$Class <- predict(mod1, head(testing))
example$Note <- ""
example$Note[example$Class1 > .5 & example$Class1 <= mod1$bestTune$threshold] <- "*"
example


