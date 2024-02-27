library(caret)
library(randomForest)

SLC14_1 <- function (n = 100, noiseVars = 0, corrVars = 0, corrType = "AR1", corrValue = 0) {
  well_numbered <- function (prefix, items) 
    paste0(prefix, gsub(" ", "0", format(1:items)))
  
  dat <- matrix(rnorm(n * 20, sd = 3), ncol = 20)
  foo <- function(x) x[1] + sin(x[2]) + log(abs(x[3])) + x[4]^2 + 
    x[5] * x[6] + ifelse(x[7] * x[8] * x[9] < 0, 1, 0) + 
    ifelse(x[10] > 0, 1, 0) + x[11] * ifelse(x[11] > 0, 1, 0) + sqrt(abs(x[12])) + cos(x[13]) + 2 * x[14] + abs(x[15]) + 
    ifelse(x[16] < -1, 1, 0) + x[17] * ifelse(x[17] < -1, 1, 0) - 2 * x[18] - x[19] * x[20]
  dat <- as.data.frame(dat)
  colnames(dat) <- well_numbered("Var", ncol(dat))
  dat$y <- apply(dat[, 1:20], 1, foo) + rnorm(n, sd = 3)
  dat
}

data_seed <- 33121
model_seed <- sample.int(100000, 25)

training_size <- 100

trees <- 1000

set.seed(data_seed)
tr_dat <- SLC14_1(training_size)
te_dat <- SLC14_1(10000)

set.seed(data_seed)
rf_mod <- randomForest(y ~ ., data = tr_dat, ntree = trees)
tst <- postResample(predict(rf_mod, te_dat), te_dat$y)


for(i in seq(along = model_seed)) {
  cat("iteration", i, "\n")
  
  set.seed(model_seed[i])
  cv10 <- train(y ~ ., data = tr_dat, ntree = trees, 
                method = "rf",
                tuneGrid = data.frame(mtry = rf_mod$mtry),
                trControl = trainControl(method = "cv"))
  
  results_cv10 <- data.frame(method = "CV",
                             number = 10,
                             pct = 9/10, 
                             RMSE = getTrainPerf(cv10)[1,1],
                             R2 = getTrainPerf(cv10)[1,2],
                             time = cv10$times$everything[3])
  results_oob <- data.frame(method = "OOB",
                            number = NA,
                            pct = .632, 
                            RMSE = caret:::rfStats(cv10$finalModel)[1],
                            R2 = caret:::rfStats(cv10$finalModel)[2],
                            time = NA)  
  
  set.seed(model_seed[i])
  cv05 <- train(y ~ ., data = tr_dat, ntree = trees, 
                method = "rf",
                tuneGrid = data.frame(mtry = rf_mod$mtry),
                trControl = trainControl(method = "cv",  number = 5))
  results_cv05 <- data.frame(method = "CV",
                             number = 5,
                             pct = 4/5, 
                             RMSE = getTrainPerf(cv05)[1,1],
                             R2 = getTrainPerf(cv05)[1,2],
                             time = cv05$times$everything[3])  
  
  results_rcv <- data.frame(method = "rCV",
                            number = (2:10)*10,
                            pct = 9/10, 
                            RMSE = NA,
                            R2 = NA,
                            time = NA)    
  for(j in 1:nrow(results_rcv)) {
    rcv <- train(y ~ ., data = tr_dat, ntree = trees, 
                 method = "rf",
                 tuneGrid = data.frame(mtry = rf_mod$mtry),
                 trControl = trainControl(method = "repeatedcv",  
                                          number = 10, 
                                          repeats = results_rcv$number[j]/10))
    results_rcv$RMSE[j] <- getTrainPerf(rcv)[1,1]
    results_rcv$R2[j] <- getTrainPerf(rcv)[1,2]
    results_rcv$time[j] <- rcv$times$everything[3]
    rm(rcv)
  }
  
  results_bt <- data.frame(method = "boot",
                           number = (2:10)*10,
                           pct = 0.632, 
                           RMSE = NA,
                           R2 = NA,
                           time = NA)    
  for(j in 1:nrow(results_bt)) {
    bt <- train(y ~ ., data = tr_dat, ntree = trees, 
                method = "rf",
                tuneGrid = data.frame(mtry = rf_mod$mtry),
                trControl = trainControl(method = "boot",  number = results_bt$number[j]))
    results_bt$RMSE[j] <- getTrainPerf(bt)[1,1]
    results_bt$R2[j] <- getTrainPerf(bt)[1,2]
    results_bt$time[j] <- bt$times$everything[3]
    rm(bt)
  }
  
  results_lgo <- expand.grid(method = "lgo",
                             number = (2:10)*10,
                             pct = seq(.5, .95, by = .05), 
                             RMSE = NA,
                             R2 = NA,
                             time = NA)    
  for(j in 1:nrow(results_lgo)) {
    #     set.seed(model_seed[i])
    lgo <- train(y ~ ., data = tr_dat, ntree = trees, 
                 method = "rf",
                 tuneGrid = data.frame(mtry = rf_mod$mtry),
                 trControl = trainControl(method = "LGOCV",  
                                          number = results_lgo$number[j],
                                          p = results_lgo$pct[j]))
    results_lgo$RMSE[j] <- getTrainPerf(lgo)[1,1]
    results_lgo$R2[j] <- getTrainPerf(lgo)[1,2]
    results_lgo$time[j] <- lgo$times$everything[3]
    rm(lgo)
  }
  
  results <- rbind(results_cv10, results_cv05, results_rcv, 
                   results_bt, results_lgo,
                   results_oob)
  results$model_seed <- model_seed[i]
  results$data_seed <- data_seed
  results$Test_RMSE <- tst[1]
  results$Test_R2 <- tst[2]
  results$n <- 100
  
  all_results <- if(i == 1) results else rbind(all_results, results)
  save(all_results, file = paste0("all_results_", data_seed, "_", 100, ".RData"))
}

q("no")

