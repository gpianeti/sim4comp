library(class)
library(ggpubr)
library(knitr)
library(e1071)
library(randomForest)
library(gbm)
library(foreach)
library(doParallel)
library(BART)
library(ggplot2)
library(tidyr)

message(R.version$version.string)
message("Platform: ", R.version$platform)

############################ ###################################################
### SCENARIOS SIMULATION ### ###################################################
############################# ##################################################

source("gendata_function.R")
p = 2
c = 2

set.seed(123)
# SCENARIO 1 ###################################################################
n = 100
distrib <- c("Normal", "Normal")
param <- list(list(c(0, 1), c(0, 1)),
              list(c(1, 1), c(1, 1)))

s1.train <- replicate(100, gendata(n, c, p, distrib, param), simplify = F)
s1.test <- gendata(1000, c, p, distrib, param)

p1 <- ggplot(data = s1.train[[1]], aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "First training Dataset")
p2 <- ggplot(data = s1.test, aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "Test Dataset")
s1.plot <- ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom")


# SCENARIO 2 ###################################################################
n = 100
distrib <- c("Normal", "Normal")
param <- list(list(c(0, 1), c(0, 1)),
              list(c(1, 1), c(1, 1)))
corr <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)

s2.train <- replicate(100, gendata(n, c, p, distrib, param, corr), simplify = F)
s2.test <- gendata(1000, c, p, distrib, param, corr)

p1 <- ggplot(data = s2.train[[1]], aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "First training Dataset")
p2 <- ggplot(data = s2.test, aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "Test Dataset")
s2.plot <- ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom")


# SCENARIO 3 ###################################################################
n = 100
distrib <- c("t", "t")
param <- list(list(c(2, 0), c(2, 0)),
              list(c(2, 2), c(2, 2)))
corr <- matrix(c(1, -0.5, -0.5, 1), nrow = 2, ncol = 2)

s3.train <- replicate(100, gendata(n, c, p, distrib, param, corr), simplify = F)
s3.test <- gendata(1000, c, p, distrib, param, corr)

p1 <- ggplot(data = s3.train[[1]], aes(x = t1, y = t2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "First training Dataset")
p2 <- ggplot(data = s3.test, aes(x = t1, y = t2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "Test Dataset")
s3.plot <- ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom")


# SCENARIO 4 ###################################################################
n = 100
distrib <- c("Normal", "Normal")
param <- list(list(c(0, 1), c(0, 1)),
              list(c(1, 1), c(1, 1)))
corr <- list(matrix(c(1, 0.3, 0.3, 1), nrow = 2, ncol = 2),
             matrix(c(1, -0.8, -0.8, 1), nrow = 2, ncol = 2))

s4.train <- replicate(100, gendata(n, c, p, distrib, param, corr), simplify = F)
s4.test <- gendata(1000, c, p, distrib, param, corr)

p1 <- ggplot(data = s4.train[[1]], aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "First training Dataset")
p2 <- ggplot(data = s4.test, aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "Test Dataset")
s4.plot <- ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom")


# SCENARIO 5 ###################################################################
n = 100
distrib <- c("t", "t")
param <- list(list(c(2, 0), c(2, 0)),
              list(c(2, 1), c(2, 1)))
corr <- list(matrix(c(1, 0.3, 0.3, 1), nrow = 2, ncol = 2),
             matrix(c(1, -0.8, -0.8, 1), nrow = 2, ncol = 2))

s5.train <- replicate(100, gendata(n, c, p, distrib, param, corr), simplify = F)
s5.test <- gendata(1000, c, p, distrib, param, corr)

p1 <- ggplot(data = s5.train[[1]], aes(x = t1, y = t2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "First training Dataset")
p2 <- ggplot(data = s5.test, aes(x = t1, y = t2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "Test Dataset")
s5.plot <- ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom")


# SCENARIO 6 ###################################################################
s6.dgp <- function (n, corr) {
  Normal1 <- rnorm(n, 0, 1)
  Normal2 <- corr*Normal1 + sqrt(1-corr^2)*rnorm(n, 0, 1)
  response <- log(Normal1^2) + exp(Normal2) + rnorm(n, sd = 2)
  Y <- ifelse(response>0.3784, 1, 2)
  output <- data.frame(cbind(Normal1, Normal2, Y))
  output$Y <- as.factor(output$Y)
  return(output)
}

s6.train <- replicate(100, s6.dgp(100, 0.7), simplify = F)
s6.test <- s6.dgp(1000, 0.7)

p1 <- ggplot(data = s6.train[[1]], aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "First training Dataset")
p2 <- ggplot(data = s6.test, aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "Test Dataset")
s6.plot <- ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom")


# SCENARIO 7 ###################################################################
n = 20
distrib <- c("Normal", "Normal")
param <- list(list(c(0, 1), c(0, 1)),
              list(c(1, 1), c(1, 1)))
corr <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)

s7.train <- replicate(100, gendata(n, c, p, distrib, param, corr), simplify = F)
s7.test <- gendata(1000, c, p, distrib, param, corr)

p1 <- ggplot(data = s7.train[[1]], aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "First training Dataset")
p2 <- ggplot(data = s7.test, aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "Test Dataset")
s7.plot <- ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom")


# SCENARIO 8 ###################################################################
n = 20
distrib <- c("Normal", "Normal")
param <- list(list(c(0, 1), c(0, 1)),
              list(c(1, 1), c(1, 1)))
corr <- list(matrix(c(1, 0.3, 0.3, 1), nrow = 2, ncol = 2),
             matrix(c(1, -0.8, -0.8, 1), nrow = 2, ncol = 2))

s8.train <- replicate(100, gendata(n, c, p, distrib, param, corr), simplify = F)
s8.test <- gendata(1000, c, p, distrib, param, corr)

p1 <- ggplot(data = s8.train[[1]], aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "First training Dataset")
p2 <- ggplot(data = s8.test, aes(x = Normal1, y = Normal2, color = Y)) + 
  geom_point(size = 0.7) + 
  theme(axis.title = element_text(size = 8), aspect.ratio = 1) +
  labs(title = "Test Dataset")
s8.plot <- ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom")


### ############################################################################
train.sets <- list(s1.train, s2.train, s3.train, s4.train, s5.train, s6.train,
                   s7.train, s8.train)
test.sets <- list(s1.test, s2.test, s3.test, s4.test, s5.test, s6.test, s7.test,
                  s8.test)
sets.plots <- list(s1.plot, s2.plot, s3.plot, s4.plot, s5.plot, s6.plot, s7.plot,
                   s8.plot)
rm(s1.train, s2.train, s3.train, s4.train, s5.train, s6.train, s7.train, 
   s8.train, s1.test, s2.test, s3.test, s4.test, s5.test, s6.test, 
   s7.test, s8.test, s1.plot, s2.plot, s3.plot, s4.plot, s5.plot, 
   s6.plot, s7.plot, s8.plot, s6.dgp, p1, p2)


########################### ####################################################
### FITTING AND TESTING ### ####################################################
########################### ####################################################

final.results <- as.list(rep(NA, 8))
final.charts <- as.list(rep(NA, 8))

n_cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(n_cores)
registerDoParallel(cl)
parallel::clusterSetRNGStream(cl, 123)

for (sc in 1:8) {
  current.train <- train.sets[[sc]]
  current.test <- test.sets[[sc]]
  n.test <- nrow(current.test)
  n <- nrow(current.train[[1]])
  
  
  error.distrib <- foreach(i = 1:100, .combine = rbind, 
                           .packages = c("class", "e1071", "randomForest", "gbm", 
                                         "BART", "MASS"))   %dopar% {
                                                         
    train_data <- current.train[[i]]
    X.train <- train_data[, 1:2]
    Y.train <- train_data[, 3]
    X.test <- current.test[, 1:2]
    Y.test <- current.test[, 3]
    errors_i <- numeric(8)
    index <- split(sample(1:n, n), ceiling(1:n / (n/10)))
    
    # K-nn #########################################################################
    kfold.err <- matrix(ncol = 2, nrow = 0)
    for (k in seq(1, floor(n*9/10 - 1), by = 2)) {
      errors <- rep(NA, 10)
      for (q in 1:10) {
        X.train.cv <- train_data[-index[[q]], 1:2]
        Y.train.cv <- train_data[-index[[q]], 3]
        X.test.cv <- train_data[index[[q]], 1:2]
        Y.test.cv <- train_data[index[[q]], 3]
        predval <- factor(knn(train = X.train.cv, test = X.test.cv, 
                          cl = Y.train.cv, k = k), levels = c("1", "2"))
        errors[q] <- sum(Y.test.cv != predval)/ length(Y.test.cv)
      }
      kfold.err <- rbind(kfold.err, c(k, mean(errors)))
    }
    colnames(kfold.err) <- c("k", "error")
    k.tuned <- kfold.err[which.min(kfold.err[, 2]), 1]
    
    predval.knn <- factor(knn(train = X.train, test = X.test, 
                              cl = Y.train, k = k.tuned), levels = c("1", "2"))
    errors_i[1] <- sum(predval.knn != Y.test)/ n.test
    
    # Logistic Regression ##########################################################
    lr.fits <- glm(Y ~ ., data = train_data, family = "binomial")
    response <- stats::predict(lr.fits, newdata = current.test, type = "response")
    predval.lr <- factor(ifelse(response < 0.5, 1, 2), levels = c("1", "2"))
    errors_i[2] <- sum(predval.lr != Y.test)/ n.test
    
    # LDA ##########################################################################
    lda.fits <- lda(Y ~ ., data = train_data)
    predval.lda <- predict(lda.fits, current.test[, 1:2])$class
    errors_i[3] <- sum(predval.lda != Y.test)/ n.test
    
    # QDA ##########################################################################
    qda.fits <- qda(Y ~ ., data = train_data)
    predval.qda <- predict(qda.fits, current.test[, 1:2])$class
    errors_i[4] <- sum(predval.qda != Y.test)/ n.test
    
    # Naive Bayes ##################################################################
    nb.fits <- naiveBayes(Y ~ ., data = train_data)
    predval.nb <- factor(predict(nb.fits, current.test[, 1:2]), levels = c("1", "2"))
    errors_i[5] <- sum(predval.nb != Y.test)/ n.test
    
    # Random Forest ################################################################
    rf.fits <- randomForest(Y ~ ., data = train_data)
    predval.rf <- factor(predict(rf.fits, current.test[, 1:2]), levels = c("1", "2"))
    errors_i[6] <- sum(predval.rf != Y.test)/ n.test
    
    # Boosting #####################################################################
    trees.grid <- seq(20, 1000, by = 20)
    cv.gbm <- data.frame(matrix(nrow = length(trees.grid), ncol = 2))
    for (h in 1:length(trees.grid)) {
      errors <- numeric(10)
      for (k in 1:10) {
        train.cv <- train_data[-index[[k]], ]
        X.test.cv <- train_data[index[[k]], 1:2]
        Y.test.cv <- train_data[index[[k]], 3]
        gbm.fits <- gbm(as.integer(Y)-1 ~ ., 
                        data = train.cv,
                        distribution = "bernoulli",
                        interaction.depth = 1,
                        n.trees = trees.grid[h],
                        shrinkage = 0.01,
                        n.minobsinnode = 2,
                        verbose = FALSE)
        response <- predict(gbm.fits, 
                            newdata = X.test.cv,
                            n.trees = trees.grid[h],
                            type = "response")
        predval <- factor(ifelse(response > 0.5, 2, 1), levels = c("1", "2"))
        errors[k] <- sum(predval != Y.test.cv) / length(Y.test.cv)
      }
      cv.gbm[h, ] <- c(trees.grid[h], mean(errors))
    }
    trees.tuned <- cv.gbm[which.min(cv.gbm[, 2]), 1]
    
    gbm.fits <- gbm(as.integer(Y)-1 ~ ., 
               data = train_data,
               distribution = "bernoulli",
               interaction.depth = 1,
               n.trees = trees.tuned,
               shrinkage = 0.01,
               n.minobsinnode = 2,
               verbose = FALSE)
    response.gbm <- predict(gbm.fits,
                        newdata = current.test[, 1:2],
                        n.trees = trees.tuned,
                        type = "response")
    predval.gbm <- factor(ifelse(response.gbm > 0.5, 2, 1), levels = c("1", "2"))
    errors_i[7] <- sum(predval.gbm != Y.test)/ n.test
  
    
    # BART #########################################################################
    bart_ntree <- ifelse(n < 50, 50, 200)
    bart_nskip <- ifelse(n < 50, 500, 200)
    bart_ndpost <- ifelse(n < 50, 500, 1000)
    
    response.bart <- pbart(x.train = X.train, 
                      y.train = as.integer(Y.train)-1, 
                      x.test = current.test[, 1:2],
                      ntree = bart_ntree,
                      nskip = bart_nskip,
                      ndpost = bart_ndpost)$prob.test.mean
    predval.bart <- factor(ifelse(response.bart > 0.5, 2, 1), levels = c("1", "2"))
    errors_i[8] <- sum(predval.bart != Y.test)/ n.test
    
    return(errors_i)
  }
  
  print(paste("Iteration: Scenario", sc))
  
  colnames(error.distrib) <- c("K-nn", "Logistic", "LDA", "QDA", 
                               "Naive Bayes", "Random Forest",
                               "Boosting", "BART")
  
  final.results[[sc]] <- error.distrib
  
}

parallel::stopCluster(cl)

saveRDS(final.results, file = "final.results")

