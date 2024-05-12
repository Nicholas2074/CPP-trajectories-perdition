# The defaults that are set for
# initialization and update functions
# usually don't have to be changed

# //SECTION - 01-rbf

# //ANCHOR - 1 layer

# parallel processing

library(RSNNS)

library(doParallel)

# clear env
# env <- foreach:::.foreachGlobals
# rm(list = ls(name = env), pos = env)

# cores
registerDoParallel(cores = detectCores())

# modeling
set.seed(0114)

model_rbfr1 <- list()

model_rbfr1 <- foreach(i = 1:1300, .packages = "RSNNS") %dopar% {
    rbf(
        data_snns$inputsTrain,
        data_snns$targetsTrain,
        size = parameterGrid1[i, 1],
        learnFuncParams = c(1e-8, 0, 1e-8, parameterGrid1[i, 2], 0.8),
        maxit = 200
    )
}

# stop
stopImplicitCluster()

# MSE
trainErrors_r1 <- data.frame(lapply(model_rbfr1, function(mod) {
    error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
    error
}))

testErrors_r1 <- data.frame(lapply(model_rbfr1, function(mod) {
    pred <- predict(mod, data_snns$inputsTest)
    error <- sqrt(sum((pred - data_snns$targetsTest)^2))
    error
}))

trainErrors_r1 <- t(trainErrors_r1)
testErrors_r1 <- t(testErrors_r1)

trainErrors_r1[which(trainErrors_r1 == min(trainErrors_r1))]
testErrors_r1[which(testErrors_r1 == min(testErrors_r1))]

# selection
model_r1 <- model_rbfr1[[which(testErrors_r1 == min(testErrors_r1))]]

model_r1

# //ANCHOR - 2 layers

# parallel processing

library(RSNNS)

library(doParallel)

# clear env
# env <- foreach:::.foreachGlobals
# rm(list = ls(name = env), pos = env)

# cores
registerDoParallel(cores = detectCores())

# modeling
set.seed(0114)

model_rbfr2 <- list()

model_rbfr2 <- foreach(i = 1:507, .packages = "RSNNS") %dopar% {
    rbf(
        data_snns$inputsTrain,
        data_snns$targetsTrain,
        size = c(parameterGrid2[i, 1], parameterGrid2[i, 2]),
        learnFuncParams = c(1e-8, 0, 1e-8, parameterGrid1[i, 2], 0.8),
        maxit = 200
    )
}

# stop
stopImplicitCluster()

# MSE
trainErrors_r2 <- data.frame(lapply(model_rbfr2, function(mod) {
    error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
    error
}))

testErrors_r2 <- data.frame(lapply(model_rbfr2, function(mod) {
    pred <- predict(mod, data_snns$inputsTest)
    error <- sqrt(sum((pred - data_snns$targetsTest)^2))
    error
}))

trainErrors_r2 <- t(trainErrors_r2)
testErrors_r2 <- t(testErrors_r2)

trainErrors_r2[which(trainErrors_r2 == min(trainErrors_r2))]
testErrors_r2[which(testErrors_r2 == min(testErrors_r2))]

# selection
model_r2 <- model_rbfr2[[which(testErrors_r2 == min(testErrors_r2))]]

model_r2

# //!SECTION