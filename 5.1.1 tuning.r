# The defaults that are set for
# initialization and update functions
# usually don't have to be changed

# //SECTION - 01-Std_Backpropagation

# //ANCHOR - 1 layer

# para grid
parameterGrid1 <- expand.grid(c(3:15), seq(0.001, 0.1, by = 0.001))

colnames(parameterGrid1) <- c("nHidden", "learnRate")
rownames(parameterGrid1) <- paste("nnet-",
    apply(parameterGrid1, 1, function(x) {
        paste(x, sep = "", collapse = "-")
    }),
    sep = ""
)

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

model_mlp11 <- list()

model_mlp11 <- foreach(i = 1:1300, .packages = "RSNNS") %dopar% {
    mlp(
        data_snns$inputsTrain,
        data_snns$targetsTrain,
        size = parameterGrid1[i, 1],
        learnFunc = "Std_Backpropagation",
        learnFuncParams = c(parameterGrid1[i, 2], 0.1),
        maxit = 200,
        inputsTest = data_snns$inputsTest,
        targetsTest = data_snns$targetsTest
    )
}

# stop
stopImplicitCluster()

# single processing

set.seed(0114)

library(RSNNS)

# model_mlp11 <- apply(parameterGrid1, 1, function(p) {
#   mlp(
#     data_snns$inputsTrain,
#     data_snns$targetsTrain,
#     size = p[1],
#     learnFunc = "Std_Backpropagation",
#     learnFuncParams = c(p[2], 0.1),
#     maxit = 200,
#     inputsTest = data_snns$inputsTest,
#     targetsTest = data_snns$targetsTest
#   )
# })

# # graph
# par(mfrow=c(4,3))

# for (modInd in 1:length(model_mlp1)) {
#   plotIterativeError(model_mlp1[[modInd]], main = names(model_mlp1)[modInd])
# }

# MSE
trainErrors_11 <- data.frame(lapply(model_mlp11, function(mod) {
    error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
    error
}))

testErrors_11 <- data.frame(lapply(model_mlp11, function(mod) {
    pred <- predict(mod, data_snns$inputsTest)
    error <- sqrt(sum((pred - data_snns$targetsTest)^2))
    error
}))

trainErrors_11 <- t(trainErrors_11)
testErrors_11 <- t(testErrors_11)

trainErrors_11[which(trainErrors_11 == min(trainErrors_11))]
testErrors_11[which(testErrors_11 == min(testErrors_11))]

# selection
model_m11 <- model_mlp11[[which(testErrors_11 == min(testErrors_11))]]

model_m11

# //ANCHOR - 2 layers

set.seed(0114)

# para grid
parameterGrid2 <- expand.grid(c(3:15), c(3:15), c(0.00316, 0.0147, 0.1))

colnames(parameterGrid2) <- c("nHidden1", "nHidden2", "learnRate")
# rownames(parameterGrid2) <- paste("nnet-",
#   apply(parameterGrid2, 1, function(x) {
#     paste(x, sep = "", collapse = "-")
#   }),
#   sep = ""
# ) # for plotIterativeError

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

model_mlp12 <- list()

model_mlp12 <- foreach(i = 1:507, .packages = "RSNNS") %dopar% {
    mlp(
        data_snns$inputsTrain,
        data_snns$targetsTrain,
        size = c(parameterGrid2[i, 1], parameterGrid2[i, 2]),
        learnFunc = "Std_Backpropagation",
        learnFuncParams = c(parameterGrid2[i, 3], 0.1),
        maxit = 200,
        inputsTest = data_snns$inputsTest,
        targetsTest = data_snns$targetsTest
    )
}

# stop
stopImplicitCluster()

# single processing

# set.seed(0114)
#
# library(RSNNS)
#
# model_mlp12 <- apply(parameterGrid2, 1, function(p) {
#   mlp(
#     data_snns$inputsTrain,
#     data_snns$targetsTrain,
#     size = c(p[1], p[2]),
#     learnFunc = "Std_Backpropagation",
#     learnFuncParams = c(p[3], 0.1),
#     maxit = 200,
#     inputsTest = data_snns$inputsTest,
#     targetsTest = data_snns$targetsTest
#   )
# })

# # graph
# par(mfrow=c(4,3))

# for (modInd in 1:length(model_mlp1)) {
#   plotIterativeError(model_mlp1[[modInd]], main = names(model_mlp1)[modInd])
# }

# MSE
trainErrors_12 <- data.frame(lapply(model_mlp12, function(mod) {
    error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
    error
}))

testErrors_12 <- data.frame(lapply(model_mlp12, function(mod) {
    pred <- predict(mod, data_snns$inputsTest)
    error <- sqrt(sum((pred - data_snns$targetsTest)^2))
    error
}))

trainErrors_12 <- t(trainErrors_12)
testErrors_12 <- t(testErrors_12)

trainErrors_12[which(trainErrors_12 == min(trainErrors_12))]
testErrors_12[which(testErrors_12 == min(testErrors_12))]

# selection
model_m12 <- model_mlp12[[which(testErrors_12 == min(testErrors_12))]]

model_m12

# //!SECTION
