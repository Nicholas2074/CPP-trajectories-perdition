# The defaults that are set for
# initialization and update functions
# usually don't have to be changed

# //SECTION - 02-BackpropBatch

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

model_mlp21 <- list()

model_mlp21 <- foreach(i = 1:1300, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = parameterGrid1[i, 1],
		learnFunc = "BackpropBatch",
		learnFuncParams = c(parameterGrid1[i, 2], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_21 <- data.frame(lapply(model_mlp21, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_21 <- data.frame(lapply(model_mlp21, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_21 <- t(trainErrors_21)
testErrors_21 <- t(testErrors_21)

trainErrors_21[which(trainErrors_21 == min(trainErrors_21))]
testErrors_21[which(testErrors_21 == min(testErrors_21))]

# selection
model_m21 <- model_mlp21[[which(testErrors_21 == min(testErrors_21))]]

model_m21

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

model_mlp22 <- list()

model_mlp22 <- foreach(i = 1:507, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = c(parameterGrid2[i, 1], parameterGrid2[i, 2]),
		learnFunc = "BackpropBatch",
		learnFuncParams = c(parameterGrid2[i, 3], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_22 <- data.frame(lapply(model_mlp22, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_22 <- data.frame(lapply(model_mlp22, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_22 <- t(trainErrors_22)
testErrors_22 <- t(testErrors_22)

trainErrors_22[which(trainErrors_22 == min(trainErrors_22))]
testErrors_22[which(testErrors_22 == min(testErrors_22))]

# selection
model_m22 <- model_mlp22[[which(testErrors_22 == min(testErrors_22))]]

model_m22

# //!SECTION
