# The defaults that are set for
# initialization and update functions
# usually don't have to be changed

# //SECTION - 06-Rprop

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

model_mlp61 <- list()

model_mlp61 <- foreach(i = 1:1300, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = parameterGrid1[i, 1],
		learnFunc = "Rprop",
		learnFuncParams = c(parameterGrid1[i, 2], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_61 <- data.frame(lapply(model_mlp61, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_61 <- data.frame(lapply(model_mlp61, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_61 <- t(trainErrors_61)
testErrors_61 <- t(testErrors_61)

trainErrors_61[which(trainErrors_61 == min(trainErrors_61))]
testErrors_61[which(testErrors_61 == min(testErrors_61))]

# selection
model_m61 <- model_mlp61[[which(testErrors_61 == min(testErrors_61))]]

model_m61

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

model_mlp62 <- list()

model_mlp62 <- foreach(i = 1:507, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = c(parameterGrid2[i, 1], parameterGrid2[i, 2]),
		learnFunc = "Rprop",
		learnFuncParams = c(parameterGrid2[i, 3], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_62 <- data.frame(lapply(model_mlp62, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_62 <- data.frame(lapply(model_mlp62, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_62 <- t(trainErrors_62)
testErrors_62 <- t(testErrors_62)

trainErrors_62[which(trainErrors_62 == min(trainErrors_62))]
testErrors_62[which(testErrors_62 == min(testErrors_62))]

# selection
model_m62 <- model_mlp62[[which(testErrors_62 == min(testErrors_62))]]

model_m62

# //!SECTION
