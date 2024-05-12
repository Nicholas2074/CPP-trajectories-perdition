# The defaults that are set for
# initialization and update functions
# usually don't have to be changed

# //SECTION - 07-Quickprop

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

model_mlp71 <- list()

model_mlp71 <- foreach(i = 1:1300, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = parameterGrid1[i, 1],
		learnFunc = "Quickprop",
		learnFuncParams = c(parameterGrid1[i, 2], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_71 <- data.frame(lapply(model_mlp71, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_71 <- data.frame(lapply(model_mlp71, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_71 <- t(trainErrors_71)
testErrors_71 <- t(testErrors_71)

trainErrors_71[which(trainErrors_71 == min(trainErrors_71))]
testErrors_71[which(testErrors_71 == min(testErrors_71))]

# selection
model_m71 <- model_mlp71[[which(testErrors_71 == min(testErrors_71))]]

model_m71

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

model_mlp72 <- list()

model_mlp72 <- foreach(i = 1:507, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = c(parameterGrid2[i, 1], parameterGrid2[i, 2]),
		learnFunc = "Quickprop",
		learnFuncParams = c(parameterGrid2[i, 3], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_72 <- data.frame(lapply(model_mlp72, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_72 <- data.frame(lapply(model_mlp72, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_72 <- t(trainErrors_72)
testErrors_72 <- t(testErrors_72)

trainErrors_72[which(trainErrors_72 == min(trainErrors_72))]
testErrors_72[which(testErrors_72 == min(testErrors_72))]

# selection
model_m72 <- model_mlp72[[which(testErrors_72 == min(testErrors_72))]]

model_m72

# //!SECTION