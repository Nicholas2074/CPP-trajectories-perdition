# The defaults that are set for
# initialization and update functions
# usually don't have to be changed

# //SECTION - 03-BackpropChunk

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

model_mlp31 <- list()

model_mlp31 <- foreach(i = 1:1300, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = parameterGrid1[i, 1],
		learnFunc = "BackpropChunk",
		learnFuncParams = c(parameterGrid1[i, 2], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_31 <- data.frame(lapply(model_mlp31, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_31 <- data.frame(lapply(model_mlp31, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_31 <- t(trainErrors_31)
testErrors_31 <- t(testErrors_31)

trainErrors_31[which(trainErrors_31 == min(trainErrors_31))]
testErrors_31[which(testErrors_31 == min(testErrors_31))]

# selection
model_m31 <- model_mlp31[[which(testErrors_31 == min(testErrors_31))]]

model_m31

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

model_mlp32 <- list()

model_mlp32 <- foreach(i = 1:507, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = c(parameterGrid2[i, 1], parameterGrid2[i, 2]),
		learnFunc = "BackpropChunk",
		learnFuncParams = c(parameterGrid2[i, 3], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_32 <- data.frame(lapply(model_mlp32, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_32 <- data.frame(lapply(model_mlp32, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_32 <- t(trainErrors_32)
testErrors_32 <- t(testErrors_32)

trainErrors_32[which(trainErrors_32 == min(trainErrors_32))]
testErrors_32[which(testErrors_32 == min(testErrors_32))]

# selection
model_m32 <- model_mlp32[[which(testErrors_32 == min(testErrors_32))]]

model_m32

# //!SECTION
