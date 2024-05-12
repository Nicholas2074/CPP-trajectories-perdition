# The defaults that are set for
# initialization and update functions
# usually don't have to be changed

# //SECTION - 05-BackpropWeightDecay

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

model_mlp51 <- list()

model_mlp51 <- foreach(i = 1:1300, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = parameterGrid1[i, 1],
		learnFunc = "BackpropWeightDecay",
		learnFuncParams = c(parameterGrid1[i, 2], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_51 <- data.frame(lapply(model_mlp51, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_51 <- data.frame(lapply(model_mlp51, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_51 <- t(trainErrors_51)
testErrors_51 <- t(testErrors_51)

trainErrors_51[which(trainErrors_51 == min(trainErrors_51))]
testErrors_51[which(testErrors_51 == min(testErrors_51))]

# selection
model_m51 <- model_mlp51[[which(testErrors_51 == min(testErrors_51))]]

model_m51

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

model_mlp52 <- list()

model_mlp52 <- foreach(i = 1:507, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = c(parameterGrid2[i, 1], parameterGrid2[i, 2]),
		learnFunc = "BackpropWeightDecay",
		learnFuncParams = c(parameterGrid2[i, 3], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_52 <- data.frame(lapply(model_mlp52, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_52 <- data.frame(lapply(model_mlp52, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_52 <- t(trainErrors_52)
testErrors_52 <- t(testErrors_52)

trainErrors_52[which(trainErrors_52 == min(trainErrors_52))]
testErrors_52[which(testErrors_52 == min(testErrors_52))]

# selection
model_m52 <- model_mlp52[[which(testErrors_52 == min(testErrors_52))]]

model_m52

# //!SECTION
