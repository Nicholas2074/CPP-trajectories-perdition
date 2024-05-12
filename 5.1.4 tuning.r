# The defaults that are set for
# initialization and update functions
# usually don't have to be changed

# //SECTION - 04-BackpropMomentum

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

model_mlp41 <- list()

model_mlp41 <- foreach(i = 1:1300, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = parameterGrid1[i, 1],
		learnFunc = "BackpropMomentum",
		learnFuncParams = c(parameterGrid1[i, 2], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_41 <- data.frame(lapply(model_mlp41, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_41 <- data.frame(lapply(model_mlp41, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_41 <- t(trainErrors_41)
testErrors_41 <- t(testErrors_41)

trainErrors_41[which(trainErrors_41 == min(trainErrors_41))]
testErrors_41[which(testErrors_41 == min(testErrors_41))]

# selection
model_m41 <- model_mlp41[[which(testErrors_41 == min(testErrors_41))]]

model_m41

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

model_mlp42 <- list()

model_mlp42 <- foreach(i = 1:507, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = c(parameterGrid2[i, 1], parameterGrid2[i, 2]),
		learnFunc = "BackpropMomentum",
		learnFuncParams = c(parameterGrid2[i, 3], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_42 <- data.frame(lapply(model_mlp42, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_42 <- data.frame(lapply(model_mlp42, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_42 <- t(trainErrors_42)
testErrors_42 <- t(testErrors_42)

trainErrors_42[which(trainErrors_42 == min(trainErrors_42))]
testErrors_42[which(testErrors_42 == min(testErrors_42))]

# selection
model_m42 <- model_mlp42[[which(testErrors_42 == min(testErrors_42))]]

model_m42

# //!SECTION
