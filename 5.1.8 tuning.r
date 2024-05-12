# The defaults that are set for
# initialization and update functions
# usually don't have to be changed

# //SECTION - 07-SCG

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

model_mlp81 <- list()

model_mlp81 <- foreach(i = 1:1300, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = parameterGrid1[i, 1],
		learnFunc = "SCG",
		learnFuncParams = c(parameterGrid1[i, 2], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_81 <- data.frame(lapply(model_mlp81, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_81 <- data.frame(lapply(model_mlp81, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_81 <- t(trainErrors_81)
testErrors_81 <- t(testErrors_81)

trainErrors_81[which(trainErrors_81 == min(trainErrors_81))]
testErrors_81[which(testErrors_81 == min(testErrors_81))]

# selection
model_m81 <- model_mlp81[[which(testErrors_81 == min(testErrors_81))]]

model_m81

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

model_mlp82 <- list()

model_mlp82 <- foreach(i = 1:507, .packages = "RSNNS") %dopar% {
	mlp(
		data_snns$inputsTrain,
		data_snns$targetsTrain,
		size = c(parameterGrid2[i, 1], parameterGrid2[i, 2]),
		learnFunc = "SCG",
		learnFuncParams = c(parameterGrid2[i, 3], 0.1),
		maxit = 200,
		inputsTest = data_snns$inputsTest,
		targetsTest = data_snns$targetsTest
	)
}

# stop
stopImplicitCluster()

# MSE
trainErrors_82 <- data.frame(lapply(model_mlp82, function(mod) {
	error <- sqrt(sum((mod$fitted.values - data_snns$targetsTrain)^2))
	error
}))

testErrors_82 <- data.frame(lapply(model_mlp82, function(mod) {
	pred <- predict(mod, data_snns$inputsTest)
	error <- sqrt(sum((pred - data_snns$targetsTest)^2))
	error
}))

trainErrors_82 <- t(trainErrors_82)
testErrors_82 <- t(testErrors_82)

trainErrors_82[which(trainErrors_82 == min(trainErrors_82))]
testErrors_82[which(testErrors_82 == min(testErrors_82))]

# selection
model_m82 <- model_mlp82[[which(testErrors_82 == min(testErrors_82))]]

model_m82

# //!SECTION
