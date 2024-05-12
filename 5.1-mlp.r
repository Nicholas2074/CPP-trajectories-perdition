# ---------------------------------------------------------------------------- #
#                      all the followings are tuned model                      #
# ---------------------------------------------------------------------------- #

# //ANCHOR - 01-Std_Backpropagation

# modeling
library(RSNNS)

model_mlp1 <- mlp(
	data_snns$inputsTrain,
	data_snns$targetsTrain,
	size = c(15, 5),
	learnFunc = "Std_Backpropagation",
	learnFuncParams = c(0.1, 0.1),
	maxit = 200,
	inputsTest = data_snns$inputsTest,
	targetsTest = data_snns$targetsTest
)

# validation

eva <- c("accuracy", "recall", "precision", "youden")

# 1 layer
model_m11
summary(model_m11)

library(pROC)

val_m11 <- predict(model_m11, feat_5[, -1])

roc_m11 <- roc(feat_5$group, as.numeric(val_m11[, 2]))

res_m11 <- coords(roc_m11, "best", ret = eva)

res_m11
# specificity sensitivity  accuracy
# threshold   0.8604651   0.6923077 0.7804878

# 2 layers
model_m12
summary(model_m12)

library(pROC)

val_m12 <- predict(model_m12, feat_5[, -1])

roc_m12 <- roc(feat_5$group, as.numeric(val_m12[, 2]))

res_m12 <- coords(roc_m12, "best", ret = eva)

ci_m12 <- as.data.frame(ci.coords(roc_m12, "best", ret = eva))

res_m12
# specificity sensitivity accuracy
# threshold    0.744186   0.8717949 0.804878

# //ANCHOR - 02-BackpropBatch

# modeling
library(RSNNS)

model_mlp2 <- mlp(
	data_snns$inputsTrain,
	data_snns$targetsTrain,
	size = c(14, 9),
	learnFunc = "BackpropBatch",
	learnFuncParams = c(0.00316, 0.1),
	maxit = 200,
	inputsTest = data_snns$inputsTest,
	targetsTest = data_snns$targetsTest
)

# validation
# 1 layer
model_m21
summary(model_m21)

library(pROC)

val_m21 <- predict(model_m21, feat_5[, -1])

roc_m21 <- roc(feat_5$group, as.numeric(val_m21[, 2]))

res_m21 <- coords(roc_m21, "best", ret = eva)

res_m21
# specificity sensitivity  accuracy
# threshold   0.5116279   0.6153846 0.5609756

# 2 layers
model_m22
summary(model_m22)

library(pROC)

val_m22 <- predict(model_m22, feat_5[, -1])

roc_m22 <- roc(feat_5$group, as.numeric(val_m22[, 2]))

res_m22 <- coords(roc_m22, "best", ret = eva)

ci_m22 <- as.data.frame(ci.coords(roc_m22, "best", ret = eva, best.policy = "random"))

res_m22
#           specificity sensitivity  accuracy
# threshold   0.9302326   0.2564103 0.6097561

# //ANCHOR - 03-BackpropChunk

# modeling
library(RSNNS)

model_mlp3 <- mlp(
	data_snns$inputsTrain,
	data_snns$targetsTrain,
	size = c(15, 13),
	learnFunc = "BackpropChunk",
	learnFuncParams = c(0.1, 0.1),
	maxit = 200,
	inputsTest = data_snns$inputsTest,
	targetsTest = data_snns$targetsTest
)

# validation
# 1 layer
model_m31
summary(model_m31)

library(pROC)

val_m31 <- predict(model_m31, feat_5[, -1])

roc_m31 <- roc(feat_5$group, as.numeric(val_m31[, 2]))

res_m31 <- coords(roc_m31, "best", ret = eva)

res_m31
# specificity sensitivity accuracy
# threshold   0.7906977   0.8205128 0.804878

# 2 layers
model_m32
summary(model_m32)

library(pROC)

val_m32 <- predict(model_m32, feat_5[, -1])

roc_m32 <- roc(feat_5$group, as.numeric(val_m32[, 2]))

res_m32 <- coords(roc_m32, "best", ret = eva)

ci_m32 <- as.data.frame(ci.coords(roc_m32, "best", ret = eva))

res_m32
# specificity sensitivity  accuracy
# threshold    0.744186   0.8974359 0.8170732

# //ANCHOR - 04-BackpropMomentum

# modeling
library(RSNNS)

model_mlp4 <- mlp(
	data_snns$inputsTrain,
	data_snns$targetsTrain,
	size = c(9, 15),
	learnFunc = "BackpropMomentum",
	learnFuncParams = c(0.1, 0.1),
	maxit = 200,
	inputsTest = data_snns$inputsTest,
	targetsTest = data_snns$targetsTest
)

# validation
# 1 layer
model_m41
summary(model_m41)

library(pROC)

val_m41 <- predict(model_m41, feat_5[, -1])

roc_m41 <- roc(feat_5$group, as.numeric(val_m41[, 2]))

res_m41 <- coords(roc_m41, "best", ret = eva)

ci_m41 <- as.data.frame(ci.coords(roc_m41, "best", ret = eva))

res_m41
# specificity sensitivity  accuracy
# threshold   0.7209302   0.8717949 0.7926829

# 2 layers
model_m42
summary(model_m42)

library(pROC)

val_m42 <- predict(model_m42, feat_5[, -1])

roc_m42 <- roc(feat_5$group, as.numeric(val_m42[, 2]))

res_m42 <- coords(roc_m42, "best", ret = eva)

res_m42
# specificity sensitivity  accuracy
# threshold   0.8139535   0.7692308 0.7926829

# //ANCHOR - 05-BackpropWeightDecay

# modeling
library(RSNNS)

model_mlp5 <- mlp(
	data_snns$inputsTrain,
	data_snns$targetsTrain,
	size = 12,
	learnFunc = "BackpropWeightDecay",
	learnFuncParams = c(0.092, 0.1),
	maxit = 200,
	inputsTest = data_snns$inputsTest,
	targetsTest = data_snns$targetsTest
)

# validation
# 1 layer
model_m51
summary(model_m51)

library(pROC)

val_m51 <- predict(model_m51, feat_5[, -1])

roc_m51 <- roc(feat_5$group, as.numeric(val_m51[, 2]))

res_m51 <- coords(roc_m51, "best", ret = eva)

ci_m51 <- as.data.frame(ci.coords(roc_m51, "best", ret = eva, best.policy = "random"))

res_m51
# specificity sensitivity  accuracy
# threshold   0.6046512   0.4871795 0.5487805

# 2 layers
model_m52
summary(model_m52)

library(pROC)

val_m52 <- predict(model_m52, feat_5[, -1])

roc_m52 <- roc(feat_5$group, as.numeric(val_m52[, 2]))

res_m52 <- coords(roc_m52, "best", ret = eva)

res_m52
# specificity sensitivity  accuracy
# 1           0           1 0.4756098
# 2           1           0 0.5243902

# //ANCHOR - 06-Rprop

# modeling
library(RSNNS)

model_mlp6 <- mlp(
	data_snns$inputsTrain,
	data_snns$targetsTrain,
	size = 9,
	learnFunc = "Rprop",
	learnFuncParams = c(0.056, 0.1),
	maxit = 200,
	inputsTest = data_snns$inputsTest,
	targetsTest = data_snns$targetsTest
)

# validation
# 1 layer
model_m61
summary(model_m61)

library(pROC)

val_m61 <- predict(model_m61, feat_5[, -1])

roc_m61 <- roc(feat_5$group, as.numeric(val_m61[, 2]))

res_m61 <- coords(roc_m61, "best", ret = eva)

ci_m61 <- as.data.frame(ci.coords(roc_m61, "best", ret = eva))

res_m61
# specificity sensitivity  accuracy
# threshold   0.7209302   0.9230769 0.8170732

# 2 layers
model_m62
summary(model_m62)

library(pROC)

val_m62 <- predict(model_m62, feat_5[, -1])

roc_m62 <- roc(feat_5$group, as.numeric(val_m62[, 2]))

res_m62 <- coords(roc_m62, "best", ret = eva)

res_m62
# specificity sensitivity accuracy
# threshold   0.7209302   0.8974359 0.804878

# //ANCHOR - 07-Quickprop

# modeling
library(RSNNS)

model_mlp7 <- mlp(
	data_snns$inputsTrain,
	data_snns$targetsTrain,
	size = 10,
	learnFunc = "Quickprop",
	learnFuncParams = c(0.082, 0.1),
	maxit = 200,
	inputsTest = data_snns$inputsTest,
	targetsTest = data_snns$targetsTest
)

# validation
# 1 layer
model_m71
summary(model_m71)

library(pROC)

val_m71 <- predict(model_m71, feat_5[, -1])

roc_m71 <- roc(feat_5$group, as.numeric(val_m71[, 2]))

res_m71 <- coords(roc_m71, "best", ret = eva)

ci_m71 <- as.data.frame(ci.coords(roc_m71, "best", ret = eva))

res_m71
# specificity sensitivity accuracy
# threshold   0.9302326   0.6666667 0.804878

# 2 layers
model_m72
summary(model_m72)

library(pROC)

val_m72 <- predict(model_m72, feat_5[, -1])

roc_m72 <- roc(feat_5$group, as.numeric(val_m72[, 2]))

res_m72 <- coords(roc_m72, "best", ret = eva)

res_m72
# specificity sensitivity  accuracy
# threshold   0.8604651   0.6410256 0.7560976

# //ANCHOR - 08-SCG

# modeling
library(RSNNS)

model_mlp8 <- mlp(
	data_snns$inputsTrain,
	data_snns$targetsTrain,
	size = c(5, 2),
	learnFunc = "SCG",
	learnFuncParams = c(0.1, 2.0, 0.001, 0.1),
	maxit = 200,
	inputsTest = data_snns$inputsTest,
	targetsTest = data_snns$targetsTest
)

# validation
# 1 layer
model_m81
summary(model_m81)

library(pROC)

val_m81 <- predict(model_m81, feat_5[, -1])

roc_m81 <- roc(feat_5$group, as.numeric(val_m81[, 2]))

res_m81 <- coords(roc_m81, "best", ret = eva)

res_m81
# specificity sensitivity  accuracy
# threshold   0.6976744   0.8717949 0.7804878

# 2 layers
model_m82 # best
summary(model_m82)

library(pROC)

val_m82 <- predict(model_m82, feat_5[, -1])

roc_m82 <- roc(feat_5$group, as.numeric(val_m82[, 2]))

res_m82 <- coords(roc_m82, "best", ret = eva)

ci_m82 <- as.data.frame(ci.coords(roc_m82, "best", ret = eva))

res_m82
# specificity sensitivity  accuracy
# threshold   0.7906977   0.9487179 0.8658537
