# //ANCHOR - modeling
library(RSNNS)

model_rbf <- rbf(
	data_snns$inputsTrain,
	data_snns$targetsTrain,
	size = 6,
	maxit = 200,
	learnFuncParams = c(1e-08, 0, 1e-08, 0.067, 0.8)
)

# //ANCHOR - validation
# 1 layer
model_r1
summary(model_r1)

library(pROC)

val_r1 <- predict(model_r1, feat_5[, -1])

roc_r1 <- roc(feat_5$group, as.numeric(val_r1[, 2]))

res_r1 <- coords(roc_r1, "best", ret = eva)

ci_r1 <- as.data.frame(ci.coords(roc_r1, "best", ret = eva))

res_r1
# specificity sensitivity  accuracy
# threshold   0.8139535   0.5128205 0.6707317

# 2 layers
model_r2
summary(model_r2)

library(pROC)

val_r2 <- predict(model_r2, feat_5[, -1])

roc_r2 <- roc(feat_5$group, as.numeric(val_r2[, 2]))

res_r2 <- coords(roc_r2, "best", ret = eva)

res_r2
# specificity sensitivity  accuracy
# threshold   0.4883721   0.6666667 0.5731707
