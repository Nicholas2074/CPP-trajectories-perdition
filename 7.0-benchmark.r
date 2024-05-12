# //SECTION - logistic
feat_6 <- feat_4

feat_6$group <- ifelse(feat_6$group == 1, 0, 1)

model_log <- glm(group ~ ., data = feat_6, family = binomial)

model_log

summary(model_log)

# validation
library(pROC)

val_log <- predict(model_log, feat_5[, -1])

roc_log <- roc(feat_5$group, as.numeric(val_log))

res_log <- coords(roc_log, "best", ret = eva)

ci_log <- as.data.frame(ci.coords(roc_log, "best", ret = eva))

res_log
# specificity sensitivity  accuracy
# threshold   0.8372093   0.5897436 0.7195122

# //!SECTION

# //SECTION - rsnns

# //ANCHOR - sum

# for each algorithm
# only keep the best tuning results

round(res_m12, 3)
round(ci.auc(roc_m12), 3) # 034569
round(ci_m12, 3)

round(res_m22, 3)
round(ci.auc(roc_m22), 3) # 750062
round(ci_m22, 3)

round(res_m32, 3)
round(ci.auc(roc_m32), 3) # A68900
round(ci_m32, 3)

round(res_m41, 3)
round(ci.auc(roc_m41), 3) # 0fa408
round(ci_m41, 3)

round(res_m51, 3)
round(ci.auc(roc_m51), 3) # fb8502
round(ci_m51, 3)

round(res_m61, 3)
round(ci.auc(roc_m61), 3) # 444444
round(ci_m61, 3)

round(res_m71, 3)
round(ci.auc(roc_m71), 3) # 64AAD0
round(ci_m71, 3)

round(res_m82, 3)
round(ci.auc(roc_m82), 3) # D962C7
round(ci_m82, 3)

round(res_r1, 3)
round(ci.auc(roc_r1), 3) # FFE773
round(ci_r1, 3)

round(res_log, 3)
round(ci.auc(roc_log), 3) # b5f2b5
round(ci_log, 3)

# //ANCHOR - bootstrap

recall <- 0.590
precision <- 0.767
recall_ci <- c(0.410, 0.744)
precision_ci <- c(0.679, 0.938)

n_boot <- 2000

f1_scores <- numeric(n_boot)

set.seed(20240124)

for (i in 1:n_boot) {
	sampled_recall <- runif(1, recall_ci[1], recall_ci[2])
	sampled_precision <- runif(1, precision_ci[1], precision_ci[2])

	f1_scores[i] <- 2 * (sampled_recall * sampled_precision) / (sampled_recall + sampled_precision)
}

f1_ci <- quantile(f1_scores, c(0.025, 0.975))

round(f1_ci, 3)


# ---------------------------------------------------------------------------- #
#                             model_m82 is the best                            #
# ---------------------------------------------------------------------------- #

# //ANCHOR - summary

library(RSNNS)

model_m82

summary(model_m82)

weightMatrix(model_m82)

extractNetInfo(model_m82)

# prediction
pre_m82 <- predict(model_m82, data_snns$inputsTest)

# confusion matrix
confusionMatrix(data_snns$targetsTrain, fitted.values(model_m82)) # train data

confusionMatrix(data_snns$targetsTest, pre_m82) # test data

# 402040-method
confusionMatrix(
	data_snns$targetsTrain,
	encodeClassLabels(
		fitted.values(model_m82),
		method = "402040",
		l = 0.4,
		h = 0.6
	)
)

# //ANCHOR - graph

par(mfrow = c(2, 2))

plotIterativeError(model_m82)

plotRegressionError(pre_m82[, 2], data_snns$targetsTest[, 2])
# optimal straight line is represented in black
# linear fit to the actual data is shown in red

plotROC(fitted.values(model_m82)[, 2], data_snns$targetsTrain[, 2])

plotROC(pre_m82[, 2], data_snns$targetsTest[, 2])

par(mfrow = c(1, 1))

# //ANCHOR - visualization

library(NeuralNetTools)

# neuros
plotnet(
	model_m82,
	circle_cex = 2.40, # size of nodes
	cex_val = 0.50, # size of text
	rel_rsc = 0.25, # size of lines
	# max_sp = TRUE,
	pos_col = "red",
	meg_col = "grey"
)

# importance
olden(model_m82)

# //ANCHOR - roc
library(pROC)

val_m82 <- predict(model_m82, feat_5[, -1])

roc_m82 <- roc(feat_5$group, as.numeric(val_m82[, 2]))

pROC::plot.roc(
	roc_m82,
	print.auc = TRUE,
	auc.polygon = TRUE,
	grid = c(0.1, 0.2),
	grid.col = c("blue", "black"),
	auc.polygon.col = "pink",
	print.thres = TRUE,
	legacy.axe = TRUE,
	xlab = "False Positive Rate (Positive label: 1)",
	ylab = "True Positive Rate (Positive label: 1)"
)

res_m82

# //SECTION - bench_roc

library(pROC)

plot.roc(roc_log,
	col = "#b5f2b5",
	xlab = "False Positive Rate (Positive label: 1)",
	ylab = "True Positive Rate (Positive label: 1)"
)

plot.roc(roc_m12, add = TRUE, col = "#034569")

plot.roc(roc_m22, add = TRUE, col = "#750062")

plot.roc(roc_m32, add = TRUE, col = "#A68900")

plot.roc(roc_m51, add = TRUE, col = "#fb8502")

plot.roc(roc_m61, add = TRUE, col = "#444444")

plot.roc(roc_m71, add = TRUE, col = "#64AAD0")

plot.roc(roc_m82, add = TRUE, col = "#D962C7")

plot.roc(roc_r1, add = TRUE, col = "#FFE773")

# color of legend
color_bench <- c(
	"Logistic Regression" = "#b5f2b5",
	"Std_Backpropagation" = "#034569",
	"BackpropBatch" = "#750062",
	"BackpropChunk" = "#A68900",
	"BackpropMomentum" = "#fb8502",
	"BackpropWeightDecay" = "#444444",
	"Rprop" = "#64AAD0",
	"Quickprop" = "#D962C7",
	"SCG" = "#FFE773"
)

# add legend
x <- 0.25
y <- 0.55

legend(
	x,
	y,
	legend = c(
		"LR",
		"Std_Backpropagation",
		"BackpropBatch",
		"BackpropChunk",
		"BackpropMomentum",
		"BackpropWeightDecay",
		"Rprop",
		"Quickprop",
		"SCG"
	),
	col = color_bench,
	lty = 1,
	bty = "1",
	lwd = 3,
	cex = 0.8,
	text.col = "black"
)

# //!SECTION

# //!SECTION

# //SECTION - dalex

# //ANCHOR - global explanations

library(DALEX)

set.seed(20240127)

model_dalex <- DALEX::explain(model_m82,
	data = feat_4[, 2:7],
	y = feat_4$group,
	type = "classification",
	label = "SCG"
)

dalex_imp <- model_parts(
	explainer = model_dalex,
	loss_function = loss_root_mean_square,
)

plot(dalex_imp)

# //ANCHOR - local explanations

# waterfall plot
old <- set_theme_dalex("ema")

old <- set_theme_dalex("drwhy")

# 5, 9

shap_scg_pos <- predict_parts(
	explainer = model_dalex,
	new_observation = feat_4[176, 2:7],
	type = "shap"
)

shap_scg_neg <- predict_parts(
	explainer = model_dalex,
	new_observation = feat_4[94, 2:7],
	type = "shap"
)

# force plot
library(shapviz)

shap_viz_pos <- shapviz(shap_scg_pos)

sv_force(shap_viz_pos)

shap_viz_neg <- shapviz(shap_scg_neg)

sv_force(shap_viz_neg)