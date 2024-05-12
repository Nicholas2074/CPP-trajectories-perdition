# //ANCHOR - norm

feat_1 <- feat_0[, -c(1, 2)]

# norm
feat_1[, -1] <- normalizeData(feat_1[, -1], type = "norm")

# 01
# feat_1[, -1] <-
#   apply(feat_1[, -1], 2, function(x)
#     (x - min(x)) / (max(x) - min(x)))

# norm is better than 01

# //ANCHOR - smote

library(smotefamily)

feat_1$group <- as.numeric(feat_1$group) # 0, 1 to 1, 2

smote_data <- SMOTE(feat_1, feat_1$group, K = 5, dup_size = 0)

feat_2 <- smote_data$data

# //ANCHOR - spliting
feat_3 <- feat_2[, -8]

set.seed(20230108)

trainrows <- sample(nrow(feat_3), 0.85 * nrow(feat_3))

feat_4 <- feat_3[trainrows, ] # for training and validation

feat_5 <- feat_3[-trainrows, ] # for testing

library(RSNNS)

dataValues <- feat_4[, 2:7]

dataTargets <- decodeClassLabels(feat_4[, 1])

set.seed(0114)

data_snns <-
	splitForTrainingAndTest(dataValues, dataTargets, ratio = 0.18)
