# //SECTION - feature

# //ANCHOR - unique

# cpp_cv

names(ecpp_cv)[1] <- "icuid"

names(mcpp_cv)[1] <- "icuid"

cpp_cv <- rbind(ecpp_cv, mcpp_cv)

cpp_cv_0 <- merge(foreign_key, cpp_cv, by = "icuid", all.x = TRUE)

# cpp_avg

names(ecpp_avg)[1] <- "icuid"

names(mcpp_avg)[1] <- "icuid"

cpp_avg <- rbind(ecpp_avg, mcpp_avg)

cpp_avg_0 <- merge(foreign_key, cpp_avg, by = "icuid", all.x = TRUE)

# icp_cv

names(eicp_cv)[1] <- "icuid"

names(micp_cv)[1] <- "icuid"

icp_cv <- rbind(eicp_cv, micp_cv)

icp_cv_0 <- merge(foreign_key, icp_cv, by = "icuid", all.x = TRUE)

# icp_avg

names(eicp_avg)[1] <- "icuid"

names(micp_avg)[1] <- "icuid"

icp_avg <- rbind(eicp_avg, micp_avg)

icp_avg_0 <- merge(foreign_key, icp_avg, by = "icuid", all.x = TRUE)

# //ANCHOR - others

foreign_key <- mortality_1[, 1]

names(foreign_key)[1] <- "icuid"

# patient

names(epatient)[1] <- "icuid"

names(mpatient)[1] <- "icuid"

patient <- rbind(epatient, mpatient)

patient_0 <- merge(foreign_key, patient, by = "icuid", all.x = TRUE)

# diagnosis

names(ediagnosis)[1] <- "icuid"

names(mdiagnosis)[1] <- "icuid"

diagnosis <- rbind(ediagnosis, mdiagnosis)

diagnosis_0 <- merge(foreign_key, diagnosis, by = "icuid", all.x = TRUE)

# score

names(escore)[1] <- "icuid"

names(mscore)[1] <- "icuid"

score <- rbind(escore, mscore)

score_0 <- merge(foreign_key, score, by = "icuid", all.x = TRUE)

# treatment

names(etreatment)[1] <- "icuid"

names(mtreatment)[1] <- "icuid"

treatment <- rbind(etreatment, mtreatment)

treatment_0 <- merge(foreign_key, treatment, by = "icuid", all.x = TRUE)

# vital

names(evital)[1] <- "icuid"

names(mvital)[1] <- "icuid"

vital <- rbind(evital, mvital)

vital_0 <- merge(foreign_key, vital, by = "icuid", all.x = TRUE)

# lab

names(elab)[1] <- "icuid"

names(mlab)[1] <- "icuid"

lab <- rbind(elab, mlab)

lab_0 <- merge(foreign_key, lab, by = "icuid", all.x = TRUE)

# //ANCHOR - merge

# baseline
merged_1 <- merge(patient_0, score_0, by = "icuid", all = TRUE)
merged_2 <- merge(merged_1, diagnosis_0, by = "icuid", all = TRUE)
merged_3 <- merge(merged_2, treatment_0, by = "icuid", all = TRUE)
# feature
merged_4 <- merge(merged_3, vital_0, by = "icuid", all = TRUE)
merged_5 <- merge(merged_4, lab_0, by = "icuid", all = TRUE)
merged_6 <- merge(merged_5, cpp_cv_0, by = "icuid", all = TRUE)
merged_7 <- merge(merged_6, cpp_avg_0, by = "icuid", all = TRUE)
merged_8 <- merge(merged_7, icp_cv_0, by = "icuid", all = TRUE)
merged_9 <- merge(merged_8, icp_avg_0, by = "icuid", all = TRUE)

# //ANCHOR - preprocess

# treatment
feature_0 <- merged_9
feature_0[, 32:34][is.na(feature_0[, 32:34])] <- 0

# race
# black 1
# white 2
# caucasian 3
# hispanic 4
# unknow 5
feature_0$race <-
	ifelse(
		feature_0$race %in% c(
			"African American",
			"BLACK/AFRICAN AMERICAN",
			"BLACK/CARIBBEAN ISLAND",
			"BLACK/CAPE VERDEAN"
		),
		1,
		ifelse(
			feature_0$race %in% c("WHITE", "WHITE - OTHER EUROPEAN"),
			2,
			ifelse(
				feature_0$race == "Caucasian",
				3,
				ifelse(
					feature_0$race %in% c(
						"Hispanic",
						"HISPANIC/LATINO - PUERTO RICAN",
						"HISPANIC OR LATINO"
					),
					4,
					5
				)
			)
		)
	)

# gcs
feature_0$gcs <- as.factor(cut(feature_0$gcs, breaks = c(3, 8, 12, 15)))

# //ANCHOR - factorization
# gender[, 3]
# race[, 5]
# gcs[, 11]
# delirium[, 14]
# diseases[, 15:31]
# treatment[, 32:34]
feature_0[, c(3, 5, 11, 14, 15:31, 32:34)] <- lapply(feature_0[, c(3, 5, 11, 14, 15:31, 32:34)], as.factor)

# //ANCHOR - delete
# delete icu/hosp_los_hours, eyes, verbal, motor
feature_1 <- feature_0[, -c(6, 7, 8, 9, 10)]

feature_na <- colMeans(is.na(feature_1))

col_del <- names(feature_na[feature_na > 0.48])
col_del

feature_2 <- feature_1[, !names(feature_1) %in% col_del]

# //ANCHOR - imputation
library(missForest)

feature_2 <- missForest(feature_2)$ximp

# //ANCHOR - traj group
# regroup feature
group_2 <- group_1 %>%
	mutate(group = ifelse(group == 1 | group == 5, 1, 0))

feature_3 <- merge(group_2, feature_2, by = "icuid", all.x = TRUE)

# origroup feature
feature_4 <- merge(group_1, feature_2, by = "icuid", all.x = TRUE)

# //!SECTION

# //SECTION - baseline

# //ANCHOR - table group

# including patient, score, diagnosis, treatment, icp_related
baseline_0 <- feature_3[, c(1:30, 82:85)]

library(compareGroups)

table11 <- descrTable(group ~ . - icuid - id,
	data = baseline_0,
	method = NA,
	show.all = TRUE
)
table11 # Chi-squared approximation may be incorrect

export2word(table11, file = "table11.docx")

# //ANCHOR - table mortality

baseline_1 <- merge(baseline_0, mortality_1, by = c("icuid", "id", "group"), all = FALSE)

table12 <- descrTable(hosp_mortality ~ . - icuid - id - group,
	data = baseline_1,
	method = NA,
	show.all = TRUE
)
table12 # Chi-squared approximation may be incorrect

export2word(table12, file = "table12.docx")

# //ANCHOR - table feat
library(compareGroups)

table2 <- descrTable(group ~ . - icuid - id,
	data = feature_3,
	method = NA,
	show.all = TRUE
)
table2 # Chi-squared approximation may be incorrect

export2word(table2, file = "table2.docx")

# //!SECTION

# //SECTION - boruta

# differ
cat_cols <- c(2, 5, 7, 8, 11:20, 23:25, 28:30)
cont_cols <- c(2, 4, 6, 9, 10, 31:85)

# encoding
library(caret)

dummy_model <- dummyVars(~., data = feature_3[, cat_cols])

feature_cat <- predict(dummy_model, newdata = feature_3[, cat_cols]) # 43 vars

# normalizing
feature_cont <- feature_3[, cont_cols] # 60 vars

feature_cont[, -1] <- scale(feature_cont[, -1])

# merge
feature_4 <- merge(feature_cat, feature_cont, by = "id") # 102 vars

feature_5 <- merge(regroup, feature_4, by = "id", all.x = TRUE)

# boruta
library(Boruta)

set.seed(20231226)

feat <- Boruta(group ~ . - icuid - id,
	data = feature_5,
	pValue = 0.01,
	maxRuns = 2000
)
# feat

# feat$finalDecision

feat$finalDecision[feat$finalDecision == "Confirmed"]
# isbp_cv  chloride     paco2        pt    cpp_cv   cpp_avg
# Confirmed Confirmed Confirmed Confirmed Confirmed Confirmed

feat$finalDecision[feat$finalDecision == "Tentative"]
# NULL

Boruta::plotImpHistory(feat)

plot(feat, xlab = "Attributes", ylab = "Importance:Z-Score", las = 2)

# //ANCHOR - reshape

devtools::install_github("Tong-Chen/YSX")

library(dplyr)

boruta.imp <- function(x) {
	imp <- reshape2::melt(x$ImpHistory, na.rm = TRUE)[, -1]

	colnames(imp) <- c("Variable", "Importance")

	imp <- imp[is.finite(imp$Importance), ]

	variableGrp <- data.frame(
		Variable = names(x$finalDecision),
		finalDecision = x$finalDecision
	)

	showGrp <- data.frame(
		Variable = c("shadowMax", "shadowMean", "shadowMin"),
		finalDecision = c("shadowMax", "shadowMean", "shadowMin")
	)

	variableGrp <- rbind(variableGrp, showGrp)

	boruta.variable.imp <- merge(imp, variableGrp, all.x = T)

	sortedVariable <- boruta.variable.imp %>%
		group_by(Variable) %>%
		summarise(median = median(Importance)) %>%
		arrange(median)

	sortedVariable <- as.vector(sortedVariable$Variable)

	boruta.variable.imp$Variable <-
		factor(boruta.variable.imp$Variable, levels = sortedVariable)

	invisible(boruta.variable.imp)
}

boruta.variable.imp <- boruta.imp(feat)

library(YSX)

sp_boxplot(
	boruta.variable.imp,
	melted = TRUE,
	xvariable = "Variable",
	yvariable = "Importance",
	legend_variable = "finalDecision",
	legend_variable_order = c(
		"shadowMax", "shadowMean", "shadowMin", "Confirmed"
	),
	xtics_angle = 90
)

# subset scaled
feat_0 <- feature_3[, c(
	"id",
	"icuid",
	"group",
	"isbp_cv",
	"chloride",
	"paco2",
	"pt",
	"cpp_cv",
	"cpp_avg"
)]

# subset original
feat_rcs <- feature_3[, c(
	"group",
	"isbp_cv",
	"chloride",
	"paco2",
	"pt",
	"cpp_cv",
	"cpp_avg"
)]
