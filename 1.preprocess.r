# //SECTION - eicu

# //ANCHOR - import

# cpp from eicu
library(dplyr)
library(tidyr)
library(magrittr)

ecpp_0 <- data.frame(ecpp)

# start time is 0
# complete the time and fill the cpp value with null
ecpp_1 <- ecpp_0 %>%
	complete(patientunitstayid, interval = seq(0, 29), fill = list(avg_cpp = NA))

# keep the patients with avg_cpp num >= 10
ecpp_2 <- ecpp_1 %>%
	group_by(patientunitstayid) %>%
	filter(sum(!is.na(avg_cpp)) >= 10) %>%
	ungroup()

table(is.na(ecpp_2$avg_cpp)) # null ratio: 0.209543

# denoise
ecpp_2$avg_cpp <-
	ifelse(ecpp_2$avg_cpp < 0 |
		ecpp_2$avg_cpp >= 250, NA, ecpp_2$avg_cpp)

table(is.na(ecpp_2$avg_cpp)) # null ratio: 0.2111559

# //ANCHOR - imputation

# data distribution before imputation
# summary(ecpp_2$avg_cpp)
# hist(ecpp_2$avg_cpp, main = "eicu pre_imputation")
# qqnorm(ecpp_2$avg_cpp, main = "eicu pre_imputation")
# qqline(ecpp_2$avg_cpp, main = "eicu pre_imputation")
# table(is.na(ecpp_2$avg_cpp))

# total patients: 248
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    4.00   67.00   75.00   76.01   84.00  230.00    1571

# multi-imputation
library(mice)

set.seed(20231231)

imp <- mice(ecpp_2, method = "rf", m = 20)

ecpp_3 <- complete(imp)

# data distribution after imputation
# summary(ecpp_3$avg_cpp)
# hist(ecpp_3$avg_cpp, main = "eicu post_imputation")
# qqnorm(ecpp_3$avg_cpp, main = "eicu post_imputation")
# qqline(ecpp_3$avg_cpp, main = "eicu post_imputation")
# table(is.na(ecpp_3$avg_cpp))

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 4.00   67.00   75.00   76.06   84.00  230.00

# //!SECTION

# //SECTION - mimic

# //ANCHOR - import

# cpp from mimic
library(dplyr)
library(tidyr)
library(magrittr)

mcpp_0 <- data.frame(mcpp)

# starting time is 0
# complete the time and fill the cpp value with null
mcpp_1 <- mcpp_0 %>%
	complete(stay_id, interval = seq(0, 29), fill = list(avg_cpp = NA))

# keep the patients with avg_cpp num >= 10
mcpp_2 <- mcpp_1 %>%
	group_by(stay_id) %>%
	filter(sum(!is.na(avg_cpp)) >= 10) %>%
	ungroup()

table(is.na(mcpp_2$avg_cpp)) # null ratio: 0.2678715

mcpp_2$avg_cpp <-
	ifelse(mcpp_2$avg_cpp < 0 |
		mcpp_2$avg_cpp >= 250, NA, mcpp_2$avg_cpp)

table(is.na(mcpp_2$avg_cpp)) # null ratio: 0.2698795

# //ANCHOR - imputation

# data distribution before imputation
# summary(mcpp_2$avg_cpp)
# hist(mcpp_2$avg_cpp, main = "eicu pre_imputation")
# qqnorm(mcpp_2$avg_cpp, main = "eicu pre_imputation")
# qqline(mcpp_2$avg_cpp, main = "eicu pre_imputation")
# table(is.na(mcpp_2$avg_cpp))

# total patients: 83
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    0.00   63.00   70.00   70.40   77.75  124.00     672

# multi-imputation
library(mice)

set.seed(20231231)

imp <- mice(mcpp_2, method = "rf", m = 20)
# cause of the singular matrices
# rf method is used

mcpp_3 <- complete(imp)

# data distribution after imputation
# summary(mcpp_3$avg_cpp)
# hist(mcpp_3$avg_cpp, main = "eicu post_imputation")
# qqnorm(mcpp_3$avg_cpp, main = "eicu post_imputation")
# qqline(mcpp_3$avg_cpp, main = "eicu post_imputation")
# table(is.na(mcpp_3$avg_cpp))

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00   63.00   70.00   70.47   78.00  124.00

# //!SECTION
