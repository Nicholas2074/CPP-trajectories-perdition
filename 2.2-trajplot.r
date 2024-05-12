# 2.2 graphic ####

# 2.2.1 ori ####

# color of traj
trans <- "70"
col1 <- "#034569"
col1_1 <- paste0("#64AAD0", trans)
col2 <- "#750062"
col2_1 <- paste0("#D962C7", trans)
col3 <- "#A68900"
col3_1 <- paste0("#FFE773", trans)
col4 <- "#0fa408"
col4_1 <- paste0("#b5f2b5", trans)
col5 <- "#fb8502"
col5_1 <- paste0("#fbbf8f", trans)
# col6 <- "#444444"
# col6_1 <- paste0("#cccccc", trans)
cols1 <- c(col1_1, col2_1, col3_1, col4_1, col5_1)
cols2 <- c(col1, col2, col3, col4, col5)
vcol <- c(cols1, cols2)

sol_5
# Call TrajeR with 5 groups and a 2,1,1,2,0 degrees of polynomial shape of trajectory.
# Model : Censored Normal
# Method : Expectation-maximization

# group intercept
# 1 67.06365
# 2 65.86659
# 3 74.51985
# 4 77.25001
# 5 86.96081

# plot trajectory
library(trajeR)

plotrajeR(sol_5,
	col = vcol,
	# Y = cocpp_1[, 3:32],
	# A = cocpp_1[, 33:62],
	xlab = "Hours after ICU admission",
	ylab = "CPP"
)

# color of legend
color_mapping <- c(
	"Group 5" = "#fb8502",
	"Group 4" = "#0fa408",
	"Group 3" = "#A68900",
	"Group 2" = "#750062",
	"Group 1" = "#034569"
)

plot(1, type = "n")

# add legend
legend(
	"center",
	legend = c("group 5", "group 4", "group 3", "group 2", "group 1"),
	col = color_mapping,
	lty = 1,
	bty = "1",
	lwd = 2,
	cex = 0.8,
	text.col = "black",
	ncol = 3
)

# 2.2.2 del ####

## eicu ####

# calculate the ir
eq1 <- quantile(ecpp_2$avg_cpp, 0.25, na.rm = TRUE)
eq3 <- quantile(ecpp_2$avg_cpp, 0.75, na.rm = TRUE)

# calculate the iqr
eiqr <- eq3 - eq1

# calculate the limitation
eupper_limit <- eq3 + 1.5 * eiqr
elower_limit <- eq1 - 1.5 * eiqr

# del the data beyond limitation
ecpp_del <- within(ecpp_2, {
	avg_cpp[avg_cpp < elower_limit | avg_cpp > eupper_limit] <- NA
})

# imputation
library(mice)

set.seed(20230107)

imp <- mice(ecpp_del, method = "rf", m = 20)

ecpp_del <- complete(imp)

# convert the long format to wide format
library(tidyr)

ecpp_del <- ecpp_del %>%
	pivot_wider(names_from = interval, values_from = avg_cpp)

# assign id to each patient
ecpp_del <- ecpp_del %>%
	mutate(id = row_number()) %>%
	select(id, everything())

# rename
names(ecpp_del)[2] <- "icuid"

## mimic ####

# calculate the ir
mq1 <- quantile(mcpp_2$avg_cpp, 0.25, na.rm = TRUE)
mq3 <- quantile(mcpp_2$avg_cpp, 0.75, na.rm = TRUE)

# calculate the iqr
miqr <- mq3 - mq1

# calculate the limitation
mupper_limit <- mq3 + 1.5 * miqr
mlower_limit <- mq1 - 1.5 * miqr

# del the data beyond limitation
mcpp_del <- within(mcpp_2, {
	avg_cpp[avg_cpp < mlower_limit | avg_cpp > mupper_limit] <- NA
})

# imputation
library(mice)

set.seed(20230107)

imp <- mice(mcpp_del, method = "rf", m = 20)

mcpp_del <- complete(imp)

# convert the long format to wide format
mcpp_del <- mcpp_del %>%
	pivot_wider(names_from = interval, values_from = avg_cpp)

# assign id to each patient
mcpp_del <- mcpp_del %>%
	mutate(id = 249 + row_number() - 1) %>%
	select(id, everything())

# rename
names(mcpp_del)[2] <- "icuid"

## bind ####
cocpp_del <- rbind(ecpp_del, mcpp_del) # id 1-248: eicu, id 249-331: mimic

# change the column names of 3-32 to 4, 8, 12...120
colnames(cocpp_del)[3:32] <- seq(4, 120, by = 4)

# create an empty data frame with 331 rows
cocpp_time <- data.frame(id = 1:331)

# gene 30 columns
# with time increasing by a step of 4
for (i in seq(4, 120, by = 4)) {
	col_name <- as.character(i)
	cocpp_time[col_name] <- rep(i, 331)
}

# combine
cocpp_del <- as.matrix(merge(cocpp_del, cocpp_time, by = "id"))

## plot ####

library(trajeR)

sol_del <- trajeR(
	Y = cocpp_del[, 3:32],
	A = cocpp_del[, 33:62],
	degre = c(2, 1, 1, 2, 0),
	Model = "CNORM",
	Method = "EM",
	ssigma = FALSE,
	hessian = TRUE,
	itermax = 1000,
	ProbIRLS = TRUE
)

# plot trajectory
plotrajeR(sol_del,
	col = vcol,
	Y = cocpp_del[, 3:32],
	A = cocpp_del[, 33:62],
	xlab = "Hours after ICU admission",
	ylab = "CPP",
	mean = FALSE,
	alpha = 1
)
