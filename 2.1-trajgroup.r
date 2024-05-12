# //SECTION - import

# //ANCHOR - eicu

# convert the long format to wide format
ecpp_4 <- ecpp_3 %>%
	pivot_wider(names_from = interval, values_from = avg_cpp)

# assign id to each patient
ecpp_5 <- ecpp_4 %>%
	mutate(id = row_number()) %>%
	select(id, everything())

# rename
names(ecpp_5)[2] <- "icuid"

# //ANCHOR - mimic

# convert the long format to wide format
mcpp_4 <- mcpp_3 %>%
	pivot_wider(names_from = interval, values_from = avg_cpp)

# assign id to each patient
mcpp_5 <- mcpp_4 %>%
	mutate(id = 249 + row_number() - 1) %>%
	select(id, everything())

# rename
names(mcpp_5)[2] <- "icuid"

# //ANCHOR - merge

cocpp_0 <- rbind(ecpp_5, mcpp_5) # id 1-248: eicu, id 249-331: mimic

# change the column names of 3-32 to 4, 8, 12...120
colnames(cocpp_0)[3:32] <- seq(4, 120, by = 4)

# create an empty data frame with 331 rows
cocpp_time <- data.frame(id = 1:331)

# gene 30 columns
# with time increasing by a step of 4
for (i in seq(4, 120, by = 4)) {
	col_name <- as.character(i)
	cocpp_time[col_name] <- rep(i, 331)
}

# combine
cocpp_1 <- as.matrix(merge(cocpp_0, cocpp_time, by = "id"))

# //!SECTION

# //SECTION - tuning

# //ANCHOR - ng = 2

# para gene
degre_2 <- list()

for (j in 0:4) {
	for (k in 0:4) {
		degre_2[[length(degre_2) + 1]] <- c(j, k)
	}
}

# model selection

library(doParallel)

# clear foreach env
env <- foreach:::.foreachGlobals
rm(list = ls(name = env), pos = env)

# cores for parallel computing
num_cores <- detectCores()
registerDoParallel(cores = num_cores)

sol_list_2 <- list()

sol_list_2 <- foreach(i = 1:25, .packages = "trajeR") %dopar% {
	sol <- trajeR(
		Y = cocpp_2[, 2:31],
		A = cocpp_2[, 32:61],
		degre = degre_2[[i]],
		Model = "CNORM",
		Method = "EM",
		ssigma = FALSE,
		hessian = TRUE,
		itermax = 100,
		ProbIRLS = TRUE
	)
}

# stop parallel computing
stopImplicitCluster()

# bic value
bic_2 <- list()

for (i in 1:25) {
	bic_2[[i]] <- trajeRBIC(sol_list_2[[i]])
}

# index of min bic
print(which(unlist(bic_2) == min(unlist(bic_2))))
# [1] 7

trajeRBIC(sol_list_2[[7]])
# [1] 90203.02

sol_list_2[[7]]
# > sol_list_2[[7]]
# Call TrajeR with 2 groups and a 1,1 degrees of polynomial shape of trajectory.

# //ANCHOR - ng = 3

# para gene
degre_3 <- list()

for (j in 0:4) {
	for (k in 0:4) {
		for (l in 0:4) {
			degre_3[[length(degre_3) + 1]] <- c(j, k, l)
		}
	}
}

# model selection

library(doParallel)

# clear env
env <- foreach:::.foreachGlobals
rm(list = ls(name = env), pos = env)

# cores for parallel computing
num_cores <- detectCores()
registerDoParallel(cores = num_cores)

sol_list_3 <- list()

sol_list_3 <- foreach(i = 1:125, .packages = "trajeR") %dopar% {
	sol <- trajeR(
		Y = cocpp_2[, 2:31],
		A = cocpp_2[, 32:61],
		degre = degre_3[[i]],
		Model = "CNORM",
		Method = "EM",
		ssigma = FALSE,
		hessian = TRUE,
		itermax = 100,
		ProbIRLS = TRUE
	)
}

# stop parallel computing
stopImplicitCluster()

# bic value
bic_3 <- list()

for (i in 1:125) {
	bic_3[[i]] <- trajeRBIC(sol_list_3[[i]])
}

# index of min bic
print(which(unlist(bic_3) == min(unlist(bic_3))))
# [1] 32

trajeRBIC(sol_list_3[[32]])
# [1] 89341.14

sol_list_3[[32]]
# > sol_list_3[[32]]
# Call TrajeR with 3 groups and a 1,1,1 degrees of polynomial shape of trajectory.

# //ANCHOR - ng = 4

# para gene
degre_4 <- list()

for (j in 0:4) {
	for (k in 0:4) {
		for (l in 0:4) {
			for (m in 0:4) {
				degre_4[[length(degre_4) + 1]] <- c(j, k, l, m)
			}
		}
	}
}

# model selection

library(doParallel)

# clear foreach env
env <- foreach:::.foreachGlobals
rm(list = ls(name = env), pos = env)

# cores for parallel computing
num_cores <- detectCores()
registerDoParallel(cores = num_cores)

sol_list_4 <- list()

# foreach loop
sol_list_4 <- foreach(i = 1:625, .packages = "trajeR") %dopar% {
	sol <- trajeR(
		Y = cocpp_2[, 2:31],
		A = cocpp_2[, 32:61],
		degre = degre_4[[i]],
		Model = "CNORM",
		Method = "EM",
		ssigma = FALSE,
		hessian = TRUE,
		itermax = 100,
		ProbIRLS = TRUE
	)
}

# stop parallel computing
stopImplicitCluster()

# bic value
bic_4 <- list()

for (i in 1:625) {
	bic_4[[i]] <- trajeRBIC(sol_list_4[[i]])
}

# index of min bic
print(which(unlist(bic_4) == min(unlist(bic_4))))
# [1] 282 raw

trajeRBIC(sol_list_4[[282]])
# [1] 88237.93

sol_list_4[[282]]
# > sol_list_4[[282]]
# Call TrajeR with 4 groups and a 2,1,1,1 degrees of polynomial shape of trajectory.

# //ANCHOR - ng = 5

# para gene
degre_5 <- list()

for (j in 0:4) {
	for (k in 0:4) {
		for (l in 0:4) {
			for (m in 0:4) {
				for (n in 0:4) {
					degre_5[[length(degre_5) + 1]] <- c(j, k, l, m, n)
				}
			}
		}
	}
}

# model selection

library(doParallel)

# clear env
env <- foreach:::.foreachGlobals
rm(list = ls(name = env), pos = env)

# cores for parallel computing
num_cores <- detectCores()
registerDoParallel(cores = num_cores)

sol_list_5 <- list()

# foreach loop
sol_list_5 <- foreach(i = 1:3125, .packages = "trajeR") %dopar% {
	sol <- trajeR(
		Y = cocpp_2[, 2:31],
		A = cocpp_2[, 32:61],
		degre = degre_5[[i]],
		Model = "CNORM",
		Method = "EM",
		ssigma = FALSE,
		hessian = TRUE,
		itermax = 100,
		ProbIRLS = TRUE
	)
}

# stop parallel computing
stopImplicitCluster()

bic_5 <- list()

for (i in 1:3125) {
	bic_5[[i]] <- trajeRBIC(sol_list_5[[i]])
}

# index of min bic
print(which(unlist(bic_5) == min(unlist(bic_5)))) # i = 1411

trajeRBIC(sol_list_5[[1411]])
# [1] 87731.39

sol_list_5[[1411]]
# Call TrajeR with 5 groups and a 2,1,1,2,0 degrees of polynomial shape of trajectory.

# //ANCHOR - ng = 6

# para gene
degre_6 <- list()

for (j in 0:4) {
	for (k in 0:4) {
		for (l in 0:4) {
			for (m in 0:4) {
				for (n in 0:4) {
					for (o in 0:4) {
						degre_6[[length(degre_6) + 1]] <- c(j, k, l, m, n, o)
					}
				}
			}
		}
	}
}

# model selection

library(doParallel)

# clear env
env <- foreach:::.foreachGlobals
rm(list = ls(name = env), pos = env)

# cores for parallel computing
num_cores <- detectCores()
registerDoParallel(cores = num_cores)

sol_list_6 <- list()

# foreach loop
sol_list_6 <- foreach(i = 1:15625, .packages = "trajeR") %dopar% {
	sol <- trajeR(
		Y = cocpp_2[, 2:31],
		A = cocpp_2[, 32:61],
		degre_6 = degre_6[[i]],
		Model = "CNORM",
		Method = "EM",
		ssigma = FALSE,
		hessian = TRUE,
		itermax = 100,
		ProbIRLS = TRUE
	)
}

# stop parallel computing
stopImplicitCluster()

bic_6 <- list()

for (i in 1:15625) {
	bic_6[[i]] <- trajeRBIC(sol_list_6[[i]])
}

# index of min bic
print(which(unlist(bic_6) == min(unlist(bic_6)))) # i = 7536

trajeRBIC(sol_list_6[[7536]])
# [1] 87442.7

sol_list_6[[7536]]
# Call TrajeR with 6 groups and a 2,2,0,1,2,0 degrees of polynomial shape of trajectory.

# trajeRSH() is not suitable for para selction
# normalization is not right for modeling
# from ng = 4, the normalized model is clearly incorrect

# sum
# ng = 2 degre = 1,1 bic = 78746.87
# ng = 3 degre = 1,1,1 bic = 77906.85
# ng = 4 degre = 2,1,1,1 bic = 77199.6
# ng = 5 degre = 2,1,1,2,0 bic = 76837.02
# ng = 6 degre = 2,2,0,1,2,0 bic = 76683.35

# //!SECTION

# //SECTION - modeling

library(trajeR)

# //ANCHOR - n = 2

sol_2 <- trajeR(
	Y = cocpp_1[, 3:32],
	A = cocpp_1[, 33:62],
	degre = c(1, 1),
	Model = "CNORM",
	Method = "EM",
	ssigma = FALSE,
	hessian = TRUE,
	itermax = 1000,
	ProbIRLS = TRUE
)
sol_2

# //ANCHOR - n = 3

sol_3 <- trajeR(
	Y = cocpp_1[, 3:32],
	A = cocpp_1[, 33:62],
	degre = c(1, 1, 1),
	Model = "CNORM",
	Method = "EM",
	ssigma = FALSE,
	hessian = TRUE,
	itermax = 1000,
	ProbIRLS = TRUE
)
sol_3

# //ANCHOR - n = 4

sol_4 <- trajeR(
	Y = cocpp_1[, 3:32],
	A = cocpp_1[, 33:62],
	degre = c(2, 1, 1, 1),
	Model = "CNORM",
	Method = "EM",
	ssigma = FALSE,
	hessian = TRUE,
	itermax = 1000,
	ProbIRLS = TRUE
)
sol_4

# //ANCHOR - n = 5

sol_5 <- trajeR(
	Y = cocpp_1[, 3:32],
	A = cocpp_1[, 33:62],
	degre = c(2, 1, 1, 2, 0),
	Model = "CNORM",
	Method = "EM",
	ssigma = FALSE,
	hessian = TRUE,
	itermax = 1000,
	ProbIRLS = TRUE
)
sol_5

# //ANCHOR - n = 6

sol_6 <- trajeR(
	Y = cocpp_1[, 3:32],
	A = cocpp_1[, 33:62],
	degre = c(2, 2, 0, 1, 2, 0),
	Model = "CNORM",
	Method = "EM",
	ssigma = FALSE,
	hessian = TRUE,
	itermax = 1000,
	ProbIRLS = TRUE
)
sol_6

# //ANCHOR - final model

# ---------------------------------------------------------------------------- #
#                               sol_5 is the best                              #
# ---------------------------------------------------------------------------- #

adequacy(
	sol_5,
	Y = cocpp_1[, 3:32],
	A = cocpp_1[, 33:62],
	nb = 10000,
	alpha = 0.98
)
# 1            2            3          4            5
# Prob. est. 1.874918e-01 2.116648e-01 2.226361e-01  0.2105451 1.676622e-01
# CI inf.    3.124794e-13 8.773203e-13 4.881392e-11  0.9999996 9.387751e-17
# CI sup.    1.864932e-09 5.577817e-09 3.640568e-07  1.0000000 5.638037e-13
# Prop.      1.329305e-01 2.658610e-01 3.172205e-01  0.2567976 2.719033e-02
# AvePP      9.420522e-01 9.357853e-01 9.245031e-01  0.9575177 9.994327e-01
# OCC        9.941023e+01 4.108705e+01 2.695870e+01 65.3280879 5.948431e+04

propAssign(sol_5, Y = cocpp_1[, 3:32], A = cocpp_1[, 33:62])
# 1        2         3         4          5
# [1,] 0.1329305 0.265861 0.3172205 0.2567976 0.02719033

# //ANCHOR - traj group

group_mat <- GroupProb(sol_5, Y = cocpp_1[, 3:32], A = cocpp_1[, 33:62])

group_0 <- cbind(cocpp_1[, c("id", "icuid")], group_mat)

# add group allocation col
library(dplyr)

group_1 <- as.data.frame(group_0)

# group_1$group <- apply(group_1[, 3:7], 1, function(x) {
#   which.max(x)
# })

group_1$group <- max.col(group_1[, 3:7]) # max.col() better

group_1 <- group_1[, c(1, 2, 8)]
