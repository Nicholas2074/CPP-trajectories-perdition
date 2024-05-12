# 2.4 sensitive ####

## preprocessing ####
patient_elder <- as.data.frame(patient_0[patient_0$age >= 45, 1])

colnames(patient_elder) <- "icuid"

cocpp_2 <- merge(cocpp_1, patient_elder, by = "icuid", all = FALSE)

cocpp_2 <- as.matrix(cocpp_2)

## modeling ####
library(trajeR)

sol_elder <- trajeR(
	Y = cocpp_2[, 3:32],
	A = cocpp_2[, 33:62],
	degre = c(2, 1, 1, 2, 0),
	Model = "CNORM",
	Method = "EM",
	ssigma = FALSE,
	hessian = TRUE,
	itermax = 1000,
	ProbIRLS = TRUE
)

## summary ####
adequacy(
	sol_elder,
	Y = cocpp_2[, 3:32],
	A = cocpp_2[, 33:62],
	nb = 10000,
	alpha = 0.98
)
# 1            2            3           4            5
# Prob. est. 1.910945e-01 1.923475e-01 2.239387e-01   0.2244681 1.681512e-01
# CI inf.    1.551534e-13 4.179503e-13 7.056243e-11   0.9999988 6.431599e-17
# CI sup.    2.831568e-09 7.193095e-09 1.202474e-06   1.0000000 1.056481e-12
# Prop.      1.623377e-01 1.688312e-01 3.181818e-01   0.3181818 3.246753e-02
# AvePP      9.486792e-01 9.536329e-01 9.612004e-01   0.9822395 9.815110e-01
# OCC        9.678839e+01 1.026659e+02 5.289507e+01 116.8099019 1.582586e+03

propAssign(sol_elder, Y = cocpp_2[, 3:32], A = cocpp_2[, 33:62])
#  1         2         3         4          5
# [1,] 0.1623377 0.1688312 0.3181818 0.3181818 0.03246753

## plot ####
library(trajeR)

plotrajeR(sol_elder,
	col = vcol,
	# Y = cocpp_1[, 3:32],
	# A = cocpp_1[, 33:62],
	xlab = "Hours after ICU admission",
	ylab = "CPP"
)
