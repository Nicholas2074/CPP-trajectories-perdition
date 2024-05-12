# //ANCHOR - mortality

# data preparation
text_adm1 <- c("Variable", "group2", "group3", "group4", "group5")

text_adm2 <- c("P value", "< 0.001", "< 0.001", "< 0.001", "0.152")

text_adm3 <- c(
	"OR(95% CI)",
	"0.20 (0.09, 0.45)",
	"0.11 (0.05, 0.26)",
	"0.12 (0.05, 0.29)",
	"0.32 (0.07, 1.53)"
)

foresttext_adm <- data.frame(
	"Variable" = text_adm1,
	"P value" = text_adm2,
	"OR(95% CI)" = text_adm3
)

value_adm1 <- c(NA, c(0.20, 0.11, 0.12, 0.32))

value_adm2 <- c(NA, c(0.09, 0.05, 0.05, 0.07))

value_adm3 <- c(NA, c(0.45, 0.26, 0.29, 1.53))

# plot
library(forestplot)

forest_adm <- forestplot(
	labeltext = as.matrix(foresttext_adm),
	mean = value_adm1,
	lower = value_adm2,
	upper = value_adm3,
	zero = 1,
	boxsize = 0.15,
	lineheight = unit(7, "mm"),
	colgap = unit(2, "mm"),
	lwd.zero = 1.5,
	lwd.ci = 2,
	col = fpColors(
		box = "#458B00",
		summary = "#8B008B",
		lines = "black",
		zero = "#7AC5CD"
	),
	xlab = "Odds Ratio",
	lwd.xaxis = 1,
	txt_gp = fpTxtGp(
		ticks = gpar(cex = 0.8),
		xlab = gpar(cex = 0.8),
		cex = 1.0
	),
	lty.ci = "solid",
	title = "Adjusted model (hospital mortality)",
	line.margin = 0.08,
	graph.pos = 2
)
forest_adm

# //ANCHOR - dis_gcs

# data preparation
text_addg1 <- c("Variable", "group2", "group3", "group4", "group5")

text_addg2 <- c("P value", "0.003", "< 0.007", "< 0.026", "0.242")

text_addg3 <- c(
	"OR(95% CI)",
	"0.33 (0.16, 0.67)",
	"0.39 (0.19, 0.76)",
	"0.44 (0.21, 0.90)",
	"0.44 (0.10, 1.76)"
)

foresttext_addg <- data.frame(
	"Variable" = text_addg1,
	"P value" = text_addg2,
	"OR(95% CI)" = text_addg3
)

value_addg1 <- c(NA, c(0.33, 0.39, 0.44, 0.44))

value_addg2 <- c(NA, c(0.16, 0.19, 0.21, 0.10))

value_addg3 <- c(NA, c(0.67, 0.76, 0.90, 1.76))

# plot
library(forestplot)

forest_addg <- forestplot(
	labeltext = as.matrix(foresttext_addg),
	mean = value_addg1,
	lower = value_addg2,
	upper = value_addg3,
	zero = 1,
	boxsize = 0.15,
	lineheight = unit(7, "mm"),
	colgap = unit(2, "mm"),
	lwd.zero = 1.5,
	lwd.ci = 2,
	col = fpColors(
		box = "#458B00",
		summary = "#8B008B",
		lines = "black",
		zero = "#7AC5CD"
	),
	xlab = "Odds Ratio",
	lwd.xaxis = 1,
	txt_gp = fpTxtGp(
		ticks = gpar(cex = 0.8),
		xlab = gpar(cex = 0.8),
		cex = 1.0
	),
	lty.ci = "solid",
	title = "Adjusted model (discharge GCS)",
	line.margin = 0.08,
	graph.pos = 2
)
forest_addg

# //ANCHOR - dev_gcs

# data preparation
text_adeg1 <- c("Variable", "group2", "group3", "group4", "group5")

text_adeg2 <- c("P value", "0.915", "< 0.026", "< 0.054", "0.167")

text_adeg3 <- c(
	"OR(95% CI)",
	"0.96 (0.45, 2.03)",
	"0.44 (0.21, 0.91)",
	"0.48 (0.22, 1.01)",
	"0.34 (0.07, 1.57)"
)

foresttext_adeg <- data.frame(
	"Variable" = text_adeg1,
	"P value" = text_adeg2,
	"OR(95% CI)" = text_adeg3
)

value_adeg1 <- c(NA, c(0.96, 0.44, 0.48, 0.34))

value_adeg2 <- c(NA, c(0.45, 0.21, 0.22, 0.07))

value_adeg3 <- c(NA, c(2.03, 0.91, 1.01, 1.57))

# plot
library(forestplot)

forest_adeg <- forestplot(
	labeltext = as.matrix(foresttext_adeg),
	mean = value_adeg1,
	lower = value_adeg2,
	upper = value_adeg3,
	zero = 1,
	boxsize = 0.15,
	lineheight = unit(7, "mm"),
	colgap = unit(2, "mm"),
	lwd.zero = 1.5,
	lwd.ci = 2,
	col = fpColors(
		box = "#458B00",
		summary = "#8B008B",
		lines = "black",
		zero = "#7AC5CD"
	),
	xlab = "Odds Ratio",
	lwd.xaxis = 1,
	txt_gp = fpTxtGp(
		ticks = gpar(cex = 0.8),
		xlab = gpar(cex = 0.8),
		cex = 1.0
	),
	lty.ci = "solid",
	title = "Adjusted model (GCS difference)",
	line.margin = 0.08,
	graph.pos = 2
)
forest_adeg

# //ANCHOR - comortality

# data preparation
text_co1 <- c("Variable", "class 1", "age", "charlson", "ventriculostomy")

text_co2 <- c("P value", "< 0.001", "0.955", "0.010", "0.045")

text_co3 <- c(
	"OR(95% CI)",
	"5.86 (3.07, 11.37)",
	"1.00 (0.98, 1.02)",
	"1.36 (1.08, 1.73)",
	"1.78 (1.01, 3.13)"
)

foresttext_co <- data.frame(
	"Variable" = text_co1,
	"P value" = text_co2,
	"OR(95% CI)" = text_co3
)

value_co1 <- c(NA, c(5.86, 1.00, 1.36, 1.78))

value_co2 <- c(NA, c(3.07, 0.98, 1.08, 1.01))

value_co3 <- c(NA, c(11.37, 1.02, 1.73, 3.13))

# plot
library(forestplot)

forest_co <- forestplot(
	labeltext = as.matrix(foresttext_co),
	mean = value_co1,
	lower = value_co2,
	upper = value_co3,
	zero = 1,
	boxsize = 0.15,
	lineheight = unit(7, "mm"),
	colgap = unit(2, "mm"),
	lwd.zero = 1.5,
	lwd.ci = 2,
	col = fpColors(
		box = "#458B00",
		summary = "#8B008B",
		lines = "black",
		zero = "#7AC5CD"
	),
	xlab = "Odds Ratio",
	lwd.xaxis = 1,
	txt_gp = fpTxtGp(
		ticks = gpar(cex = 0.8),
		xlab = gpar(cex = 0.8),
		cex = 1.0
	),
	lty.ci = "solid",
	title = "Adjusted model (hospital mortality)",
	line.margin = 0.08,
	graph.pos = 2
)
forest_co
