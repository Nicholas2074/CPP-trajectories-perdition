# //SECTION - mortality

# //ANCHOR - preprocessing

# mortality
names(emortality)[1] <- "icuid"

names(mmortality)[1] <- "icuid"

mortality <- rbind(emortality, mmortality)

# link group
mortality_0 <- merge(group_1, mortality, by = "icuid", all = FALSE)

# correction
mortality_0$hosp_mortality[is.na(mortality_0$hosp_mortality)] <- 0

# factorization
mortality_0$group <- as.factor(mortality_0$group)

# group1 as ref
mortality_0$group <- relevel(mortality_0$group, ref = "1")

# //ANCHOR - multinom

library(nnet)

model_mortality <- multinom(hosp_mortality ~ group, data = mortality_0)

## summary ####

summary(model_mortality)

# z value
z_m <- summary(model_mortality)$coefficients / summary(model_mortality)$standard.errors

# 2-tailed z test
p_m <- (1 - pnorm(abs(z_m), 0, 1)) * 2

# or value
or_m <- exp(coef(model_mortality))

# 95% ci
ci_m <- exp(confint(model_mortality))

# //SECTION - dis_gcs

# //ANCHOR - preprocessing

# dis_gcs
names(edev_gcs)[1] <- "icuid"

names(mdev_gcs)[1] <- "icuid"

gcs <- rbind(edev_gcs, mdev_gcs)

# link group
gcs_0 <- merge(group_1, gcs, by = "icuid", all = FALSE)

# assignment
dis_gcs <- gcs_0[, c(1, 2, 3, 5)]

# factorization
dis_gcs$group <- as.factor(dis_gcs$group)

# group1 as ref
dis_gcs$group <- relevel(dis_gcs$group, ref = "1")

# relabel
dis_gcs$dis_gcs <- ifelse(dis_gcs$dis_gcs <= 8, 2,
	ifelse(dis_gcs$dis_gcs >= 9 &
		dis_gcs$dis_gcs <= 12, 1, 0)
)

# factorization
dis_gcs$dis_gcs <- as.factor(dis_gcs$dis_gcs)

# //ANCHOR - plor

library(MASS)

model_disgcs <- polr(dis_gcs ~ group, data = dis_gcs, Hess = TRUE)

## summary ####

summary(model_disgcs)

# coef ste and t value
para_dg <- coef(summary(model_disgcs)) # coef() is differ from coef(summary())

# 2-tailed t test
p_dg <- pnorm(abs(para_dg[, 3]), lower.tail = F) * 2

# or value
or_dg <- exp(coef(model_disgcs))

# 95% ci
ci_dg <- exp(confint(model_disgcs))

# //SECTION - dev_gcs

# //ANCHOR - preprocessing

# dev_gcs
dev_gcs <- gcs_0[, c(1, 2, 3, 6)]

# factorization
dev_gcs$group <- as.factor(dev_gcs$group)

# group1 as ref
dev_gcs$group <- relevel(dev_gcs$group, ref = "1")

# relabel
dev_gcs$dev_gcs <- ifelse(dev_gcs$dev_gcs <= 0, 1, 0)

# //ANCHOR - multinom

library(nnet)

model_devgcs <- multinom(dev_gcs ~ group, data = dev_gcs, family = binomial)

## summary ####

summary(model_devgcs)

# z value
z_eg <- summary(model_devgcs)$coefficients / summary(model_devgcs)$standard.errors

# 2-tailed z test
p_eg <- (1 - pnorm(abs(z_eg), 0, 1)) * 2

# or value
or_eg <- exp(coef(model_devgcs))

# 95% ci
ci_eg <- exp(confint(model_devgcs))

# //SECTION - comortality

# //ANCHOR - preprocessing

# relabel
library(magrittr)
library(dplyr)

mortality_1 <- mortality_0 %>%
	mutate(group = ifelse(group == 1 | group == 5, 2, 1))

# factorization
mortality_1$group <- as.factor(mortality_1$group)

# class0 as ref
mortality_1$group <- relevel(mortality_1$group, ref = "0")

# //ANCHOR - glm
model_comortality <- glm(hosp_mortality ~ group, data = mortality_1, family = binomial)

## summary ####

summary(model_comortality)

p_co <- coef(summary(model_comortality))[, "Pr(>|z|)"]

or_co <- exp(coef(model_comortality))

ci_co <- exp(confint(model_comortality))

result_co <- cbind(p_co, or_co, ci_co)

# //SECTION - forestplot

# //ANCHOR - mortality
# data preparation
text_name <- c("Variable", "group2", "group3", "group4", "group5")

text_m2 <- c("P value", "< 0.001", "< 0.001", "< 0.001", "0.169")

text_m3 <- c(
	"OR(95% CI)",
	"0.20 (0.09, 0.44)",
	"0.14 (0.07, 0.31)",
	"0.16 (0.07, 0.36)",
	"0.35 (0.08, 1,57)"
)

foresttext_m <- data.frame(
	"Variable" = text_name,
	"P value" = text_m2,
	"OR(95% CI)" = text_m3
)

value_m1 <- c(NA, 0.20, 0.14, 0.16, 0.35)

value_m2 <- c(NA, 0.09, 0.07, 0.07, 0.08)

value_m3 <- c(NA, 0.44, 0.31, 0.36, 1.57)

# plot
library(forestplot)

forest_m <- forestplot(
	labeltext = as.matrix(foresttext_m),
	mean = value_m1,
	lower = value_m2,
	upper = value_m3,
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
	title = "Crude model (hospital mortality)",
	line.margin = 0.08,
	graph.pos = 2
)
forest_m

# //!SECTION
