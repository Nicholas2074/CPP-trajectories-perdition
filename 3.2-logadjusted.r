# //ANCHOR - boruta

adjusted_co <- merge(mortality_1, adjusted_regroup, by = c("icuid", "id", "group"), all = FALSE)

adjusted_regroup <- feature_3[, c(1:4, 6, 10, 12, 23, 24, 29)]

adjusted_origroup <- feature_4[, c(1:4, 6, 10, 12, 23, 24, 29)]

library(Boruta)

set.seed(20240107)

boruta_ad <- Boruta(hosp_mortality ~ . - icuid - id,
	data = adjusted_co,
	pValue = 0.01,
	maxRuns = 2000
)
boruta_ad

boruta_ad$finalDecision

boruta_ad$finalDecision[boruta_ad$finalDecision == "Confirmed"]
# group             age        charlson ventriculostomy
# Confirmed       Confirmed       Confirmed       Confirmed

# selection
# age[, 4], bmi[, 6], charlson[, 10]
# cerebrovascular_disease[, 12], renal_disease[, 23], diabetes[, 24]
# ventriculostomy[, 29]
# from Yang et al., 2022

# //ANCHOR - comortality

# vars from boruta
model_adco <-
	glm(
		hosp_mortality ~
			group +
			age +
			# bmi +
			charlson +
			# cerebrovascular_disease +
			# renal_disease +
			# diabetes +
			ventriculostomy,
		data = adjusted_co,
		family = binomial
	)

# summary

summary(model_adco)

p_adco <- coef(summary(model_adco))[, "Pr(>|z|)"]

or_adco <- exp(coef(model_adco))

ci_adco <- exp(confint(model_adco))

result_adco <- cbind(p_adco, or_adco, ci_adco)

# //ANCHOR - mortality

adjusted_mor <- merge(mortality_0, adjusted_origroup, by = c("icuid", "id", "group"), all = FALSE)

# multinom
library(nnet)

model_admor <-
	multinom(
		hosp_mortality ~
			group +
			age +
			charlson +
			ventriculostomy,
		data = adjusted_mor,
		family = binomial
	)

# summary

summary(model_admor)

# z value
z_adm <- summary(model_admor)$coefficients / summary(model_admor)$standard.errors

# 2-tailed z test
p_adm <- (1 - pnorm(abs(z_adm), 0, 1)) * 2

# or value
or_adm <- exp(coef(model_admor))

# 95% ci
ci_adm <- exp(confint(model_admor))

# //ANCHOR - dis_gcs

adjusted_disgcs <- merge(dis_gcs, adjusted_origroup, by = c("icuid", "id", "group"), all = FALSE)

# plor

library(MASS)

model_addg <- polr(
	dis_gcs ~
		group +
		age +
		charlson +
		ventriculostomy,
	data = adjusted_disgcs,
	Hess = TRUE
)

# summary

summary(model_addg)

# coef ste and t value
para_addg <- coef(summary(model_addg)) # coef() is differ from coef(summary())

# 2-tailed t test
p_addg <- pnorm(abs(para_addg[, 3]), lower.tail = F) * 2

# or value
or_addg <- exp(coef(model_addg))

# 95% ci
ci_addg <- exp(confint(model_addg))

# //ANCHOR - dev_gcs

adjusted_devgcs <- merge(dev_gcs, adjusted_origroup, by = c("icuid", "id", "group"), all = FALSE)

# multinom

library(nnet)

model_adeg <-
	multinom(
		dev_gcs ~
			group +
			age +
			charlson +
			ventriculostomy,
		data = adjusted_devgcs,
		family = binomial
	)

# summary

summary(model_adeg)

# z value
z_adeg <- summary(model_adeg)$coefficients / summary(model_adeg)$standard.errors

# 2-tailed z test
p_adeg <- (1 - pnorm(abs(z_adeg), 0, 1)) * 2

# or value
or_adeg <- exp(coef(model_adeg))

# 95% ci
ci_adeg <- exp(confint(model_adeg))



