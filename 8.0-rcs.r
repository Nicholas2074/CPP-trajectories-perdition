# //ANCHOR - paco2
library(rcssci)

rcssci_logistic(
	knot = 3,
	data = feat_rcs,
	y = "group",
	x = "paco2",
	covs = c("isbp_cv", "chloride", "pt", "cpp_cv", "cpp_avg"),
	prob = 0.1,
	filepath = "D:/"
)

# //ANCHOR - chloride

# irrelevant!!!

# rcs
library(rcssci)

rcssci_logistic(
	knot = 3,
	data = feat_rcs,
	y = "group",
	x = "chloride",
	covs = c("paco2", "isbp_cv", "pt", "cpp_cv", "cpp_avg"),
	prob = 0.1,
	filepath = "D:/"
)

# p for non-linear > 0.05

# glm

model_chloride <-
	glm(
		group ~
			chloride +
			paco2 +
			isbp_cv +
			pt +
			cpp_cv +
			cpp_avg,
		data = feat_rcs,
		family = binomial
	)

# summary

summary(model_chloride)

p_chloride <- coef(summary(model_chloride))[, "Pr(>|z|)"]

or_chloride <- exp(coef(model_chloride))

ci_chloride <- exp(confint(model_chloride))

result_chloride <- cbind(p_chloride, or_chloride, ci_chloride)

# //ANCHOR - isbpcv

# irrelevant!!!

# //ANCHOR - pt

# //ANCHOR - cpp_cv

library(rcssci)

rcssci_logistic(
	knot = 3,
	data = feat_rcs,
	y = "group",
	x = "cpp_cv",
	covs = c("paco2", "pt", "chloride", "isbp_cv", "cpp_avg"),
	prob = 0.1,
	filepath = "D:/"
)

# //ANCHOR - cpp_avg

library(rcssci)

rcssci_linear(
	knot = 3,
	data = feat_rcs,
	y = "group",
	x = "cpp_avg",
	covs = c("paco2", "pt", "chloride", "isbp_cv", "cpp_cv"),
	prob = 0.1,
	filepath = "D:/"
)
