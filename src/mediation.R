library(mediation)
library(haven)

# Load the dataset
data = read_dta("data/causalmech.dta")
attach(data)

# 1) ESTIMATION FOR FEMALES:

indicator=female==1 
y=exhealth30[indicator==1]
d=treat[indicator==1]
m=work2year2q[indicator==1]
x=cbind(schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat, edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis, hhinc12, hhinc8, fdstamp, welf1, welf2, publicass)[indicator==1,]
w=cbind(emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,  health1212, health123,  pe_prb12, pe_prb12mis,  narry1, numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis)[indicator==1,]

# Combine confounders
confounders <- cbind(x, w)

# Step 1: Fit the mediator model (outcome = mediator, predictor = treatment + confounders)
mediator_model <- glm(m ~ d + confounders, data = data, family = binomial(link = "probit"))

# Step 2: Fit the outcome model (outcome = y, predictors = treatment + mediator + confounders)
outcome_model <- glm(y ~ d + m + confounders, data = data, family = binomial(link = "probit"))

# Step 3: Perform mediation analysis
set.seed(123)  # for reproducibility
mediation_result <- mediate(
  model.m = mediator_model,
  model.y = outcome_model,
  treat = "d",
  mediator = "m",
  sims = 1000,  # number of simulations
  boot = TRUE   # use bootstrap
)

# Summarize results
summary(mediation_result)
# From summary output
summary_stats <- summary(mediation_result)

# MSE = SE^2 (assuming unbiased estimator)
mse_total <- (summary_stats$tau.ci[2] - summary_stats$tau.ci[1])/(2*1.96)^2
mse_ade <- (summary_stats$z0.ci[2] - summary_stats$z0.ci[1])/(2*1.96)^2
mse_acme <- (summary_stats$d1.ci[2] - summary_stats$d1.ci[1])/(2*1.96)^2



# 2) ESTIMATION FOR MALES:

indicator=female==0
y=exhealth30[indicator==1]
d=treat[indicator==1]
m=work2year2q[indicator==1]
x=cbind(schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat, edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis, hhinc12, hhinc8, fdstamp, welf1, welf2, publicass)[indicator==1,]
w=cbind(emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,  health1212, health123,  pe_prb12, pe_prb12mis,  narry1, numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis)[indicator==1,]

# Combine confounders
confounders <- cbind(x, w)

# Step 1: Fit the mediator model (outcome = mediator, predictor = treatment + confounders)
mediator_model <- glm(m ~ d + confounders, data = data, family = binomial(link = "probit"))

# Step 2: Fit the outcome model (outcome = y, predictors = treatment + mediator + confounders)
outcome_model <- glm(y ~ d + m + confounders, data = data, family = binomial(link = "probit"))

# Step 3: Perform mediation analysis
set.seed(123)  # for reproducibility
mediation_result <- mediate(
  model.m = mediator_model,
  model.y = outcome_model,
  treat = "d",
  mediator = "m",
  sims = 1000,  # number of simulations
  boot = TRUE   # use bootstrap
)

# Summarize results
summary(mediation_result)
# From summary output
summary_stats <- summary(mediation_result)

# MSE = SE^2 (assuming unbiased estimator)
mse_total <- (summary_stats$tau.ci[2] - summary_stats$tau.ci[1])/(2*1.96)^2
mse_ade <- (summary_stats$z0.ci[2] - summary_stats$z0.ci[1])/(2*1.96)^2
mse_acme <- (summary_stats$d1.ci[2] - summary_stats$d1.ci[1])/(2*1.96)^2

# 1) Estimation for females
indicator <- female == 1

# Define outcome, treatment, mediator
y <- exhealth30[indicator == 1]
d <- treat[indicator == 1]
m <- work2year2q[indicator == 1]

# Define pre-treatment covariates (named)
x <- data.frame(
  schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,
  pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat,
  edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis,
  hhinc12, hhinc8, fdstamp, welf1, welf2, publicass
)[indicator == 1, ]

# Post-treatment covariates
w <- data.frame(
  emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,
  health1212, health123, pe_prb12, pe_prb12mis, narry1,
  numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis
)[indicator == 1, ]

# Combine confounders
confounders <- cbind(x, w)

# Full dataset for modeling
female_data <- data.frame(d = d, m = m, y = y, confounders)

# Step 1: Mediator model
mediator_model <- glm(m ~ d + ., data = female_data[, c("m", "d", names(confounders))],
                      family = binomial(link = "probit"))

# Step 2: Outcome model
outcome_model <- glm(y ~ d + m + ., data = female_data[, c("y", "d", "m", names(confounders))],
                     family = binomial(link = "probit"))

# Step 3: Conditional mediation analysis (condition on all pre-treatment covariates)
# We'll fix all pre-treatment covariates to their sample mean (or median for categorical)
pre_treatment_covariates <- x
covariate_profile <- lapply(pre_treatment_covariates, function(col) {
  if (is.numeric(col)) mean(col, na.rm = TRUE) else {
    # For factors or logicals, use mode
    as.numeric(names(sort(table(col), decreasing = TRUE))[1])
  }
})

# Convert to named list
names(covariate_profile) <- names(pre_treatment_covariates)

# Perform mediation analysis
set.seed(123)
mediation_result <- mediate(
  model.m = mediator_model,
  model.y = outcome_model,
  treat = "d",
  mediator = "m",
  covariates = covariate_profile,
  sims = 1000,
  boot = TRUE
)

# Summary
summary(mediation_result)

# 2) Estimation for males
indicator <- female == 0

# Define outcome, treatment, mediator
y <- exhealth30[indicator == 1]
d <- treat[indicator == 1]
m <- work2year2q[indicator == 1]

# Define pre-treatment covariates (named)
x <- data.frame(
  schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,
  pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat,
  edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis,
  hhinc12, hhinc8, fdstamp, welf1, welf2, publicass
)[indicator == 1, ]

# Post-treatment covariates
w <- data.frame(
  emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,
  health1212, health123, pe_prb12, pe_prb12mis, narry1,
  numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis
)[indicator == 1, ]

# Combine confounders
confounders <- cbind(x, w)

# Full dataset for modeling
male_data <- data.frame(d = d, m = m, y = y, confounders)

# Step 1: Mediator model
mediator_model <- glm(m ~ d + ., data = male_data[, c("m", "d", names(confounders))],
                      family = binomial(link = "probit"))

# Step 2: Outcome model
outcome_model <- glm(y ~ d + m + ., data = male_data[, c("y", "d", "m", names(confounders))],
                     family = binomial(link = "probit"))

# Step 3: Conditional mediation analysis (condition on all pre-treatment covariates)
# We'll fix all pre-treatment covariates to their sample mean (or median for categorical)
pre_treatment_covariates <- x
covariate_profile <- lapply(pre_treatment_covariates, function(col) {
  if (is.numeric(col)) mean(col, na.rm = TRUE) else {
    # For factors or logicals, use mode
    as.numeric(names(sort(table(col), decreasing = TRUE))[1])
  }
})

# Convert to named list
names(covariate_profile) <- names(pre_treatment_covariates)

# Perform mediation analysis
set.seed(123)
mediation_result <- mediate(
  model.m = mediator_model,
  model.y = outcome_model,
  treat = "d",
  mediator = "m",
  covariates = covariate_profile,
  sims = 1000,
  boot = TRUE
)

# Summary
summary(mediation_result)