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