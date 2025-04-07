library(causalweight)

# Load the dataset
data=read.dta("data/causalmech.dta")
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

mediation_result <- medDML(
  y = y,          # Outcome variable (exhealth30)
  d = d,          # Treatment variable (treat)
  m = m,          # Mediator variable (work2year2q)
  x = confounders, # Confounders (all pre-treatment covariates)
  trim = 0
)
mediation_result_trimmed <- medDML(
  y = y,          # Outcome variable (exhealth30)
  d = d,          # Treatment variable (treat)
  m = m,          # Mediator variable (work2year2q)
  x = confounders, # Confounders (all pre-treatment covariates)
  trim = 0.05
)
mediation_result_pretreat <- medDML(
  y = y, 
  d = d, 
  m = m, 
  x = x,  # Only pre-treatment covariates
  trim = 0
)
mediation_result_pretreat_trimmed <- medDML(
  y = y, 
  d = d, 
  m = m, 
  x = x,  # Only pre-treatment covariates
  trim = 0.05
)
# Summarize results
summary(mediation_result)
mediation_result$results
mediation_result_trimmed$results
mediation_result_pretreat$results
mediation_result_pretreat_trimmed$results
mse_total <- (0.01625071)^2
mse_dir_treat <- (0.01623779)^2 
mse_dir_control <- (0.01627597)^2
mse_indir_treat <- (0.0007100627)^2
mse_indir_control <- (0.002508323)^2

# Estimates
theta_total <- 0.02609086
theta_dir_treat <- 0.02790119
theta_dir_control <- 0.02459912
theta_indir_treat <- 0.0014917421
theta_indir_control <- -0.001810328

# Standard errors
se_total <- 0.01625071
se_dir_treat <- 0.01623779
se_dir_control <- 0.01627597
se_indir_treat <- 0.0007100627
se_indir_control <- 0.002508323

# 95% CIs
ci_total <- c(theta_total - 1.96 * se_total, theta_total + 1.96 * se_total)
ci_dir_treat <- c(theta_dir_treat - 1.96 * se_dir_treat, theta_dir_treat + 1.96 * se_dir_treat)
ci_dir_control <- c(theta_dir_control - 1.96 * se_dir_control, theta_dir_control + 1.96 * se_dir_control)
ci_indir_treat <- c(theta_indir_treat - 1.96 * se_indir_treat, theta_indir_treat + 1.96 * se_indir_treat)
ci_indir_control <- c(theta_indir_control - 1.96 * se_indir_control, theta_indir_control + 1.96 * se_indir_control)

# 2) ESTIMATION FOR MALES:

indicator=female==0 
y=exhealth30[indicator==1]
d=treat[indicator==1]
m=work2year2q[indicator==1]
x=cbind(schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat, edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis, hhinc12, hhinc8, fdstamp, welf1, welf2, publicass)[indicator==1,]
w=cbind(emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,  health1212, health123,  pe_prb12, pe_prb12mis,  narry1, numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis)[indicator==1,]

# Combine confounders
confounders <- cbind(x, w)

mediation_result <- medDML(
  y = y,          # Outcome variable (exhealth30)
  d = d,          # Treatment variable (treat)
  m = m,          # Mediator variable (work2year2q)
  x = confounders, # Confounders (all pre-treatment covariates)
  trim = 0
)
mediation_result_trimmed <- medDML(
  y = y,          # Outcome variable (exhealth30)
  d = d,          # Treatment variable (treat)
  m = m,          # Mediator variable (work2year2q)
  x = confounders, # Confounders (all pre-treatment covariates)
  trim = 0.05
)
mediation_result_pretreat <- medDML(
  y = y, 
  d = d, 
  m = m, 
  x = x,  # Only pre-treatment covariates
  trim = 0
)
mediation_result_pretreat_trimmed <- medDML(
  y = y, 
  d = d, 
  m = m, 
  x = x,  # Only pre-treatment covariates
  trim = 0.05
)
# Summarize results
summary(mediation_result)
mediation_result$results
mediation_result_trimmed$results
mediation_result_pretreat$results
mediation_result_pretreat_trimmed$results