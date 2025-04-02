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