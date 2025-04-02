library(grf)
library(dplyr)
library(haven)

data = read_dta("data/causalmech.dta")
attach(data)
data_female <- subset(data, female == 1)

indicator=female==1 
y=exhealth30[indicator==1]
d=treat[indicator==1]
m=work2year2q[indicator==1]
x=cbind(schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat, edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis, hhinc12, hhinc8, fdstamp, welf1, welf2, publicass)[indicator==1,]
w=cbind(emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,  health1212, health123,  pe_prb12, pe_prb12mis,  narry1, numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis)[indicator==1,]

#y <- data_female$exhealth30
#d <- data_female$treat
#m <- data_female$work2year2q
#x <- data_female %>%
#  select(schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis, 
#         pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat, 
#         edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis, hhinc12, 
#         hhinc8, fdstamp, welf1, welf2, publicass)

cf_total <- causal_forest(X = as.matrix(x), Y = y, W = d, set.seed(123))  # Total effect
cf_direct <- causal_forest(X = as.matrix(x), Y = y, W = d, W.hat = predict(causal_forest(X = as.matrix(x), Y = m, W = d))$predictions, set.seed(123))  # Direct effect
cf_mediation <- causal_forest(X = as.matrix(x), Y = y, W = m, set.seed(123))  # Indirect effect

total_effect <- predict(cf_total)$predictions
direct_effect_treat <- predict(cf_direct)$predictions
indirect_effect_treat <- predict(cf_mediation)$predictions
#indirect_effect_treat <- predict(cf_mediation)$predictions * predict(causal_forest(X = as.matrix(x), Y = m, W = d))$predictions
direct_effect_control <- direct_effect_treat - total_effect
indirect_effect_control <- total_effect - direct_effect_control
baseline_y <- mean(y[d == 0 & m == 0])

se_total <- average_treatment_effect(cf_total)[2]
se_direct_treat <- average_treatment_effect(cf_direct)[2]
se_indirect_treat = average_treatment_effect(cf_mediation)[2]
#se_direct_control <- sd(direct_effect_control) / sqrt(length(direct_effect_control))
#se_indirect_treat <- sd(indirect_effect_treat) / sqrt(length(indirect_effect_treat))
#se_indirect_control <- sd(indirect_effect_control) / sqrt(length(indirect_effect_control))
se_baseline_y <- sd(y[d == 0 & m == 0]) / sqrt(sum(d == 0 & m == 0))

pval <- function(est, se) { 2 * pnorm(-abs(est/se)) }

pval_total <- pval(total_effect, se_total)
pval_direct_treat <- pval(direct_effect_treat, se_direct_treat)
#pval_direct_control <- pval(direct_effect_control, se_direct_control)
pval_indirect_treat <- pval(indirect_effect_treat, se_indirect_treat)
#pval_indirect_control <- pval(indirect_effect_control, se_indirect_control)
pval_baseline_y <- pval(baseline_y, se_baseline_y)

results <- data.frame(
  effect = c(
    mean(total_effect), 
    mean(direct_effect_treat), 
   # mean(direct_effect_control), 
    mean(indirect_effect_treat), 
    #mean(indirect_effect_control), 
    baseline_y
  ),
  se = c(
    average_treatment_effect(cf_total)[2],
    average_treatment_effect(cf_direct)[2],
    #sd(total_effect) / sqrt(length(total_effect)), 
    #sd(direct_effect_treat) / sqrt(length(direct_effect_treat)), 
    #sd(direct_effect_control) / sqrt(length(direct_effect_control)), 
    se_indirect_treat = average_treatment_effect(cf_mediation)[2],
    #sd(indirect_effect_treat) / sqrt(length(indirect_effect_treat)), 
    #sd(indirect_effect_control) / sqrt(length(indirect_effect_control)), 
    se_baseline_y
  ),
  p_val = c(
    pval(mean(total_effect), average_treatment_effect(cf_total)[2]), 
    pval(mean(direct_effect_treat), average_treatment_effect(cf_direct)[2]), 
    #pval(mean(direct_effect_control), sd(direct_effect_control) / sqrt(length(direct_effect_control))), 
    pval(mean(indirect_effect_treat), average_treatment_effect(cf_mediation)[2]), 
    #pval(mean(indirect_effect_control), sd(indirect_effect_control) / sqrt(length(indirect_effect_control))), 
    pval(baseline_y, se_baseline_y)
  )
)

#rownames(results) <- c("total", "dir.treat", "dir.control", "indir.treat", "indir.control", "Y(0,M(0))")
rownames(results) <- c("total", "dir.treat", "indir.treat", "Y(0,M(0))")

print(results)

