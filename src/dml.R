library(foreign)
library(xtable)
library(np)
library(DoubleML)
library(grf)
library(mlr3)
library(mlr3learners)


#########################
# PROCEDURE "mediation" #
#########################

# INPUT:
# y: outcome
# d: treatment
# m: mediator
# x: pre-treatment confounders
# w: post-treatment confounders of m
# trim: trimming level for the propensity score
# boot: number of bootstraps

# OUTPUT:
# te: ATE
# de.treat: direct effect under treatment 
# de.treat.trim: trimmed direct effect under treatment
# de.control: direct effect under control
# de.control.trim: trimmed direct effect under control
# ie.treat: indirect effect under treatment (Assumption 1 and 2)
# ie.treat.trim: trimmed indirect effect under treatment (Assumption 1 and 2)
# ie.control: indirect effect under control (Assumption 1 and 2)
# ie.control.trim: trimmed indirect effect under control (Assumption 1 and 2)
# ie.total.treat: total indirect effect under treatment (Assumptions 3 to 5)
# ie.partial.treat: partial indirect effect under treatment (Assumptions 3 and 4)
# ie.partial.treat.trim: trimmed partial indirect effect under treatment (Assumptions 3 and 4)
# ie.total.control: total indirect effect under control (Assumptions 3 to 5)
# ie.partial.control: partial indirect effect under control (Assumptions 3 and 4)
# ie.partial.control.trim: trimmed partial indirect effect under control (Assumptions 3 and 4)

# sd.te: Standard error of ATE
# sd.de.treat: Standard error of direct effect under treatment 
# sd.de.treat.trim: Standard error of trimmed direct effect under treatment
# sd.de.control: Standard error of direct effect under control
# sd.de.control.trim: Standard error of trimmed direct effect under control
# sd.ie.treat: Standard error of indirect effect under treatment (Assumption 1 and 2)
# sd.ie.treat.trim: Standard error of trimmed indirect effect under treatment (Assumption 1 and 2)
# sd.ie.control: Standard error of indirect effect under control (Assumption 1 and 2)
# sd.ie.control.trim: Standard error of trimmed indirect effect under control (Assumption 1 and 2)
# sd.ie.total.treat: Standard error of total indirect effect under treatment (Assumptions 3 to 5)
# sd.ie.partial.treat: Standard error of partial indirect effect under treatment (Assumptions 3 and 4)
# sd.ie.partial.treat.trim: Standard error of trimmed partial indirect effect under treatment (Assumptions 3 and 4)
# sd.ie.total.control: Standard error of total indirect effect under control (Assumptions 3 to 5)
# sd.ie.partial.control: Standard error of partial indirect effect under control (Assumptions 3 and 4)
# sd.ie.partial.control.trim: Standard error of trimmed partial indirect effect under control (Assumptions 3 and 4)


mediation<-function(y,d,m,x,w,trim=0.05, boot, n_folds = 2){
  temp<-effects.mediation(y=y,d=d,m=m,x=x,w=w,trim=trim,n_folds=n_folds)
  temp2<-bootstrap.mediation(y=y,d=d,m=m,x=x,w=w,boot=boot,trim=trim)
  list(te=temp$te, de.treat=temp$de.treat, de.treat.trim=temp$de.treat.trim, de.control=temp$de.control, de.control.trim=temp$de.control.trim, ie.treat=temp$ie.treat, ie.treat.trim=temp$ie.treat.trim, ie.control=temp$ie.control, ie.control.trim=temp$ie.control.trim,     ie.treat.pretreat=temp$ie.treat.pretreat, ie.treat.pretreat.trim=temp$ie.treat.pretreat.trim, ie.control.pretreat=temp$ie.control.pretreat, ie.control.pretreat.trim=temp$ie.control.pretreat.trim,    ie.total.treat=temp$ie.total.treat, ie.partial.treat=temp$ie.partial.treat, ie.total.control=temp$ie.total.control, ie.partial.control=temp$ie.partial.control, sd.te=temp2$sd.te, sd.de.treat=temp2$sd.de.treat, sd.de.treat.trim=temp2$sd.de.treat.trim, sd.de.control=temp2$sd.de.control, sd.de.control.trim=temp2$sd.de.control.trim, sd.ie.treat=temp2$sd.ie.treat, sd.ie.treat.trim=temp2$sd.ie.treat.trim, sd.ie.control=temp2$sd.ie.control, sd.ie.control.trim=temp2$sd.ie.control.trim,            sd.ie.treat.pretreat=temp2$sd.ie.treat.pretreat, sd.ie.treat.pretreat.trim=temp2$sd.ie.treat.pretreat.trim, sd.ie.control.pretreat=temp2$sd.ie.control.pretreat, sd.ie.control.pretreat.trim=temp2$sd.ie.control.pretreat.trim,          sd.ie.total.treat=temp2$sd.ie.total.treat, sd.ie.partial.treat=temp2$sd.ie.partial.treat, sd.ie.total.control=temp2$sd.ie.total.control, sd.ie.partial.control=temp2$sd.ie.partial.control)
}

effects.mediation <- function(y, d, m, x, w, trim = 0.05, n_folds = 2) {
  # Combine pre-treatment and post-treatment confounders
  data <- data.frame(y = y, d = d, m = m, x, w)
  
  # Define machine learning learners
  ml_y <- lrn("regr.ranger")  # Outcome model
  ml_d <- lrn("regr.ranger")  # Treatment model
  ml_m <- lrn("regr.ranger")  # Mediator model
  
  # Step 1: Estimate total effect (TE) using DoubleML
  dml_data_y <- DoubleMLData$new(data, y_col = "y", d_col = "d", x_cols = c("m", colnames(x), colnames(w)))
  dml_model_y <- DoubleMLPLR$new(dml_data_y, ml_l = ml_y, ml_m = ml_d, n_folds = n_folds)
  dml_model_y$fit()
  total_effect <- dml_model_y$coef["d"]
  
  # Step 2: Estimate mediator model
  dml_data_m <- DoubleMLData$new(data, y_col = "m", d_col = "d", x_cols = c(colnames(x), colnames(w)))
  dml_model_m <- DoubleMLPLR$new(dml_data_m, ml_l = ml_m, ml_m = ml_d, n_folds = n_folds)
  dml_model_m$fit()
  mediator_effect <- dml_model_m$coef["d"]
  
  # Compute direct and indirect effects
  direct_effect <- total_effect - mediator_effect
  indirect_effect <- mediator_effect
  
  # Step 3: Apply trimming
  ps <- dml_model_y$psi  # Extract propensity scores
  trim_mask <- (ps >= trim) & (ps <= (1 - trim))
  trimmed_data <- data[trim_mask, ]
  
  # Re-estimate effects after trimming
  dml_trimmed_y <- DoubleMLData$new(trimmed_data, y_col = "y", d_col = "d", x_cols = c("m", colnames(x), colnames(w)))
  dml_trimmed_model_y <- DoubleMLPLR$new(dml_trimmed_y, ml_l = ml_y, ml_m = ml_d, n_folds = n_folds)
  dml_trimmed_model_y$fit()
  total_effect_trimmed <- dml_trimmed_model_y$coef["d"]
  
  dml_trimmed_m <- DoubleMLData$new(trimmed_data, y_col = "m", d_col = "d", x_cols = c(colnames(x), colnames(w)))
  dml_trimmed_model_m <- DoubleMLPLR$new(dml_trimmed_m, ml_l = ml_m, ml_m = ml_d, n_folds = n_folds)
  dml_trimmed_model_m$fit()
  mediator_effect_trimmed <- dml_trimmed_model_m$coef["d"]
  
  direct_effect_trimmed <- total_effect_trimmed - mediator_effect_trimmed
  indirect_effect_trimmed <- mediator_effect_trimmed
  
  return(list(
    te = total_effect,
    de = direct_effect,
    de.trim = direct_effect_trimmed,
    ie = indirect_effect,
    ie.trim = indirect_effect_trimmed,
    te.trim = total_effect_trimmed
  ))
}




bootstrap.mediation<-function(y,d,m,x,w,boot=1999,trim=0.05,n_folds=2){
  obs<-length(y)
  mc=c()
  temp=c()
  while(length(temp)<boot){
    sboot<-sample(1:obs,obs,TRUE)
    yb=y[sboot]
    db<-d[sboot]
    mb=m[sboot]
    if (length(x)==length(y)) xb<-x[sboot]
    if (length(x)!=length(y)) xb<-x[sboot,]
    if (length(w)==length(y)) wb<-w[sboot]
    if (length(w)!=length(y)) wb<-w[sboot,]
    
    est<-c(effects.mediation(y=y,d=d,m=m,x=x,w=w,trim=trim,n_folds=n_folds))
    if (sum(is.na(est))==0) mc<-rbind(mc, est)
    temp<-c(temp,1)
  }
  list(mc=mc, sd.te=sd(as.numeric(mc[,1])), sd.de.treat=sd(as.numeric(mc[,2])), sd.de.treat.trim=sd(as.numeric(mc[,3])), sd.de.control=sd(as.numeric(mc[,4])), sd.de.control.trim=sd(as.numeric(mc[,5])), sd.ie.treat=sd(as.numeric(mc[,6])), sd.ie.treat.trim=sd(as.numeric(mc[,7])), sd.ie.control=sd(as.numeric(mc[,8])), sd.ie.control.trim=sd(as.numeric(mc[,9])), sd.ie.treat.pretreat=sd(as.numeric(mc[,10])), sd.ie.treat.pretreat.trim=sd(as.numeric(mc[,11])), sd.ie.control.pretreat=sd(as.numeric(mc[,12])), sd.ie.control.pretreat.trim=sd(as.numeric(mc[,13])),sd.ie.total.treat=sd(as.numeric(mc[,14])), sd.ie.partial.treat=sd(as.numeric(mc[,15])), sd.ie.partial.treat.trim=sd(as.numeric(mc[,16])), sd.ie.total.control=sd(as.numeric(mc[,17])), sd.ie.partial.control=sd(as.numeric(mc[,18])), sd.ie.partial.control.trim=sd(as.numeric(mc[,19]))  )
}


###############
# APPLICATION #
###############

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

test <- effects.mediation(y, d, m, x, w)
print(test)

est<-mediation(y,d,m,x,w,trim=0.05, boot=1999)   
results<-rbind(cbind(est$te, est$de.treat, est$de.control, est$ie.total.treat,  est$ie.total.control, est$ie.partial.treat,  est$ie.partial.control,  est$ie.treat.pretreat,  est$ie.control.pretreat), cbind(est$sd.te, est$sd.de.treat, est$sd.de.control, est$sd.ie.total.treat, est$sd.ie.total.control, est$sd.ie.partial.treat, est$sd.ie.partial.control, est$sd.ie.treat.pretreat,  est$sd.ie.control.pretreat), cbind(2*pnorm(-abs(est$te/est$sd.te)), 2*pnorm(-abs(est$de.treat/est$sd.de.treat)), 2*pnorm(-abs(est$de.control/est$sd.de.control)), 2*pnorm(-abs(est$ie.total.treat/est$sd.ie.total.treat)),  2*pnorm(-abs(est$ie.total.control/est$sd.ie.total.control)),  2*pnorm(-abs(est$ie.partial.treat/est$sd.ie.partial.treat)), 2*pnorm(-abs(est$ie.partial.control/est$sd.ie.partial.control)), 2*pnorm(-abs(est$ie.treat.pretreat/est$sd.ie.treat.pretreat)), 2*pnorm(-abs(est$ie.control.pretreat/est$sd.ie.control.pretreat))    )  )
xtable(results, digits=3)
    
# Save results
save.image("data/dml_females.RData")



# 2) ESTIMATION FOR MALES:

indicator=female==0
y=exhealth30[indicator==1]
d=treat[indicator==1]
m=work2year2q[indicator==1]
x=cbind(schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat, edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis, hhinc12, hhinc8, fdstamp, welf1, welf2, publicass)[indicator==1,]
w=cbind(emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,  health1212, health123,  pe_prb12, pe_prb12mis,  narry1, numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis)[indicator==1,]

# Combine confounders
confounders <- cbind(x, w)

est<-mediation(y,d,m,x,w,trim=0.05, boot=1999)   
results<-rbind(cbind(est$te, est$de.treat, est$de.control, est$ie.total.treat,  est$ie.total.control, est$ie.partial.treat,  est$ie.partial.control,  est$ie.treat.pretreat,  est$ie.control.pretreat), cbind(est$sd.te, est$sd.de.treat, est$sd.de.control, est$sd.ie.total.treat, est$sd.ie.total.control, est$sd.ie.partial.treat, est$sd.ie.partial.control, est$sd.ie.treat.pretreat,  est$sd.ie.control.pretreat), cbind(2*pnorm(-abs(est$te/est$sd.te)), 2*pnorm(-abs(est$de.treat/est$sd.de.treat)), 2*pnorm(-abs(est$de.control/est$sd.de.control)), 2*pnorm(-abs(est$ie.total.treat/est$sd.ie.total.treat)),  2*pnorm(-abs(est$ie.total.control/est$sd.ie.total.control)),  2*pnorm(-abs(est$ie.partial.treat/est$sd.ie.partial.treat)), 2*pnorm(-abs(est$ie.partial.control/est$sd.ie.partial.control)), 2*pnorm(-abs(est$ie.treat.pretreat/est$sd.ie.treat.pretreat)), 2*pnorm(-abs(est$ie.control.pretreat/est$sd.ie.control.pretreat))    )  )
xtable(results, digits=3)

# Save results
save.image("data/dml_males.RData")