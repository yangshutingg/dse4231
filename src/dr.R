library(DoubleML)
library(data.table)
library(mlr3)
library(mlr3learners)
library(WeightIt)  # For propensity score weighting
library(sandwich)  # For robust standard errors
library(lmtest)  # For hypothesis testing
library(mediation)  # Traditional mediation methods
library(haven)


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


mediation<-function(y,d,m,x,w,trim=0.05, boot){
  temp<-effects.mediation(y=y,d=d,m=m,x=x,w=w,trim=trim)
  temp2<-bootstrap.mediation(y=y,d=d,m=m,x=x,w=w,boot=boot,trim=trim)
  list(te=temp$te, de.treat=temp$de.treat, de.treat.trim=temp$de.treat.trim, de.control=temp$de.control, de.control.trim=temp$de.control.trim, ie.treat=temp$ie.treat, ie.treat.trim=temp$ie.treat.trim, ie.control=temp$ie.control, ie.control.trim=temp$ie.control.trim,     ie.treat.pretreat=temp$ie.treat.pretreat, ie.treat.pretreat.trim=temp$ie.treat.pretreat.trim, ie.control.pretreat=temp$ie.control.pretreat, ie.control.pretreat.trim=temp$ie.control.pretreat.trim,    ie.total.treat=temp$ie.total.treat, ie.partial.treat=temp$ie.partial.treat, ie.total.control=temp$ie.total.control, ie.partial.control=temp$ie.partial.control, sd.te=temp2$sd.te, sd.de.treat=temp2$sd.de.treat, sd.de.treat.trim=temp2$sd.de.treat.trim, sd.de.control=temp2$sd.de.control, sd.de.control.trim=temp2$sd.de.control.trim, sd.ie.treat=temp2$sd.ie.treat, sd.ie.treat.trim=temp2$sd.ie.treat.trim, sd.ie.control=temp2$sd.ie.control, sd.ie.control.trim=temp2$sd.ie.control.trim,            sd.ie.treat.pretreat=temp2$sd.ie.treat.pretreat, sd.ie.treat.pretreat.trim=temp2$sd.ie.treat.pretreat.trim, sd.ie.control.pretreat=temp2$sd.ie.control.pretreat, sd.ie.control.pretreat.trim=temp2$sd.ie.control.pretreat.trim,          sd.ie.total.treat=temp2$sd.ie.total.treat, sd.ie.partial.treat=temp2$sd.ie.partial.treat, sd.ie.total.control=temp2$sd.ie.total.control, sd.ie.partial.control=temp2$sd.ie.partial.control)
}

effects.mediation<-function(y,d,m,x,w,trim=0.05){
  
  temp=glm(d~cbind(m,w,x),family=binomial(probit))$coef
  pscore1=glm(d~cbind(m,w,x),family=binomial(probit))$fitted
  #pscore2=rep(mean(d),length(pscore1))
  pscore2=glm(d~x,family=binomial(probit))$fitted
  pscore3=glm(d~cbind(w,x),family=binomial(probit))$fitted
  pscore4=glm(d~cbind(m,x),family=binomial(probit))$fitted
  
  temp<-lm(y[d==1]~cbind(m,w,x)[d==1,])$coef
  
  m0 <- mean(m * (1 - d) / (1 - pscore2))  # IPW estimate of M(0)
  design_m0 <- cbind(1, m0, w, x)           # Design matrix with M(0)
  
  # Step 3: Doubly robust prediction (IPW + regression)
  y1_pred <- predict(temp, newdata = data.frame(design_m0))
  pred0 = mean(d * y / pscore2 + (1 - d / pscore2) * y1_pred)
  
  #pred0=cbind(1,mean(m*(1-d)/(1-pscore2)),w,x)%*%temp
  
  
  ind=((pscore1<trim) | (pscore1>(1-trim)) )
  y1m1<-sum(y*d/pscore2)/sum(d/pscore2)
  y1m1trim<-sum(y[ind==0]*d[ind==0]/pscore2[ind==0])/sum(d[ind==0]/pscore2[ind==0])
  y0m0<-sum(y*(1-d)/(1-pscore2))/sum((1-d)/(1-pscore2))
  y0m0trim<-sum(y[ind==0]*(1-d[ind==0])/(1-pscore2[ind==0]))/sum((1-d[ind==0])/(1-pscore2[ind==0]))
  y1m0<-(sum(y*d*(1-pscore1)/((1-pscore2)*pscore1))/sum(d*(1-pscore1)/((1-pscore2)*pscore1)))
  y1m0trim<-(sum(y[ind==0]*d[ind==0]*(1-pscore1[ind==0])/((1-pscore2[ind==0])*pscore1[ind==0]))/sum(d[ind==0]*(1-pscore1[ind==0])/((1-pscore2[ind==0])*pscore1[ind==0])))
  y0m1<-(sum(y*(1-d)* pscore1/(pscore2*(1-pscore1)))/sum((1-d)* pscore1/(pscore2*(1-pscore1))))
  y0m1trim<-(sum(y[ind==0]*(1-d[ind==0])* pscore1[ind==0]/(pscore2[ind==0]*(1-pscore1[ind==0])))/sum((1-d[ind==0])* pscore1[ind==0]/(pscore2[ind==0]*(1-pscore1[ind==0]))))
  
  
  de.treat=y1m1-y0m1
  de.treat.trim=y1m1trim-y0m1trim
  ie.control=y0m1-y0m0
  ie.control.trim=y0m1trim-y0m0trim
  ie.control.pretreat=(sum(y*(1-d)*pscore4/(pscore2*(1-pscore4)))/sum((1-d)*pscore4/(pscore2*(1-pscore4)))) - y0m0
  ie.control.pretreat.trim=(sum(y[ind==0]*(1-d[ind==0])*pscore4[ind==0]/(pscore2[ind==0]*(1-pscore4[ind==0])))/sum((1-d[ind==0])*pscore4[ind==0]/(pscore2[ind==0]*(1-pscore4[ind==0])))) - y0m0trim
  de.control=y1m0-y0m0
  de.control.trim=y1m0trim-y0m0trim
  ie.treat=y1m1-y1m0
  ie.treat.trim=y1m1trim-y1m0trim
  ie.treat.pretreat=y1m1 -  (sum(y*d *(1-pscore4)/pscore4)/sum(d *(1-pscore4)/pscore4)) 
  ie.treat.pretreat.trim=y1m1trim -  (sum(y[ind==0]*d[ind==0] *(1-pscore4[ind==0])/pscore4[ind==0])/sum(d[ind==0] *(1-pscore4[ind==0])/pscore4[ind==0]))
  
  ie.partial.treat=y1m1 - (sum( y*d/pscore1 * (1-pscore1)/(1-pscore3)* (pscore3)/pscore2 )/sum(d/pscore1 * (1-pscore1)/(1-pscore3)* (pscore3)/pscore2 ))
  ie.partial.treat.trim=y1m1trim - (sum( y[ind==0]*d[ind==0]/pscore1[ind==0] * (1-pscore1[ind==0])/(1-pscore3[ind==0])* (pscore3[ind==0])/pscore2[ind==0] )/sum(d[ind==0]/pscore1[ind==0] * (1-pscore1[ind==0])/(1-pscore3[ind==0])* (pscore3[ind==0])/pscore2[ind==0] ))
  ie.total.treat= sum((y-pred0)*d/pscore2)/sum(d/pscore2) 
  
  temp<-lm(y[d==0]~cbind(m,w,x)[d==0,])$coef
  pred0=cbind(1,mean(m*d/pscore2),w,x)%*%temp
  
  ie.partial.control=sum( y*(1-d)/(1-pscore1) * (pscore1)/(pscore3)* (1-pscore3)/(1-pscore2) )/sum((1-d)/(1-pscore1) * (pscore1)/(pscore3)* (1-pscore3)/(1-pscore2) ) - y0m0
  ie.partial.control.trim=sum( y[ind==0]*(1-d[ind==0])/(1-pscore1[ind==0]) * (pscore1[ind==0])/(pscore3[ind==0])* (1-pscore3[ind==0])   /(1-pscore2[ind==0])  )/sum((1-d[ind==0])/(1-pscore1[ind==0]) * (pscore1[ind==0])/(pscore3[ind==0])* (1-pscore3[ind==0]) /(1-pscore2[ind==0]) ) - y0m0trim
  ie.total.control=sum( (pred0-y)*(1-d)/(1-pscore2)  )/sum((1-d)/(1-pscore2)  )
  
  te=mean(y[d==1])-mean(y[d==0])
  list(te=te, de.treat=de.treat, de.treat.trim=de.treat.trim, de.control=de.control, de.control.trim=de.control.trim, ie.treat=ie.treat, ie.treat.trim=ie.treat.trim, ie.control=ie.control, ie.control.trim=ie.control.trim, ie.treat.pretreat=ie.treat.pretreat, ie.treat.pretreat.trim=ie.treat.pretreat.trim, ie.control.pretreat=ie.control.pretreat, ie.control.pretreat.trim=ie.control.pretreat.trim, ie.total.treat=ie.total.treat, ie.partial.treat=ie.partial.treat, ie.partial.treat.trim=ie.partial.treat.trim, ie.total.control=ie.total.control, ie.partial.control=ie.partial.control, ie.partial.control.trim=ie.partial.control.trim)
}

bootstrap.mediation<-function(y,d,m,x,w,boot=1999,trim=0.05){
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
    
    est<-c(effects.mediation(yb,db,mb,xb,wb,trim=trim))
    if (sum(is.na(est))==0) mc<-rbind(mc, est)
    temp<-c(temp,1)
  }
  list(mc=mc, sd.te=sd(as.numeric(mc[,1])), sd.de.treat=sd(as.numeric(mc[,2])), sd.de.treat.trim=sd(as.numeric(mc[,3])), sd.de.control=sd(as.numeric(mc[,4])), sd.de.control.trim=sd(as.numeric(mc[,5])), sd.ie.treat=sd(as.numeric(mc[,6])), sd.ie.treat.trim=sd(as.numeric(mc[,7])), sd.ie.control=sd(as.numeric(mc[,8])), sd.ie.control.trim=sd(as.numeric(mc[,9])), sd.ie.treat.pretreat=sd(as.numeric(mc[,10])), sd.ie.treat.pretreat.trim=sd(as.numeric(mc[,11])), sd.ie.control.pretreat=sd(as.numeric(mc[,12])), sd.ie.control.pretreat.trim=sd(as.numeric(mc[,13])),sd.ie.total.treat=sd(as.numeric(mc[,14])), sd.ie.partial.treat=sd(as.numeric(mc[,15])), sd.ie.partial.treat.trim=sd(as.numeric(mc[,16])), sd.ie.total.control=sd(as.numeric(mc[,17])), sd.ie.partial.control=sd(as.numeric(mc[,18])), sd.ie.partial.control.trim=sd(as.numeric(mc[,19]))  )
}


###############
# APPLICATION #
###############

# Load the dataset
data=read_dta("data/causalmech.dta")
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

# ---- Step 1: Estimate Propensity Scores ----
# Model for treatment assignment
propensity_treat <- weightit(d ~ ., data = data.frame(d, confounders), method = "ps", estimand = "ATE")

# Model for mediator assignment
propensity_mediator <- weightit(m ~ ., data = data.frame(m, confounders), method = "ps", estimand = "ATE")

# Extract weights
weights_treat <- propensity_treat$weights
weights_mediator <- propensity_mediator$weights

# ---- Step 2: Outcome Model using Weighted Regression ----
# Direct Effect: Treatment ??? Outcome (adjusting for mediator & confounders)
direct_model <- lm(y ~ d + m + confounders, weights = weights_treat)
direct_effect <- coef(direct_model)["d"]
direct_se <- sqrt(diag(vcovHC(direct_model, type = "HC3"))["d"])
direct_pval <- 2 * pnorm(-abs(direct_effect / direct_se))

# Indirect Effect: Mediator ??? Outcome (adjusting for treatment & confounders)
indirect_model <- lm(y ~ m + d + confounders, weights = weights_mediator)
indirect_effect <- coef(indirect_model)["m"]
indirect_se <- sqrt(diag(vcovHC(indirect_model, type = "HC3"))["m"])
indirect_pval <- 2 * pnorm(-abs(indirect_effect / indirect_se))

# ---- Step 3: Display Results ----
cat("Doubly Robust Mediation Analysis Results:\n")
cat("Direct Effect (Treatment on Outcome):", direct_effect, "SE:", direct_se, "P-value:", direct_pval, "\n")
cat("Indirect Effect (Mediator on Outcome):", indirect_effect, "SE:", indirect_se, "P-value:", indirect_pval, "\n")
cat("Total Effect:", direct_effect+indirect_effect, "SE:", sqrt(direct_se^2 + indirect_se^2), "\n")

# Set seed for reproducibility
set.seed(123)

# Number of bootstrap samples
n_boot <- 1000  

# Initialize storage
boot_direct <- numeric(n_boot)
boot_indirect <- numeric(n_boot)
boot_total <- numeric(n_boot)

# Bootstrap loop
for (i in 1:n_boot) {
  # Resample data with replacement
  idx <- sample(1:length(y), replace = TRUE)
  y_boot <- y[idx]
  d_boot <- d[idx]
  m_boot <- m[idx]
  confounders_boot <- confounders[idx, ]
  
  # Re-estimate models
  tryCatch({
    # Direct effect model
    direct_model_boot <- lm(y_boot ~ d_boot + m_boot + confounders_boot, weights = weights_treat[idx])
    direct_effect_boot <- coef(direct_model_boot)["d_boot"]
    
    # Mediator model (d -> m)
    mediator_model_boot <- lm(m_boot ~ d_boot + confounders_boot, weights = weights_treat[idx])
    effect_d_on_m_boot <- coef(mediator_model_boot)["d_boot"]
    
    # Indirect effect model (m -> y)
    indirect_model_boot <- lm(y_boot ~ m_boot + d_boot + confounders_boot, weights = weights_mediator[idx])
    effect_m_on_y_boot <- coef(indirect_model_boot)["m_boot"]
    
    # Store results
    boot_direct[i] <- direct_effect_boot
    boot_indirect[i] <- effect_d_on_m_boot * effect_m_on_y_boot
    boot_total[i] <- direct_effect_boot + (effect_d_on_m_boot * effect_m_on_y_boot)
  }, error = function(e) {
    # Skip iterations with errors
    boot_direct[i] <- boot_indirect[i] <- boot_total[i] <- NA
  })
}

# Remove NA values (failed iterations)
boot_direct <- na.omit(boot_direct)
boot_indirect <- na.omit(boot_indirect)
boot_total <- na.omit(boot_total)

# Original estimates (from your code)
original_direct <- direct_effect
original_indirect <- indirect_effect
original_total <- direct_effect + indirect_effect

# MSE calculation
mse_direct <- mean((boot_direct - original_direct)^2)
mse_indirect <- mean((boot_indirect - original_indirect)^2)
mse_total <- mean((boot_total - original_total)^2)

# 95% Confidence Intervals (percentile method)
ci_direct <- quantile(boot_direct, probs = c(0.025, 0.975))
ci_indirect <- quantile(boot_indirect, probs = c(0.025, 0.975))
ci_total <- quantile(boot_total, probs = c(0.025, 0.975))


# Save results
save.image("data/dr_females.RData")


# 2) ESTIMATION FOR MALES:

indicator=female==0
y=exhealth30[indicator==1]
d=treat[indicator==1]
m=work2year2q[indicator==1]
x=cbind(schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat, edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis, hhinc12, hhinc8, fdstamp, welf1, welf2, publicass)[indicator==1,]
w=cbind(emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,  health1212, health123,  pe_prb12, pe_prb12mis,  narry1, numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis)[indicator==1,]

# Combine confounders
confounders <- cbind(x, w)

# ---- Step 1: Estimate Propensity Scores ----
# Model for treatment assignment
propensity_treat <- weightit(d ~ ., data = data.frame(d, confounders), method = "ps", estimand = "ATE")

# Model for mediator assignment
propensity_mediator <- weightit(m ~ ., data = data.frame(m, confounders), method = "ps", estimand = "ATE")

# Extract weights
weights_treat <- propensity_treat$weights
weights_mediator <- propensity_mediator$weights

# ---- Step 2: Outcome Model using Weighted Regression ----
# Direct Effect: Treatment ??? Outcome (adjusting for mediator & confounders)
direct_model <- lm(y ~ d + m + confounders, weights = weights_treat)
direct_effect <- coef(direct_model)["d"]
direct_se <- sqrt(diag(vcovHC(direct_model, type = "HC3"))["d"])
direct_pval <- 2 * pnorm(-abs(direct_effect / direct_se))

# Indirect Effect: Mediator ??? Outcome (adjusting for treatment & confounders)
indirect_model <- lm(y ~ m + d + confounders, weights = weights_mediator)
indirect_effect <- coef(indirect_model)["m"]
indirect_se <- sqrt(diag(vcovHC(indirect_model, type = "HC3"))["m"])
indirect_pval <- 2 * pnorm(-abs(indirect_effect / indirect_se))

# ---- Step 3: Display Results ----
cat("Doubly Robust Mediation Analysis Results:\n")
cat("Direct Effect (Treatment on Outcome):", direct_effect, "SE:", direct_se, "P-value:", direct_pval, "\n")
cat("Indirect Effect (Mediator on Outcome):", indirect_effect, "SE:", indirect_se, "P-value:", indirect_pval, "\n")

est<-mediation(y,d,m,x,w,trim=0.05, boot=1999)   
results<-rbind(cbind(est$te, est$de.treat, est$de.control, est$ie.total.treat,  est$ie.total.control, est$ie.partial.treat,  est$ie.partial.control,  est$ie.treat.pretreat,  est$ie.control.pretreat), cbind(est$sd.te, est$sd.de.treat, est$sd.de.control, est$sd.ie.total.treat, est$sd.ie.total.control, est$sd.ie.partial.treat, est$sd.ie.partial.control, est$sd.ie.treat.pretreat,  est$sd.ie.control.pretreat), cbind(2*pnorm(-abs(est$te/est$sd.te)), 2*pnorm(-abs(est$de.treat/est$sd.de.treat)), 2*pnorm(-abs(est$de.control/est$sd.de.control)), 2*pnorm(-abs(est$ie.total.treat/est$sd.ie.total.treat)),  2*pnorm(-abs(est$ie.total.control/est$sd.ie.total.control)),  2*pnorm(-abs(est$ie.partial.treat/est$sd.ie.partial.treat)), 2*pnorm(-abs(est$ie.partial.control/est$sd.ie.partial.control)), 2*pnorm(-abs(est$ie.treat.pretreat/est$sd.ie.treat.pretreat)), 2*pnorm(-abs(est$ie.control.pretreat/est$sd.ie.control.pretreat))    )  )
xtable(results, digits=3)

# Save results
save.image("data/dr_males.RData")