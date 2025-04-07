library(foreign)
library(xtable)
library(np)
 

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
pred0=cbind(1,mean(m*(1-d)/(1-pscore2)),w,x)%*%temp


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

data=read.dta("data/causalmech.dta")
attach(data)

# 1) ESTIMATION FOR FEMALES:

indicator=female==1 
y=exhealth30[indicator==1]
d=treat[indicator==1]
m=work2year2q[indicator==1]
x=cbind(schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat, edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis, hhinc12, hhinc8, fdstamp, welf1, welf2, publicass)[indicator==1,]
w=cbind(emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,  health1212, health123,  pe_prb12, pe_prb12mis,  narry1, numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis)[indicator==1,]
pscore<-glm(d~cbind(x,w,m),family=binomial(probit))$fitted

# plot the propensity scores:
bw0 <- npudensbw(formula=~pscore[d==0],bwmethod="normal-reference")
bw1 <- npudensbw(formula=~pscore[d==1],bwmethod="normal-reference")
d0<-npudens(bws=bw0, edat=seq(0,1,length.out = 1000))$dens
d1<-npudens(bws=bw1, edat=seq(0,1,length.out = 1000))$dens
plot(seq(0,1,length.out = 1000),d1, type="S", ylim=c(0,8), ylab="probability density function", xlab="propensity score under treatment (females)")
plot(seq(0,1,length.out = 1000),d0, type="S", ylim=c(0,8), ylab="probability density function", xlab="propensity score under non-treatment (females)")

# estimate the direct and indirect effects: 
est<-mediation(y,d,m,x,w,trim=0.05, boot=1999)   
results<-rbind(cbind(est$te, est$de.treat, est$de.control, est$ie.total.treat,  est$ie.total.control, est$ie.partial.treat,  est$ie.partial.control,  est$ie.treat.pretreat,  est$ie.control.pretreat), cbind(est$sd.te, est$sd.de.treat, est$sd.de.control, est$sd.ie.total.treat, est$sd.ie.total.control, est$sd.ie.partial.treat, est$sd.ie.partial.control, est$sd.ie.treat.pretreat,  est$sd.ie.control.pretreat), cbind(2*pnorm(-abs(est$te/est$sd.te)), 2*pnorm(-abs(est$de.treat/est$sd.de.treat)), 2*pnorm(-abs(est$de.control/est$sd.de.control)), 2*pnorm(-abs(est$ie.total.treat/est$sd.ie.total.treat)),  2*pnorm(-abs(est$ie.total.control/est$sd.ie.total.control)),  2*pnorm(-abs(est$ie.partial.treat/est$sd.ie.partial.treat)), 2*pnorm(-abs(est$ie.partial.control/est$sd.ie.partial.control)), 2*pnorm(-abs(est$ie.treat.pretreat/est$sd.ie.treat.pretreat)), 2*pnorm(-abs(est$ie.control.pretreat/est$sd.ie.control.pretreat))    )  )
xtable(results, digits=3)


# get MSE and CI
boot_results <- bootstrap.mediation(y, d, m, x, w, boot = 1999, trim = 0.05)
mc <- boot_results$mc  # This is your bootstrap matrix
# Convert to matrix and ensure all values are numeric
mc <- as.matrix(mc)
mc <- matrix(as.numeric(mc), nrow = nrow(mc))  # Force coercion to numeric
# Check for NA/NaN/Inf
sum(is.na(mc))        # Total NA/NaN values
sum(!is.finite(mc))   # Includes Inf/-Inf
# Remove problematic rows (if needed)
mc <- mc[complete.cases(mc), ]

mse <- colMeans(mc^2, na.rm = TRUE)  # MSE for each effect
# Preserve column names
if(!is.null(colnames(mc))) {
  names(mse) <- colnames(mc)
}

print(mse)

ci_lower <- apply(mc, 2, quantile, probs = 0.025, na.rm = TRUE)
ci_upper <- apply(mc, 2, quantile, probs = 0.975, na.rm = TRUE)
# Preserve column names
if(!is.null(colnames(mc))) {
  names(ci_lower) <- colnames(mc)
}
print(ci_lower)
if(!is.null(colnames(mc))) {
  names(ci_upper) <- colnames(mc)
}
print(ci_upper)

save.image("data/females.RData")


# 2) ESTIMATION FOR MALES:

indicator=female==0 
y=exhealth30[indicator==1]
d=treat[indicator==1]
m=work2year2q[indicator==1]
x=cbind(schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat, edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis, hhinc12, hhinc8, fdstamp, welf1, welf2, publicass)[indicator==1,]
w=cbind(emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,  health1212, health123,  pe_prb12, pe_prb12mis,  narry1, numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis)[indicator==1,]
pscore<-glm(d~cbind(x,w,m),family=binomial(probit))$fitted

# plot the propensity scores:
bw0 <- npudensbw(formula=~pscore[d==0],bwmethod="normal-reference")
bw1 <- npudensbw(formula=~pscore[d==1],bwmethod="normal-reference")
d0<-npudens(bws=bw0, edat=seq(0,1,length.out = 1000))$dens
d1<-npudens(bws=bw1, edat=seq(0,1,length.out = 1000))$dens
plot(seq(0,1,length.out = 1000),d1, type="S", ylim=c(0,8), ylab="probability density function", xlab="propensity score under treatment (males)")
plot(seq(0,1,length.out = 1000),d0, type="S", ylim=c(0,8), ylab="probability density function", xlab="propensity score under non-treatment (males)")

# estimate the direct and indirect effects: 
est<-mediation(y,d,m,x,w,trim=0.05, boot=1999)
results<-rbind(cbind(est$te, est$de.treat, est$de.control, est$ie.total.treat,  est$ie.total.control, est$ie.partial.treat,  est$ie.partial.control,  est$ie.treat.pretreat,  est$ie.control.pretreat), cbind(est$sd.te, est$sd.de.treat, est$sd.de.control, est$sd.ie.total.treat, est$sd.ie.total.control, est$sd.ie.partial.treat, est$sd.ie.partial.control, est$sd.ie.treat.pretreat,  est$sd.ie.control.pretreat), cbind(2*pnorm(-abs(est$te/est$sd.te)), 2*pnorm(-abs(est$de.treat/est$sd.de.treat)), 2*pnorm(-abs(est$de.control/est$sd.de.control)), 2*pnorm(-abs(est$ie.total.treat/est$sd.ie.total.treat)),  2*pnorm(-abs(est$ie.total.control/est$sd.ie.total.control)),  2*pnorm(-abs(est$ie.partial.treat/est$sd.ie.partial.treat)), 2*pnorm(-abs(est$ie.partial.control/est$sd.ie.partial.control)), 2*pnorm(-abs(est$ie.treat.pretreat/est$sd.ie.treat.pretreat)), 2*pnorm(-abs(est$ie.control.pretreat/est$sd.ie.control.pretreat))    )  )
xtable(results, digits=3)

# get MSE and CI
boot_results <- bootstrap.mediation(y, d, m, x, w, boot = 1999, trim = 0.05)
mc <- boot_results$mc  # This is your bootstrap matrix
# Convert to matrix and ensure all values are numeric
mc <- as.matrix(mc)
mc <- matrix(as.numeric(mc), nrow = nrow(mc))  # Force coercion to numeric
# Check for NA/NaN/Inf
sum(is.na(mc))        # Total NA/NaN values
sum(!is.finite(mc))   # Includes Inf/-Inf
# Remove problematic rows (if needed)
mc <- mc[complete.cases(mc), ]

mse <- colMeans(mc^2, na.rm = TRUE)  # MSE for each effect
# Preserve column names
if(!is.null(colnames(mc))) {
  names(mse) <- colnames(mc)
}

print(mse)

ci_lower <- apply(mc, 2, quantile, probs = 0.025, na.rm = TRUE)
ci_upper <- apply(mc, 2, quantile, probs = 0.975, na.rm = TRUE)
# Preserve column names
if(!is.null(colnames(mc))) {
  names(ci_lower) <- colnames(mc)
}
print(ci_lower)
if(!is.null(colnames(mc))) {
  names(ci_upper) <- colnames(mc)
}
print(ci_upper)

save.image("data/males.RData")