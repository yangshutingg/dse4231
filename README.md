# DSE4231: Machine learning in mediation analysis - an extension of inverse probability weighting

## Project Overview  

This repository contains the codes and methodologies for estimating treatment effects using **machine learning (ML) models**. The study compares IPW, DML, DR, CF with OLS to explore their potential to identify treatment effects of the Job Corps program.  

---

## Table of Contents  

- [Background](#background)  
- [Methodology](#methodology)  
- [Files](#files)  
- [Installation](#installation)  
- [Contributors](#contributors)  

---

## Background  

In causal inference, mediators transmit part of a treatment’s effect to the outcome but are typically not randomly assigned, leading to potential confounding bias. Huber (2014) tackles this challenge using inverse probability weighting (IPW) to estimate direct and indirect effects. Machine learning methods further enhance mediation analysis by flexibly adjusting for high-dimensional confounders and capturing treatment effect heterogeneity.

### Objectives:  

1. Compare the performance of models (IPW, DML, DR, CF) with OLS causal mediation model.  
2. Identify total, direct and indirect treatment effects of the Job Corps program for males and females respectively.  

---

## Methodology  

### Data  

- **Source**:  
  - **Martin Huber's Dataset**: A dataset of the U.S. Job Corps experimental study with information on the health and employment outcomes of disadvantaged youths. The dataset can be downloaded [here](http://qed.econ.queensu.ca/jae/datasets/huber001/).

- **Variables of interest**:  
  - **Treatment (D)**:
    - `treat`: Assignment to the Job Corps program. = 1 if in program group, = 0 if in control group.
  - **Mediator (M)**:
    - `work2year2q`: Employment 1 to 1.5 years after assignment.
  - **Outcome (Y)**:
    - `exhealth30`: Whether participant reported “very good” health 30 months after assignment.
  - **Pre-treatment covariates (X)**:
    - `schobef`: Whether participant was in school 1 year before assignment.
    - `trainyrbef`: Whether participant was in training 1 year before assignment.
    - `jobeverbef`: Whether participant ever had a job before assignment.
    - `jobyrbef`: Whether participant had a job 1 year before assignment.
    - `health012`: Health at assignment. = 1 if very good health, = 4 if bad health.
    - `health0mis`: Whether health at assignment was missing. = 1 if `health012` is missing, = 0 otherwise.
    - `pe_prb0`: Whether participant had physical/emotional problems at assignment.
    - `pe_prb0mis`: Whether physical/emotional problems at assignment was missing. = 1 if `pe_prb0` is missing, = 0 otherwise.
    - `everalc`: Whether participant have ever had alcohol abuse before assignment.
    - `everilldrugs`: Whether participant have ever took illegal drugs before assignment.
    - `age_cat`: Age at assignment.
    - `edumis`: education missing
    - `eduhigh`: higher education
    - `rwhite`: Whether participant was white.
    - `everarr`: Whether participant was ever arrested before assignment.
    - `hhsize`: Household size at assignment.
    - `hhsizemis`: Whether household size at assignment was missing. = 1 if `hhsize` is missing, = 0 otherwise.
    - `hhinc12`: Whether household income was low 1 year before assignment. (baseline category = intermediate household income)
    - `hhinc8`: Whether household income was high 1 year before assignment. (baseline category = intermediate household income)
    - `fdstamp`: Whether participant received foodstamps 1 year before assignment.
    - `welf1`: Whether participant was once on welfare before assignment.
    - `welf2`: Whether participant was twice on welfare before assignment.
    - `publicass`: Whether participant was on public assistance 1 year before assignment.
  - **Post-treatment covariates (W)**:
    - `emplq4`: Whether participant worked some time in 9-12 months after assignment.
    - `emplq4full`: Whether participant worked all the time in 9-12 months after assignment
    - `pemplq4`: Proportion of weeks worked 9-12 months after assignment.
    - `pemplq4mis`: Whether proportion of weeks worked 9-12 months after assignment was missing. = 1 if `pemplq4` is missing, = 0 otherwise.
    - `vocq4`: Whether participant was in vocational training 9-12 months after assignment.
    - `vocq4mis`: Whether participation in vocational training 9-12 months after assignment was missing. = 1 if `vocq4` is missing, = 0 otherwise.
    - `health1212`: Health 1 year after assignment. = 1 if very good or good health, = 0 otherwise. (baseline category = 4 = bad health)
    - `health123`: Health 1 year after assignment. = 1 if fair health, = 0 otherwise. (baseline category = 4 = bad health)
    - `pe_prb12`: Whether participant had physical/emotional problems 1 year after assignment.
    - `pe_prb12mis`: missing - Whether physical/emotional problems 1 year after assignment was missing. = 1 if `pe_prb12` is missing, = 0 otherwise.
    - `narry1`: Number of arrests in first year after assignment.
    - `numkidhhf1zero`: Whether participant has his/her own children in the household 1 year after assignment. = 1 if no own children in the household, = 0 otherwise.
    - `numkidhhf1onetwo`: Whether participant has one or two own children in the household 1 year after assignment. = 1 if 1 or 2 children in the household, = 0 otherwise.
    - `pubhse12`: Whether participant stayed in public housing 1 year after assignment.
    - `h_ins12a`: Whether participant received Aid to Families with Dependent Children (AFDC) or other transfer payments 1 year after assignment.
    - `h_ins12amis`: Whether transfer payments 1 year after assignment was missing. = 1 if `h_ins12a` is missing, = 0 otherwise.

### Models  

- **Ordinary Least Squares (OLS) Causal Mediation**
  - Using the `mediation` package to conduct causal mediation analysis using linear mediator model and outcome model.

- **Inverse Probability Weighting (IPW)**
  - A replication of Huber's application.  

- **Double Machine Learning (DML)**
  - Using the `medDML` package to conduct causal mediation analysis using double machine learning to control for confounders based on doubly robust efficient score functions for potential outcomes.

- **Doubly Robust (DR)**
  - Using the `WeightIt` and `mlr3` packages to conduct causal mediation analysis using doubly robust estimator to combine outcome weighted using IPW with predictions generated by outcome model.

- **Causal Forest (CF)**
  - Using the `grf` package to conduct causal mediation analysis using causal forests trained via subsampling & EMSE-optimized splits.

### Treatment effects  
1. **Total effect**: Average treatment effect = E[Y(1)-Y(0)].
2. **Direct effect**: Treatment effect while holding the mediator (M) and post-treatment covariates (W) constant.
3. **Indirect effect**: All effects via mediator (M) which either comes from treatment (D) or post-treatment covariates (W).

---

## Files  

- `data/`: Dataset used for analysis.
  - STATA data file format: [`causalmech.dta`](https://github.com/yangshutingg/dse4231/blob/main/data/causalmech.dta)
  - CSV format: [`causalmech.csv`](https://github.com/yangshutingg/dse4231/blob/main/data/causalmech.csv)
- `src/`: Source code for each model and treatment effects estimation.
  - OLS causal mediation: [`mediation.R`](https://github.com/yangshutingg/dse4231/blob/main/src/mediation.R)
  - IPW: [`ipw.R`](https://github.com/yangshutingg/dse4231/blob/main/src/ipw.R)
  - DML: [`dml.R`](https://github.com/yangshutingg/dse4231/blob/main/src/dml.R)
  - DR: [`dr.R`](https://github.com/yangshutingg/dse4231/blob/main/src/dr.R)
  - CF: [`causal_forest.R`](https://github.com/yangshutingg/dse4231/blob/main/src/causal_forest.R)
- `README.md`: This file.  

---

## Installation  

1. Clone this repository:  
    ```bash  
    git clone https://github.com/yangshutingg/dse4231.git  
    ```  
2. Navigate to the repository directory:  
    ```bash  
    cd dse4231  
    ```  
3. Install dependencies:  
    ```bash  
    pip install -r requirements.txt  
    ```  

---

## Contributors  

1. [Eliza Ong](https://github.com/Elizaongwz)  
2. [Krystal Low Li Tong](https://github.com/krystallow)
3. [Lily Rozana Joehann Aung](https://github.com/lilyrja)
4. [Yang Shu Ting](https://github.com/yangshutingg)  

---

## Built With

* [R](https://www.r-project.org/)

---

## Acknowledgments

* Prof. Denis & Prof. Huang for their guidance on this project

---

## References

* Huber, M. (2014). Identifying causal mechanisms (primarily) based on inverse probability weighting. Journal of Applied Econometrics,29(6), 920-943. (https://onlinelibrary.wiley.com/doi/abs/10.1002/jae.2341https://onlinelibrary.wiley.com/doi/abs/10.1002/jae.2341)

---

This project showcases how machine learning models can enhance mediation analysis by enhancing precision in treatment effect estimation. Visit the [GitHub repository](https://github.com/yangshutingg/dse4231) for more details!
