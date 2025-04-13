# DSE4231: Machine learning in mediation analysis - an extension of inverse probability weighting

## Project Overview  

This repository contains the codes and methodologies for estimating treatment effects using **machine learning (ML) models**. The study compares ML methods with inverse probability weighting to explore their potential to identify treatment effects of the Job Corps program.  

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
    - `alc12`: 
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
    - emplq4
    - emplq4full
    - pemplq4,
    - pemplq4mis,
    - vocq4,
    - vocq4mis,
    - health1212,
    - health123,
    - pe_prb12,
    - pe_prb12mis,
    - narry1,
    - numkidhhf1zero,
    - numkidhhf1onetwo,
    - pubhse12,
    - h_ins12a,
    - h_ins12amis

### Models  

#### Causal Mediation
- **Logit Regression**: A replication of Huber's application.

#### Inverse Probability Weighting (IPW)  
- **Logit Regression**: A replication of Huber's application.  

#### Double Machine Learning (DML)  
- **Logit Regression**: A replication of Huber's application.

#### Doubly Robust (DR)  
- **Logit Regression**: A replication of Huber's application.

#### Causal Forest (CF)  
- **Logit Regression**: A replication of Huber's application.

### Evaluation Metrics  
- **F1 Score**: Evaluates model classification performance for imbalanced market state data.  
- **Profit-Based Portfolio Strategy**: Assesses cumulative returns by applying market predictions to an investment strategy.  

### Treatment effects  
1. **Total effect**:
2. **Direct effect**: 
3. **Indirect effect**:

---

## Files  

- `data/`: Dataset used for analysis. 
- `src/`: Scripts for OLS causal mediation, IPW, DML, DR, CF models, as well as treatment effects estimations.  
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

This project showcases how machine learning models can enhance mediation analysis by enhancing precision in treatment effect estimation. Visit the [GitHub repository](https://github.com/yangshutingg/dse4231) for more details!
