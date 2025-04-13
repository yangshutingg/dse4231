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

1. Compare the performance of ML models (e.g., LASSO, Random Forest, Gradient Boosting Machines) with traditional logit regression models.  
2. Use **profit-based portfolio strategies** to evaluate the practical utility of market state predictions.  
3. Identify key economic and financial predictors across different forecast horizons (short, medium, and long-term).  

---

## Methodology  

### Data  

- **Source**:  
  - **Martin Huber's Dataset**: Job Corps program [Download dataset](http://qed.econ.queensu.ca/jae/datasets/huber001/)  

- **Variables of interest**:  
  - **Treatment (D)**: Assignment to the Job Corps program
  - **Mediator (M)**: Employment 1 to 1.5 years after assignment
  - **Outcome (Y)**: Whether participants reported “very good” health 30 months after assignment
  - **Pre-treatment covariates (X)**: schobef, trainyrbef, jobeverbef, jobyrbef, health012, health0mis,pe_prb0, pe_prb0mis, everalc, alc12, everilldrugs, age_cat, edumis, eduhigh, rwhite, everarr, hhsize, hhsizemis, hhinc12, hhinc8, fdstamp, welf1, welf2, publicass
  - **Post-treatment covariates (W)**: emplq4, emplq4full, pemplq4, pemplq4mis, vocq4, vocq4mis,  health1212, health123,  pe_prb12, pe_prb12mis,  narry1, numkidhhf1zero, numkidhhf1onetwo, pubhse12, h_ins12a, h_ins12amis

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

- `data/`: Preprocessed datasets, including predictors and market state classifications.  
- `src/`: Scripts for traditional and machine learning models, as well as performance metrics calculations and portfolio strategy implementation.  
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

This project showcases how machine learning models can enhance causal mediation analysis by enhancing precision in treatment effect estimation. Visit the [GitHub repository](https://github.com/yangshutingg/dse4231) for more details!  
