# Project Overview: Churn Prediction
This dataset is a fictional HR dataset created by IBM Data Scientists. There is not much backgroud information about where the data is sourced, what each data point means or how each variable is measured . However based on the data I infer that it may be a company related to the production and sales of Pharmaceuticals. For this project I will be analyzing the data to find drivers of churn and develop a model.

## Code Used
R | version
--- | ---
readr            | 1.4.0
dplyr            | 1.0.3
tidyr            | 1.1.2
ggplot           | 3.3.3
RColorBrewer     | 1.1-2
ggpubr           | 0.4.0
purrr            | 0.3.4
broom            | 0.7.3
caret            | 6.0-86
gbm              | 2.1.8
ranger           | 0.12.1
DMwR             | 0.4.1
InformationValue | 1.2.3
DALEX            | 2.0.1
modelplotr       | 1.1.0
doParallel       | 1.0.16

## Business Requirement
1. Find drivers of employee Churn for company A
2. Develop a model to predict Churn

## Data Collection
_The dataset did not come with predefined description. My idea of what each variable means is listed_

Feature | Description
--- | ---
Age                      | employee age
**Attrition "Churn"**    | Yes - Churn; No = Stayed
BusinessTravel           | Non Travel; Travel Rarely; Travel Frequently
DailyRate                | employee pay per day
Department               | Department employee works in
DistanceFromHome         | How far is work from their home
Education                | Below College; College; Bachelor; Master; Doctor
EducationField           | Human Resources; Life Sciences; Marketing; Medical; Technical Degree; Other
EmployeeCount            | employee count
EmployeeNumber           | employee ID
EnvironmentSatisfaction  | Low; Medium; High; Very High
Gender                   | Female; Male
HourlyRate               | employee pay per hour
JobInvolvement           | Low; Medium; High; Very High
JobLevel                 | 1; 2; 3; 4; 5
JobRole                  | Healthcare Representative; Human Resources; Laboratory Technician; Manager; Manufacturing Director; Research Director; Research Scientist; Sales Executive; Sales Representative
JobSatisfaction          | Low; Medium; High; Very High 
MaritalStatus            | Divorced; Married; Single
MonthlyIncome            | employee pay per Month
NumCompaniesWorked       | number of companies an employee has worked before
Over18                   | Y-Yes
OverTime                 | No; Yes
PercentSalaryHike        | increase in salary by percentage
PerformanceRating        | Low; Good; Excellent; Outstanding
RelationshipSatisfaction | Low; Medium; High; Very High
StandardHours            | standard hours per week or 2 weeks (not sure) 
StockOptionLevel         | 0; 1; 2; 3
TotalWorkingYears        | how long has a company been working
TrainingTimesLastYear    | how many times did an employee get trained last year
WorkLifeBalance          | Bad; Good; Better; Best
YearsAtCompany           | how many years has an employee been at the company
YearsInCurrentRole       | how many years has an employee been in their current role
YearsSinceLastPromotion  | how many years has it been since an employee receieved a promotion
YearsWithCurrManager     | how many years has an employee been with their current mamanger

# Exploratory Data Analysis
<img src="Images/TARGET_Analysis/TARGET.PNG" width="600">
<img src="Images/TARGET_Analysis/TARGET_dept.PNG" width="600">

## DEPT: SALES
<img src="Images/TARGET_Analysis/TARGET_Sales_jobrole.PNG" width="600">

### Sales Representatives: 
#### Factors
<p float="left">
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_joblevel.PNG" width="300" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_jobinvolvement.PNG" width="300" /> 
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_businesstravel.PNG" width="300" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_maritalstatus.PNG" width="300" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_overtime.PNG" width="300" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_stockoptionlevel.PNG" width="300" />
</p>

#### Continous
<img src="Images/TARGET_Analysis/TARGET_SalesRep_Corr.PNG" width="700">

# Modeling

## Preprocess
**Split**
* 60% - Training Set
* 20% - Validation Set
* 20% - Test Set

Used 10-Fold Cross Validation to find the best hyperparameters

**GOAL**: To catch a VERY HIGH % of the people that may Churn from the company and rank based on risk of churning. (Based on this the metric I choose to optimize is **Recall** becauses it focuses on predicting the Target Class “Churn” Correctly)

## BASELINE 
## Fit
<img src="Images/MODELING/BASELINE_dt.PNG" width="600">

Hyperparameter | value
--- | ---
Cost Complexity | 0.06

<img src="Images/MODELING/BASELINE_rf.PNG" width="600">

Hyperparameter | value
--- | ---
Num of Feats per Split | 2
Minimal Node Size      | 1
Splitrule              | gini

<img src="Images/MODELING/BASELINE_xgb.PNG" width="600">

Hyperparameter | value
--- | ---
Num of Boosting Iterations | 50
Shrinkage | 0.3
Max Tree Depth      | 1
gamma    | 0
Subsample Ratio of Columns | 0.6
Minimum Sum of Instance Weight | 1
Subsample | 1

<img src="Images/MODELING/BASELINE_svm.PNG" width="600">

Hyperparameter | value
--- | ---
Cost | 16
Sigma | 0.006

<img src="Images/MODELING/BASELINE_elas.PNG" width="600">

Hyperparameter | value
--- | ---
alpha | 0.1
lambda | 0.11

## Diagnostic

Num | Model | Recall
|--- | --- | ---
1 | Logistic Regression | 53%
2 | SVM | 23%
3 | XGB | 11%
4 | Regularized Regression | 6%
5 | Random Forrest | 4%
6 | Decision Tree | 0%

Best **_BASELINE_** Model: **Logistic Regression**

<img src="Images/MODELING/BASELINE_ROC.PNG" width="600">

Num | Model | Recall
| --- | --- | ---
1 | Decision Tree | 100%
2 | Logistic Regression | 77%
3 | SVM | 72%
4 | Regularized Regression | 72%
5 | XGB | 68%
6 | Random Forrest | 55%

Best **_BASELINE + Threshold_** Model: **Logistic Regression with cut off at 16%**. _Suspicious about the 100% Recall from Decision Tree plus the Precision was really poor compared to the other models_.



**NOTE (Class Imbalance)**: 
Their are a number of different Techniques to potentially overcome Class Imbalance when modeling, such as changing the Hyperparameter and Threshold tunning like done previously. Other strategies are Cost Sensitive Algorithms or One-Class Algorithms.

The technique I’m going to used is called Resampling. Resampling is done by a number of ways;
* **Undersampling (Down Sampling)**: removing samples from the majority to match the minority.
* **Oversampling (Up Sampling)**: duplicating samples from the minority class to match the majority class.
* **Synthetic Minority Oversampling Technique (SMOTE)**: synthesize new examples from the minority class using k-nearest neighbors.
* 
## UNDERSAMPLE 
## Fit
<img src="Images/MODELING/DOWN_dt.PNG" width="600">

Hyperparameter | value
--- | ---
Cost Complexity | 0.02

<img src="Images/MODELING/DOWN_rf.PNG" width="600">

Hyperparameter | value
--- | ---
Num of Feats per Split | 41
Minimal Node Size      | 1
Splitrule              | extratrees

<img src="Images/MODELING/DOWN_xgb.PNG" width="600">

Hyperparameter | value
--- | ---
Num of Boosting Iterations | 50
Shrinkage | 0.4
Max Tree Depth      | 1
gamma    | 0
Subsample Ratio of Columns | 0.8
Minimum Sum of Instance Weight | 1
Subsample | 0.625

<img src="Images/MODELING/DOWN_svm.PNG" width="600">

Hyperparameter | value
--- | ---
Cost | 2
Sigma | 0.009

<img src="Images/MODELING/DOWN_elas.PNG" width="600">

Hyperparameter | value
--- | ---
alpha | 0.16
lambda | 0.01

## Diagnostic

Num | Model | Recall
| --- | --- | ---
1 | Regularized Regression| 79%
2 | Logistic Regression | 72%
3 | Random Forrest | 70%
4 | XGB | 70%
5 | SVM | 68%
6 | Decision Tree | 64%

Best **_UNDERSAMPLE_** Model: **Regularized Regression**

<img src="Images/MODELING/DOWN_ROC.PNG" width="600">

Num |Model | Recall
| --- | --- | ---
1 | XGB | 83%
2 | SVM | 77%
3 | Random Forrest | 70%
4 | Logistic Regression | 70%
5 | Regularized Regression | 66%
6 | Decision Tree | 62%

Best **_BASELINE + Threshold_** Model: **Logistic Regression with cut off at 16%**. _Suspicious about the 100% Recall from Decision Tree plus the Precision was really poor compared to the other models_.

