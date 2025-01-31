# Project Overview: Churn Prediction

<img src="Images/CHURN.PNG" width="900" >

[source][1]

> **Seperation**: an employee who leaves an organization voluntarily or involuntary
>  * Voluntary Turnover:
>   + Employee has access to better job alternatives
>   + Employee is unhappy or dissatisfied with current job or organization
<img src="Images/VT.PNG" width="900" >
[Voluntary Turnover][5]
>  * Involuntary Turnover:
>   + Employee failed to meet performance standards
>   + Changes in economic conditions or strategy
>
> **Retention**: process of keeping individuals within the organization who meet or exceed performance standards

## Common Reasons for Employee Churn
>**Toxic Culture**: Sometimes it's not the employees that doesn't fit the workplace culture. It may be the workplace culture doesn't fit with the employees. If a certain workplace environment does not fit with their employees morally or ethically, then that can certainly cause some employees to leave. The culture can stem from other employees to upper management, and even all the way up to the CEO.\
>**Lack of Purpose**: Some employees want to know that their work is meaningful. They want to have a reason to wake up everyday and come to work, or want to feel that their work really matters in the grand scheme of things. It could be that if they feel that their work doesn't really matter they may indirectly feel like they dont matter. If that's the case, then that can also cause employees to leave.\
> **Overwork**: High Stress. Employee burnout also is a common reason for churn.\
> **Boredom**: Similar to Lack of Purpose but taking a different angle, their work might matter greatly to the organization, but they might have hit a ceiling in regards to their work output. In other words they've been doing the samething for a long time now. Although this might now bother some employees it is still another reason why employees leave.\
> **Bad Management**: "People don't leave their jobs. They leave their managers"

[blog][2]

## Problems with Employee Churn
> **Cost**: Estimates suggest that the cost to replace an employee ranges from 90% to 200% of the annual salary of that employee.
[Allen, Bryant, & Vardaman, 2010][4]\
**Time**: Depending on the position, the experience needed, and the employee market. Finding new employees can be difficult. Because of all these constraints it takes time to find someone that's the right fit to a particular organization. Along with finding new employees, getting them up to speed on the day to day can also be quite the task.\
**Team Dynamics and Productivity**: Going in the direction of getting them up to speed, most organizations have teams that rely on each other. Employees coming in and out of those teams can hurt the productivity of an organization, in terms of meeting deadlines or even having the capability of performing certain tasks.

## Summary
**Exploratory Data Analysis**
> * Churn Rate: **16%**
> * Churn Rate By Department: R&D = **14%** | Sales = **21%** | HR = **19%**
> * Features that are correlated with Churn: Job Involvement | Job Level | Job Role | Marital Status | Overtime | Stock Option Level | Business Travel\

**Modeling**
> * **_Validation Results_**
* **_Test Results_**




> Choosen Top Model: **Logistic Regression-Up**
>
> **Feature Selection**

### Code Used

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

### Business Requirement
1. Find drivers of employee Churn for company A
2. Develop a model to predict Churn

### Data Collection
> This dataset is a fictional HR dataset created by IBM Data Scientists. There is not much background information about where the data is sourced, what each data point means or how each variable is measured . However based on the data I infer that it may be a company related to the production and sales of Pharmaceuticals. For this project I will be analyzing the data to find drivers of churn and develop a model.

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

### Table Of Contents

- [Exploratory Data Analysis](https://github.com/Gmabatah93/PEOPLE_Analytics_ChurnPrediction#exploratory-data-analysis)
- [Modeling](https://github.com/Gmabatah93/PEOPLE_Analytics_ChurnPrediction#modeling)
- [Test Results](https://github.com/Gmabatah93/PEOPLE_Analytics_ChurnPrediction#test-results)
- [Feature Selection](https://github.com/Gmabatah93/PEOPLE_Analytics_ChurnPrediction#feature-selection)
- [Risk Bucket](https://github.com/Gmabatah93/PEOPLE_Analytics_ChurnPrediction#risk-bucket)
- [Prescriptive Analysis](https://github.com/Gmabatah93/PEOPLE_Analytics_ChurnPrediction/blob/main/README.md#prescriptive-analysis)


# Exploratory Data Analysis
<img src="Images/TARGET_Analysis/TARGET.PNG" width="900" >
<img src="Images/TARGET_Analysis/TARGET_dept.PNG" width="800">

## DEPARTMENT: Sales
<img src="Images/TARGET_Analysis/TARGET_Sales_jobrole.PNG" width="600">

### Sales Representatives:

> #### Factors

<p float="left">
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_joblevel.PNG" width="300" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_jobinvolvement.PNG" width="300" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_businesstravel.PNG" width="300" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_maritalstatus.PNG" width="300" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_overtime.PNG" width="300" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_stockoptionlevel.PNG" width="300" />
</p>

> #### Continous

<img src="Images/TARGET_Analysis/TARGET_SalesRep_Corr.PNG" width="700">

# Modeling

## Preprocess

> **Split**
> * 60% - Training Set
> * 20% - Validation Set
> * 20% - Test Set

> Used 10-Fold Cross Validation to find the best hyperparameters

> **GOAL**: To catch a VERY HIGH % of the people that may Churn from the company and rank based on risk of churning. (Based on this the metric I choose to optimize is **Recall** becauses it focuses on predicting the Target Class “Churn” Correctly)

## BASELINE

> ### Fit

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

> ### Diagnostic

<img src="Images/MODELING/BASELINE_ROC.PNG" width="600">

Num | Model | Recall | | Num | Model | Recall
| --- | --- | --- | --- | --- | --- | ---
1 | Logistic Regression     | 53% |   | 1 | Decision Tree.16          | 100%
2 | SVM                     | 23% |   | 2 | Logistic Regression.16    | 77%
3 | XGB                     | 11% |   | 3 | SVM.13                    | 72%
4 | Regularized Regression  | 6%  |   | 4 | Regularized Regression.21 | 72%
5 | Random Forrest          | 4%  |   | 5 | XGB.21                    | 68%
6 | Decision Tree           | 0%  |   | 6 | Random Forrest.21         | 55%

> * Best **_BASELINE_** Model: **Logistic Regression**
> * Best **_BASELINE + Threshold_** Model: **Logistic Regression with cut off at 16%**. _Suspicious about the 100% Recall from Decision Tree plus the Precision was really poor compared to the other models_.




## UNDERSAMPLE
> **NOTE (Class Imbalance)**:
Their are a number of different Techniques to potentially overcome Class Imbalance when modeling, such as changing the Hyperparameter and Threshold tunning like done previously. Other strategies are Cost Sensitive Algorithms or One-Class Algorithms.

> The technique I’m going to used is called Resampling. Resampling is done by a number of ways;
* **Undersampling (Down Sampling)**: removing samples from the majority to match the minority.
* **Oversampling (Up Sampling)**: duplicating samples from the minority class to match the majority class.
* **Synthetic Minority Oversampling Technique (SMOTE)**: synthesize new examples from the minority class using k-nearest neighbors.

### Fit
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

### Diagnostic

<img src="Images/MODELING/DOWN_ROC.PNG" width="600">

Num | Model | Recall | | Num | Model | Recall
| --- | --- | --- | --- | --- | --- | ---
1 | Regularized Regression (DOWN)  | 79% |  | 1 | XGB.43 (DOWN)                     | 83%
2 | Logistic Regression (DOWN)     | 72% |  | 2 | SVM.46 (DOWN)                     | 77%
3 | Random Forrest (DOWN)          | 70% |  | 3 | Random Forrest.55 (DOWN)          | 70%
4 | XGB (DOWN)                     | 70% |  | 4 | Logistic Regression.79 (DOWN)     | 70%
5 | SVM (DOWN)                     | 68% |  | 5 | Regularized Regression.72 (DOWN)  | 66%
6 | Decision Tree (DOWN)           | 64% |  | 6 | Decision Tree.79 (DOWN)           | 62%

> Best **_UNDERSAMPLE_** Model: **Regularized Regression**
> Best **_UNDERSAMPLE + Threshold_** Model: **XGB with cut off at 43%**.

## OVERSAMPLE
### Fit
<img src="Images/MODELING/UP_dt.PNG" width="600">

Hyperparameter | value
--- | ---
Cost Complexity | 0

<img src="Images/MODELING/UP_rf.PNG" width="600">

Hyperparameter | value
--- | ---
Num of Feats per Split | 5
Minimal Node Size      | 1
Splitrule              | gini

<img src="Images/MODELING/UP_xgb.PNG" width="600">

Hyperparameter | value
--- | ---
Num of Boosting Iterations | 200
Shrinkage | 0.3
Max Tree Depth      | 4
gamma    | 0
Subsample Ratio of Columns | 0.6
Minimum Sum of Instance Weight | 1
Subsample | 0.875

<img src="Images/MODELING/UP_svm.PNG" width="600">

Hyperparameter | value
--- | ---
Cost | 16
Sigma | 0.01

<img src="Images/MODELING/UP_elas.PNG" width="600">

Hyperparameter | value
--- | ---
alpha | 0.74
lambda | 0.001

## Diagnostic

<img src="Images/MODELING/UP_ROC.PNG" width="600">

Num | Model | Recall | | Num | Model | Recall
| --- | --- | --- | --- | --- | --- | ---
1 | Logistic Regression (UP)    | 70% |   | 1 | Random Forrest.24 (UP)        | 79%
2 | Regularized Regression (UP) | 68% |   | 2 | Logistic Regression.23 (UP)   | 74%
3 | XGB (UP)                    | 45% |   | 3 | Regularized Regression.6 (UP) | 68%
4 | Decision Tree (UP)          | 43% |   | 4 | XGB.2 (UP)                    | 55%
5 | SVM (UP)                    | 19% |   | 5 | Decision Tree.23 (UP)         | 53%
6 | Random Forrest (UP)         | 17% |   | 6 | SVM.03 (UP)                   | 47%

* Best **_OVERSAMPLE_** Model: **Logistic Regression**
* Best **_OVERSAMPLE + Threshold_** Model: **Random Forrest with cut off at 79%**.


## SMOTE
### Fit
<img src="Images/MODELING/SMOTE_dt.PNG" width="600">

Hyperparameter | value
--- | ---
Cost Complexity | 0.02

<img src="Images/MODELING/SMOTE_rf.PNG" width="600">

Hyperparameter | value
--- | ---
Num of Feats per Split | 2
Minimal Node Size      | 1
Splitrule              | gini

<img src="Images/MODELING/SMOTE_xgb.PNG" width="600">

Hyperparameter | value
--- | ---
Num of Boosting Iterations | 50
Shrinkage | 0.3
Max Tree Depth      | 1
gamma    | 0
Subsample Ratio of Columns | 0.6
Minimum Sum of Instance Weight | 1
Subsample | 0.625

<img src="Images/MODELING/SMOTE_svm.PNG" width="600">

Hyperparameter | value
--- | ---
Cost | 4
Sigma | 0.01

<img src="Images/MODELING/SMOTE_elas.PNG" width="600">

Hyperparameter | value
--- | ---
alpha | 0.94
lambda | 0.11

### Diagnostic

<img src="Images/MODELING/SMOTE_ROC.PNG" width="600">

Num | Model | Recall | | Num | Model | Recall
| --- | --- | --- | --- | --- | --- | ---
1 | Logistic Regression (SMOTE)    | 57% |   | 1 | Decision Tree.14 (SMOTE)         | 79%
2 | SVM (SMOTE)                    | 45% |   | 2 | Regularized Regression.4 (SMOTE) | 79%
3 | Decision Tree (SMOTE)          | 38% |   | 3 | XGB.23 (SMOTE)                   | 77%
4 | XGB (SMOTE)                    | 36% |   | 4 | Logistic Regression.14 (SMOTE)   | 77%
5 | Regularized Regression (SMOTE) | 26% |   | 5 | Random Forrest.29 (SMOTE)        | 74%
6 | Random Forrest (SMOTE)         | 11% |   | 6 | SVM.22 (SMOTE)                   | 72%

* Best **_SMOTE_** Model: **Logistic Regression**
* Best **_SMOTE + Threshold_** Model: **Decision Tree cut off at 14%**.

## Test Results

Num | Model | Recall | Precision | F1
| --- | --- | --- | --- | ---
1  | XGB.43 (DOWN)                       | 87% | 31% | 0.46
2  | Logistic Regression.23 (UP)         | 85% | 47% | 0.60
3  | Logistic Regression (UP)            | 85% | 47% | 0.61
4  | SVM.13                              | 85% | 36% | 0.51
5  | Logistic Regression (DOWN)          | 85% | 34% | 0.49
6  | Logistic Regression.16 (DOWN)       | 83% | 46% | 0.59
7  | XGB.23 (SMOTE)                      | 83% | 33% | 0.47
8  | Regularized Regression.40 (SMOTE)   | 83% | 28% | 0.42
9  | SVM.46 (DOWN)                       | 81% | 32% | 0.46
10 | Decision Tree.24 (SMOTE)            | 81% | 22% | 0.35

**Note (Test Results)**: Top Models I choose for further evaluation
1. **XGB-Down (Thres 43%)**: Performed the best overall in terms of Recall
2. **Logistic Regression-Up**: Performed nearly as well as the best model with a better much Precision (Best in terms of F1 score)
3. **SVM (Thres 13%)**: Perform as well as the best models with no Resampling

### Model Performance

<p float="left">
  <img src="Images/MODELING/GAIN.PNG" width="500" />
  <img src="Images/MODELING/LIFT.PNG" width="500" />
</p>

Num | Model | Percentile | GAIN | LIFT
| --- | --- | --- | --- | ---
1  | Logistic Regression-Up | 20th | 70.2% | 3.5x
2  | SVM (Thres 13%)        | 20th | 61.7% | 3x
3  | XGB-Down (Thres 43%)   | 20th | 57.4% | 2.86x
   |                        |      |       |
1  | Logistic Regression-Up | 30th | 85.1% | 2.84x
2  | SVM (Thres 13%)        | 30th | 76.6% | 2.56x
3  | XGB-Down (Thres 43%)   | 30th | 70.2% | 2.35x
   |                        |      |       |
1  | Logistic Regression-Up | 50th | 91.5% | 1.83x
1  | SVM (Thres 13%)        | 50th | 91.5% | 1.83x
3  | XGB-Down (Thres 43%)   | 50th | 87.2% | 1.74x

**Note (20th Percentile)**:
* **Logistic Regression-Up** performed the best compared to the other models capturing **70%** of employees that Churned performing 3.5x better than random
* **SVM (Thres 13%)** came in 2nd with a Gain of **61.7%** perfroming 3x better than random

**Note (30th Percentile)**:
* **Logistic Regression-Up** performed the best compared to the other models capturing **85%** of employees that Churned performing **2.84x** better than random
* **SVM (Thres 13%)** came in 2nd with a Gain of **76.6%** perfroming **2.56x** better than random

**Note (50th Percentile)**:
* **Logistic Regression-Up & SVM (Thres 13%)** performed the best capturing **91.5%** of employees that Churned performing **1.83x** better than random

# Feature Selection
<p float="left">
  <img src="Images/MODELING/FEATURE_log_up.PNG" width="500" />
  <img src="Images/MODELING/FEATURE_svm.PNG" width="500" />
</p>

# Risk Bucket

<img src="Images/RISK/RISK_High_Level.PNG" width="500" />

**Note (High Level)**:
* **Logistic Regression-Up** identified **69**:(38-High | 31-Med) Risk employees
* **SVM (Thres 13%)** identified **15**:(5-High | 8-Med) Risk employees

_Moving forward only evaluating Logistic Regression-Up_

<img src="Images/RISK/RISK_Department.PNG" width="500" />

**Note (Department)**: Of the 69 high-med risk employees
* R&D Department has **42**:(22-High | 20-Med)
* Sales Department has **24**:(15-High | 9-Med)

<p float="left">
  <img src="Images/RISK/RISK_Research_jobrole.PNG" width="500" />
  <img src="Images/RISK/RISK_Sales_jobrole.PNG" width="500" />
</p>

**Note (Jobrole)**:
* **19**:(11-High | 8-Med) are **Lab Techs**
* **16**:(9-High | 7-Med) are **Research Scientist**
* **12**:(8-High | 4-Med) are **Sales Reps**
* **11**:(6-High | 5-Med) are **Sales Execs**

## Risk Bucket: Overtime

<img src="Images/RISK/RISK_Overtime.PNG" width="500" />

<img src="Images/RISK/RISK_Overtime_jobrole.PNG" width="500" />

**Note (Overtime)**: employees that have low, medium, and high risk work significantly more Overtime hours than employees with no risk
* **13**:(8-High | 5-Med) are **Research Scientist**
* **10**:(7-High | 3-Med) are **Lab Techs**
* **8**:(3-High  | 5-Med) are **Sales Executives**
* **5**:(4_High  | 1-Med) are **Sales Reps**

## Risk Bucket: Years with Current Manager

<img src="Images/RISK/RISK_YearsWithCurrentManager.PNG" width="500" />

<img src="Images/RISK/RISK_YearsWithCurrentManager_jobrole.PNG" width="500" />

**Note (YearsWithCurrentManager)**: Employees that have low, medium and high risk generally have worked with the current manager for a lesser amount of years
* as Years with Current Manager Decreases will on average clearly effect **Sales Reps**

## Risk Bucket: JobLevel

<img src="Images/RISK/RISK_JobLevel.PNG" width="500" />

<img src="Images/RISK/RISK_JobLevel_jobrole.PNG" width="500" />

**Note (JobLevel)**: Employees that have medium and high risk work are mostly JobLevel 1
* **18**:(11-High | 7-Med) are **Lab Techs**
* **15**:(8-High  | 7-Med) are **Research Scientist**
* **12**:(8-High  | 4-Med) are **Sales Reps**

# Risk Bucket: Environment Satisfaction

<img src="Images/RISK/RISK_EnvironmentSatisfaction.PNG" width="500" />

<img src="Images/RISK/RISK_EnvironmentSatisfaction_jobrole.PNG" width="500" />

**Note (EnvironmentSatisfaction)**: Employees that have medium and high risk tend to a LOW Environment Satisfaction
* **7**:(5-High | 2-Med) are **Lab Techs**
* **7**:(4-High | 3-Med) are **Research Scientist**
* **4**:(4-High) are **Sales Execs**

# Prescriptive Analysis

> ## Overtime

<img src="Images/PRESCRIPTIVE/OVERTIME_Actual.PNG" width="500">

> **Note (Actuals):**
> * **_Laboratory Technician_**: 18 Churn | 61% Overtime
> * **_Research Scientis_**: 8 Churn | 75% Overtime
> * **_Sales Executives_**: 8 Churn | 62% Overtime
> * **_Sales Representative_**: 7 Churn | 43% Overtime

<img src="Images/PRESCRIPTIVE/OVERTIME_Prediction.PNG" width="500">

> **Note (Predictions):**
> * **_Laboratory Technician_**: 15:(7-High | 3-Med | 5-Low)
> * **_Research Scientist_**: 14:(8-High | 5-Med | 1-Low)
> * **_Sales Executive_**: 8: (3-High | 5-Med)
> * **_Sales Representative_**: 5: (4-High | 1-Med)

> ### STRATEGY:

For the employees that actually churned. They had a very high Overtime %. I think the focus should be first focused on **Laboratory Technicians** _(18/56 Labs Techs from the test set actually churned)_. This may need a big change that may effect they current work flow process so we'd have to bring in multiple stakeholders to see if we can come up with a solution


> ## Job Level 1

<img src="Images/PRESCRIPTIVE/JOBLEVEL_Actual.PNG" width="500">

> **Note (Actual)**:
> * **_Laboratory Technician_**: 16/41 Churned
> * **_Research Scientist_**: 8/46 Churned
> **_Sales Representative_**:  7/14  Churned

<img src="Images/PRESCRIPTIVE/JOBLEVEL_Prediction.PNG" width="500">

> **Note (Prediction)**:
> * **_Laboratory Technician_**: 24:(11-High | 7-Med | 6-Low)
> * **_Research Scientist_**: 16:(8-High  | 7-Med | 1-Low)
> * **_Sales Representative_**: 12:(8-High  | 4-Med)

> ### STRATEGY:

Assuming JobLevel 1 is Entry Level, HR and Management can use this model to help facilitate in recruiting and hiring. Identifying future canidates into "low, medium, and high risk categories could help Management decide on which canidates to pursue further and how based off talent and level of risk. Hiring more “no to low risk” canidates could potentially decrease the overall Churn Rate for the company in future. Also identifying med-high risk canidates and employees along with what they contribute to the company can further help leaders decide on what to do going forward.


> ## Environment Satisfaction

<img src="Images/PRESCRIPTIVE/ENVIRONMENT_Actual.PNG" width="500">

> **Note (Actual)**:
> * **_Laboratory Technician_**: 6/11  Churned
> * **_Research Scientist_**: 3/11  Churned
> * **_Sales Executive_**: 2/15  Churned

<img src="Images/PRESCRIPTIVE/ENVIRONMENT_Prediction.PNG" width="500">

> **Note (Prediction)**:
> * **_Laboratory Technician_**: 7:(5-High | 2-Med)
> * **_Research Scientist_**: 7:(4-High | 3-Med)
> * **_Sales Executive_**: 4:(4-High)

> ### STRATEGY:

It looks like the primary concern is for Labratory Technicians (54% leave when environment satisfaction is low). Perform survey to assess the current state of conditions for Lab Techs, that includes phyical environment, and culture. From the results we can then work with employees and management to come up with a variety of potential solutions. Finally work with stakeholders to find out which solution to implement first. Once we decide on a solution I will conduct an A/B Test using the same survey and test for statistical significance after implementing our solution.


[1]: https://www.digitalhrtech.com/high-turnover-meaning-rates/
[2]: https://blog.jostle.me/blog/5-causes-of-employee-turnover#:~:text=On%20the%20other%20hand%2C%20a,meaning%2C%20boredom%20can%20set%20in.
[3]: https://smallbusiness.chron.com/problems-high-turnover-rates-11659.html
[4]: https://google.com
[5]: https://www.youtube.com/watch?v=I-kn24DQITI&list=PLKkRkURCtPjDmdT0oJ52ApgNE5BKO1DWo&index=2&ab_channel=DavidCaughlin
