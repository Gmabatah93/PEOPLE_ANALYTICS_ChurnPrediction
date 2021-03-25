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

### Sales Representatives
<p float="left">
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_joblevel.PNG" width="100" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_jobinvolvement.PNG" width="100" /> 
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_businesstravel.PNG" width="100" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_maritalstatus.PNG" width="100" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_overtime.PNG" width="100" />
  <img src="Images/TARGET_Analysis/TARGET_SalesRep_stockoptionlevel.PNG" width="100" />
</p>
# Modeling

## Preprocess
**Split**
* 60% - Training Set
* 20% - Validation Set
* 20% - Test Set

Used 10-Fold Cross Validation to find the best hyperparameters

**GOAL**: To catch a VERY HIGH % of the people that may Churn from the company and rank based on risk of churning. (Based on this the metric I choose to optimize is **Recall** becauses it focuses on predicting the Target Class “Churn” Correctly)

## Baseline Fit
<img src="Images/BASELINE_dt.PNG" width="600">
<img src="Images/BASELINE_rf.PNG" width="600">
<img src="Images/BASELINE_xgb.PNG" width="600">
<img src="Images/BASELINE_svm.PNG" width="600">
<img src="Images/BASELINE_elas.PNG" width="600">
