library(readr)            # Load Data
library(dplyr, warn.conflicts = FALSE); options(dplyr.summarise.inform = FALSE) # Data Manipulation
library(tidyr)            # Data Manipulation
library(ggplot2)          # Data Visualization
library(RColorBrewer)     # Color Schemes
library(ggpubr)           # Multiplot
library(purrr)            # Function: Lists
library(broom)            # Data Manipulation: Stat Tests
library(caret)            # Model Fit
library(gbm)              # Model Fit
library(ranger)           # Model Fit
library(DMwR)             # Smote resample
library(InformationValue) # Optimal Thresholds
library(DALEX)            # Feature Selection and Model Performance 
library(modelplotr)       # Model Performance
library(doParallel)       # Parallel Processing

## DATA ----

# Load csv
hr_raw <- read_csv("HR-Employee-Attrition.csv")

# Clean data: Factors
hr <- hr_raw %>%
  mutate(Attrition = factor(ifelse(Attrition == "Yes", 1,0),
                            labels =  c("Stayed", "Churn")),
         BusinessTravel = factor(BusinessTravel,
                                 levels = c("Non-Travel","Travel_Rarely","Travel_Frequently")),
         Department = factor(Department),
         EducationField = factor(EducationField),
         Gender = factor(Gender),
         JobLevel = factor(JobLevel),
         JobRole = factor(JobRole),
         MaritalStatus = factor(MaritalStatus),
         Over18 = factor(Over18),
         OverTime = factor(OverTime),
         StockOptionLevel = factor(StockOptionLevel),
         Education = factor(Education,
                            levels = c(1:5), 
                            labels = c("Below College","College","Bachelor","Master","Doctor")),
         EnvironmentSatisfaction = factor(EnvironmentSatisfaction,
                                          levels = c(1:4),
                                          labels = c("Low","Medium","High","Very High")),
         JobInvolvement = factor(JobInvolvement,
                                 levels = c(1:4),
                                 labels = c("Low","Medium","High","Very High")),
         JobSatisfaction = factor(JobSatisfaction,
                                  levels = c(1:4),
                                  labels = c("Low","Medium","High","Very High")),
         PerformanceRating = factor(PerformanceRating,
                                    levels = c(1:4),
                                    labels = c("Low","Good","Excellent","Outstanding")),
         RelationshipSatisfaction = factor(RelationshipSatisfaction,
                                           levels = c(1:4),
                                           labels = c("Low","Medium","High","Very High")),
         WorkLifeBalance = factor(WorkLifeBalance,
                                  levels = c(1:4),
                                  labels = c("Bad","Good","Better","Best")))

# View 
hr %>% glimpse()

## EDA: Counts ----

# Plot: Department
gg_Department <- hr %>% 
  ggplot(aes(Department)) +
  geom_bar(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Department") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'navyblue'),
    axis.text.x = element_text(angle = 30, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
# - view
gg_Department
# - note: most employees are in the R&D Department

# Plot: Gender
gg_Gender <- hr %>% 
  ggplot(aes(Gender)) +
  geom_bar(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Gender") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'navyblue'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
# - view
gg_Gender
# - note: slightly more males than females

# Plot: Marital Status
gg_MaritalStatus <- hr %>% 
  ggplot(aes(MaritalStatus)) +
  geom_bar(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Marital Status") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'navyblue'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
# - view
gg_MaritalStatus
# - note: most employees are married

# Plot: Education
gg_Education <- hr %>%  
  ggplot(aes(Education)) +
  geom_bar(alpha = 0.6) +
  facet_wrap(~ EducationField) +
  theme_minimal() +
  labs(title = "Education") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'navyblue'),
    axis.text.x = element_text(angle = 30, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
# - view
gg_Education
# - note: most employees have a Bachelors from the Life Sciences or Medical Fields.

# Plot: Job Role
gg_JobRole <- hr %>% 
  ggplot(aes(JobRole)) +
  geom_bar(alpha = 0.6) + 
  facet_wrap(~ Department) +
  theme_minimal() + 
  labs(title = "Job Role") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'navyblue'),
    axis.text.x = element_text(angle = 70, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
# - view
gg_JobRole
# - note: most employees in the R&D Dept are: [Research Scientist | Labatory Technician | Manufactoring Director], in the Sales Dept: [Sales Executive]

## EDA: Bivariate Analysis ----

# AGE
# Average Age
avg_Age <- hr %>% summarise(avg_Age = mean(Age)) %>% pull(avg_Age)

# Plot: Age
gg_Age <- hr %>% 
  ggplot(aes(Age)) +
  geom_histogram(bins = 10, fill = "grey60", alpha = 0.5) +
  geom_vline(xintercept = avg_Age, linetype = 3, color = "red") +
  labs(title = "Age") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = 'bold', color = 'navyblue'),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )
# - visual
gg_Age
# - note: average age of employees is 36yrs

# Plot: Age By Gender
gg_Age_Gender <- hr %>% 
  ggplot(aes(Age, fill = Gender)) +
  geom_density(alpha = 0.3) +
  labs(title = "By Gender") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'blue'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Dark2")
# - visual
gg_Age_Gender
# - note: nothing significant

# Plot: Age By Department
gg_Age_Dept <- hr %>% 
  ggplot(aes(Age, fill = Department)) +
  geom_density(alpha = 0.3) +
  labs(title = "By Department") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'blue'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Dark2")
# - visual
gg_Age_Dept
# - note: nothing too significant

# Plot: Age By Job Role
gg_Age_JobRole <- hr %>% 
  ggplot(aes(Age, fill = JobRole)) +
  geom_density(alpha = 0.3) +
  labs(title = "By Job Role") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'blue'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set3")
# - visual
gg_Age_JobRole
# - note: average age of Managers (46yrs), Research Directors (44yrs)


# MONTHLY INCOME
# Average Monthly Income
avg_MI <- hr %>% summarise(avg_MI = mean(MonthlyIncome)) %>% pull(avg_MI)

# Plot: Monthly Income
gg_Income <- hr %>% 
  ggplot(aes(MonthlyIncome)) +
  geom_histogram(bins = 50, fill = "grey60", alpha = 0.5) + 
  geom_vline(xintercept = avg_MI, linetype = 3, color = "red") +
  labs(title = "Monthly Income") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = 'bold', color = 'navyblue'),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )
# - visual
gg_Income
# - note: average Monthly Income is $6,502.93
# Plot: Monthly Income By Gender
gg_Income_Gender <- hr %>% 
  ggplot(aes(MonthlyIncome, fill = Gender)) +
  geom_density(alpha = 0.3) + 
  labs(title = "By Gender") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'blue'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Dark2")
# - visual
gg_Income_Gender
# - note: nothing significant

# Plot: Monthly Income By Department
gg_Income_Dept <- hr %>% 
  ggplot(aes(MonthlyIncome, fill = Department)) +
  geom_density(alpha = 0.3) + 
  labs(title = "By Department") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'blue'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Dark2")
# - visual
gg_Income_Dept
# - note: average Monthly Income for the HR Dept ($6,655), R&D Dept ($6,281), Sales Dept ($6,959)

# Plot: Monthly Income By JobRole
gg_Income_JobRole <- hr %>% 
  ggplot(aes(MonthlyIncome, fill = JobRole)) +
  geom_density(alpha = 0.3) + 
  labs(title = "By Job Role") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold', color = 'blue'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set3")
# - visuals
gg_Income_JobRole
# - note: average Monthly Income for Manufactoring Directors ($7,295), Healthcare Representative ($7,529), Research Director ($16,034), Manager ($17,182)



## EDA: Target Analysis ----

# Plot: Target (Churn)
gg_Target <- hr %>%
  ggplot(aes(Attrition, fill = Attrition)) +
  geom_bar(show.legend = FALSE) +
  labs(title = 'Target: Attrition "Churn Rate"',
       subtitle = "Stayed: 84% | Churn: 16%") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 25, face = 'bold', hjust = 0.5, color = "darkred"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "red"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  scale_fill_manual(values = c("grey40", "coral4"))
# - visual
gg_Target
# - note: class imbalance (16% Churn)
# - Statistical Test: Chi-Sq
tibble(
  Predictor = c("Gender", "Department"),
  Statistic = c(chisq.test(hr$Gender, hr$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr$Department, hr$Attrition) %>% tidy() %>% pull(statistic)) %>% round(2),
  Df = c(chisq.test(hr$Gender, hr$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr$Department, hr$Attrition) %>% tidy() %>% pull(parameter)),
  P.Value = c(chisq.test(hr$Gender, hr$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr$Department, hr$Attrition) %>% tidy() %>% pull(p.value)) %>% round(3)
)
# - note: Churn Rate by Gender is not significant, however by Department is Significant.

# Churn Rate: For Entire Company
hr_AR <- hr %>% 
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>% 
  summarise(Attrition = round(mean(Attrition),2)) %>% 
  pull(Attrition)

# Plot: Churn Rate By Department
gg_Target_Dept <- hr %>% 
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>%
  group_by(Department) %>% 
  summarise(Attrition_Rate = mean(Attrition)) %>% 
  mutate(cond = factor(case_when(
    Attrition_Rate > hr_AR & Department == "Sales"           ~ "S",
    Attrition_Rate > hr_AR & Department == "Human Resources" ~ "HR",
    TRUE ~ "RD"))) %>% 
  ggplot(aes(Department, Attrition_Rate)) +
  geom_col(aes(fill = cond), show.legend = FALSE) + geom_hline(yintercept = hr_AR, linetype = 5, color = "red") +
  geom_label(aes(label = round(Attrition_Rate,2)), size = 5) +
  theme_bw() + theme(
    plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, color = "darkred"),
    plot.subtitle = element_text(color = "red", face = "italic", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) + 
  scale_fill_manual(values = c("#1B9E77", "grey88", "#7570B3")) +
  labs(
    title = "Churn Rate: By Department",
    subtitle = "Attrition Rate (Overall): 16%"
  )
# - visual
gg_Target_Dept
# - note: Churn Rate for the HR Department (19%) & SALES (21%) are above the company avg

## EDA: Target Analysis - SALES Dept ----

# Filter: SALES Department
hr_Sales <- hr %>% filter(Department == "Sales")

# Statistical Test: Chi-Sq Test (SALES Department)
tibble(
  Predictor = c("Business Travel", "JobInvolvement", "Job Level", "Job Role", "Marital Status", "Over Time", "Education",
                "Education Field", "Environmenatal Satisfaction", "Gender", "Job Satisfaction", "Performance Rating",
                "Relationship Satisfaction", "Stock Option Level", "Work Life Balance"),
  Statistic = c(chisq.test(hr_Sales$BusinessTravel, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$JobInvolvement, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$JobLevel, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$JobRole, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$MaritalStatus, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$OverTime, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$Education, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$EducationField, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$EnvironmentSatisfaction, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$Gender, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$JobSatisfaction, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$PerformanceRating, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$RelationshipSatisfaction, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$StockOptionLevel, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2),
                chisq.test(hr_Sales$WorkLifeBalance, hr_Sales$Attrition) %>% tidy() %>% pull(statistic) %>% round(2)),
  Df = c(chisq.test(hr_Sales$BusinessTravel, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$JobInvolvement, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$JobLevel, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$JobRole, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$MaritalStatus, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$OverTime, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$Education, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$EducationField, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$EnvironmentSatisfaction, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$Gender, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$JobSatisfaction, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$PerformanceRating, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$RelationshipSatisfaction, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$StockOptionLevel, hr_Sales$Attrition) %>% tidy() %>% pull(parameter),
         chisq.test(hr_Sales$WorkLifeBalance, hr_Sales$Attrition) %>% tidy() %>% pull(parameter)),
  P.Value = c(chisq.test(hr_Sales$BusinessTravel, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$JobInvolvement, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$JobLevel, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$JobRole, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$MaritalStatus, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$OverTime, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$Education, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$EducationField, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$EnvironmentSatisfaction, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$Gender, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$JobSatisfaction, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$PerformanceRating, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$RelationshipSatisfaction, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$StockOptionLevel, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3),
              chisq.test(hr_Sales$WorkLifeBalance, hr_Sales$Attrition) %>% tidy() %>% pull(p.value) %>% round(3))
) %>% arrange(P.Value)
# - note: Signicant features: JobInvolvement | JobLevel | JobRole | Marital Status | OverTime | StockOptionLevel | BusinessTravel

# Churn Rate: For (SALES) Department
hr_Sales_AR <- hr_Sales %>% 
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>% 
  summarise(Attrition = round(mean(Attrition),2)) %>% 
  pull(Attrition)
# Label for Plot
hr_Sales_DF <- tibble(label = c("Attrition Rate: 21%"))

# Plot: Churn Rate By Job Role (SALES)
gg_Target_Sales_JobRole <- hr_Sales %>%
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>% 
  group_by(JobRole) %>% 
  summarise(Attrition_Rate = mean(Attrition)) %>% 
  mutate(con = factor(ifelse(Attrition_Rate > hr_Sales_AR, 1,0))) %>% 
  ggplot(aes(JobRole, Attrition_Rate)) +
  geom_col(aes(fill = con), show.legend = FALSE) + geom_hline(yintercept = hr_Sales_AR, linetype = 5, color = "red") +
  geom_label(aes(label = round(Attrition_Rate,2))) +
  geom_text(
    data = hr_Sales_DF,
    mapping = aes(x = .8, y = .22, label = label),
    color = "red"
  ) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(color = "blue", face = "italic", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x =  element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_fill_manual(values = c("grey80","#7570B3")) +
  labs(
    title = "Churn Rate: By Job Role",
    subtitle = "Department: Sales"
  )
# - visual
gg_Target_Sales_JobRole
# - note: Sales Representatives have a churn rate of 40%

# Filter: Department = SALES, JobRole = Sales Representative
hr_Sales_SalesRep <- hr_Sales %>% filter(JobRole == "Sales Representative")

# Plot: Churn Rate By Job Level (Sales Representative)
gg_Target_SalesRep_JobLevel <- hr_Sales_SalesRep %>%
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>% 
  group_by(JobRole, JobLevel) %>% 
  summarise(Attrition_Rate = mean(Attrition)) %>% 
  mutate(con = factor(ifelse(Attrition_Rate > hr_Sales_AR, 1,0))) %>% 
  ggplot(aes(JobLevel, Attrition_Rate)) +
  geom_col(aes(fill = con), show.legend = FALSE) + geom_hline(yintercept = hr_Sales_AR, linetype = 5, color = "red") +
  geom_label(aes(label = round(Attrition_Rate, 2)), size = 3) +
  facet_wrap(~ JobRole) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_fill_manual(values = c("grey80","#7570B3")) +
  labs(
    title = "By Job Level"
  )
# - visual
gg_Target_SalesRep_JobLevel
# - note: Sales Representives with a Job Level 1 have a Churn Rate of 42%

# Plot: Churn Rate By Job Involvement (Sales Representative)
gg_Target_SalesRep_JobInvolvement <- hr_Sales_SalesRep %>% 
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>% 
  group_by(JobRole, JobInvolvement) %>% 
  summarise(Attrition_Rate = mean(Attrition),
            n = n()) %>% 
  mutate(con = factor(ifelse(Attrition_Rate > hr_Sales_AR, 1,0))) %>% 
  ggplot(aes(JobInvolvement, Attrition_Rate)) +
  geom_col(aes(fill = con), show.legend = FALSE) + geom_hline(yintercept = hr_Sales_AR, linetype = 5, color = "red") +
  geom_label(aes(label = round(Attrition_Rate,2)), size = 3) +
  facet_wrap(~ JobRole) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_fill_manual(values = c("#7570B3","grey28")) +
  labs(
    title = "By Job Involvement"
  )
# - visual
gg_Target_SalesRep_JobInvolvement
# - note: sales reps with LOW JobInvolvement have a Churn rate of 70%, MED(50%), HIGH(31%), Very High(40%)

# Plot: Churn Rate By Business Travel (Sales Representative)
gg_Target_SalesRep_BusinessTravel <- hr_Sales_SalesRep %>% 
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>% 
  group_by(JobRole, BusinessTravel) %>% 
  summarise(Attrition_Rate = mean(Attrition)) %>% 
  mutate(con = factor(ifelse(Attrition_Rate > hr_Sales_AR, 1,0))) %>% 
  ggplot(aes(BusinessTravel, Attrition_Rate)) +
  geom_col(aes(fill = con), show.legend = FALSE) + geom_hline(yintercept = hr_Sales_AR, linetype = 5, color = "red") +
  geom_label(aes(label = round(Attrition_Rate, 2)), size = 3) +
  facet_wrap(~ JobRole) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_fill_manual(values = c("grey28","#7570B3")) +
  labs(
    title = "By Business Travel"
  )
# - visual
gg_Target_SalesRep_BusinessTravel
# - note: sales reps that travel Frequently have a churn rate of (65%), Rarely(33%)

# Plot: Churn Rate By Marital Status (Sales Representative)
gg_Target_SalesRep_MaritalStatus <- hr_Sales_SalesRep %>% 
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>% 
  group_by(JobRole, MaritalStatus) %>% 
  summarise(Attrition_Rate = mean(Attrition)) %>% 
  mutate(con = factor(ifelse(Attrition_Rate > hr_Sales_AR, 1,0))) %>% 
  ggplot(aes(MaritalStatus, Attrition_Rate)) +
  geom_col(aes(fill = con), show.legend = FALSE) + geom_hline(yintercept = hr_Sales_AR, linetype = 5, color = "red") +
  geom_label(aes(label = round(Attrition_Rate,2)), size = 3) +
  facet_wrap(~ JobRole) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_fill_manual(values = c("#7570B3")) +
  labs(
    title = "By Marital Status"
  )
# - visual
gg_Target_SalesRep_MaritalStatus
# - note: Single Sales reps have a Churn Rate of (55%), Divorced (36%)

# Plot: Churn Rate By Overtime (Sales Representative)
gg_Target_SalesRep_Overtime <- hr_Sales_SalesRep %>% 
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>% 
  group_by(JobRole, OverTime) %>% 
  summarise(Attrition_Rate = mean(Attrition)) %>% 
  mutate(con = factor(ifelse(Attrition_Rate > hr_Sales_AR, 1,0))) %>% 
  ggplot(aes(OverTime, Attrition_Rate)) +
  geom_col(aes(fill = con), show.legend = FALSE) + geom_hline(yintercept = hr_Sales_AR, linetype = 5, color = "red") +
  geom_label(aes(label = round(Attrition_Rate, 2)), size = 3) +
  facet_wrap(~ JobRole) +
  labs(
    title = "By Overtime"
  ) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_fill_manual(values = c("#7570B3"))
# - visual
gg_Target_SalesRep_Overtime
# - note: sales reps that work overtime have a Churn Rate of (67%)

# Plot: Churn Rate By Stock Option Level (Sales Representative)
gg_Target_SalesRep_StockOptionLevel <- hr_Sales_SalesRep %>%
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>% 
  group_by(JobRole, StockOptionLevel) %>% 
  summarise(Attrition_Rate = mean(Attrition)) %>% 
  mutate(con = factor(ifelse(Attrition_Rate > hr_Sales_AR, 1,0))) %>% 
  ggplot(aes(StockOptionLevel, Attrition_Rate)) +
  geom_col(aes(fill = con), show.legend = FALSE) + geom_hline(yintercept = hr_Sales_AR, linetype = 5, color = "red") +
  geom_label(aes(label = round(Attrition_Rate, 2)), size = 3) +
  facet_wrap(~ JobRole) +
  labs(
    title = "By Stock Option Level"
  ) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_fill_manual(values = c("grey80","#7570B3"))
# - visual
gg_Target_SalesRep_StockOptionLevel
# - note: Sales reps with a StockOptionLevel of 3 have a Churn Rate of (67%), 0 (52%)

# Plot: Correlation Matrix (Sales Representative)
pmat <- hr_Sales_SalesRep %>% 
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>% 
  select_if(is.numeric) %>% select(Attrition, everything(), -EmployeeCount, -StandardHours) %>% 
  ggcorrplot::cor_pmat()

gg_SalesRep_Corr <- hr_Sales_SalesRep %>% 
  mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) %>%
  select_if(is.numeric) %>% select(Attrition, everything(), -EmployeeCount, -StandardHours) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(method = "square", type = "lower",
                         p.mat = pmat, insig = "blank", 
                         lab = TRUE, lab_col = "grey15",lab_size = 4,
                         colors = c("tomato","white","steelblue4"), ggtheme = theme_minimal) +
  labs(
    title = "Sales Representative",
    subtitle = "Department: Sales"
  ) +
  theme(plot.title = element_text(color = "red", face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(color = "blue", face = "bold", hjust = 0.5))
# - visual
gg_SalesRep_Corr
# - note: signifcant features: Age | MonthlyIncome | YearsAtCompany | YearsInCurrentRole | YearsSinceLastPromotion. (YearsAtCompany, YearsInCurrentRole, and YearsSinceLastPromotion are all correlated with each other)

# Plot: Churn Rate By Age (Sales Representative)
gg_Target_SalesRep_Age <- hr_Sales_SalesRep %>% 
  ggplot(aes(Attrition, Age)) +
  geom_boxplot(aes(fill = Attrition), show.legend = FALSE) + geom_jitter(alpha = 0.2) +
  facet_wrap(~ JobRole) +
  labs(
    title = "By Age"
  ) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_fill_manual(values = c("gray88", "#7570B3"))
# - visual
gg_Target_SalesRep_Age
# - note: Sales reps that Churn tend to be younger

# Plot: Churn Rate By Monthly Income
gg_Target_SalesRep_MonthlyIncome <- hr_Sales_SalesRep %>% 
  ggplot(aes(Attrition, MonthlyIncome)) +
  geom_boxplot(aes(fill = Attrition), show.legend = FALSE) + geom_jitter(alpha = 0.2) +
  facet_wrap(~ JobRole) +
  labs(
    title = "By Monthly Income"
  ) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_fill_manual(values = c("gray88", "#7570B3"))
# - visual
gg_Target_SalesRep_MonthlyIncome
# - note: Sales reps that Churn tend to have a lower Monthly Income

# Plot: Churn Rate By Years At Company
gg_Target_SalesRep_YearsAtCompany <- hr_Sales_SalesRep %>% 
  ggplot(aes(Attrition, YearsAtCompany)) +
  geom_boxplot(aes(fill = Attrition), show.legend = FALSE) + geom_jitter(alpha = 0.2) +
  facet_wrap(~ JobRole) +
  labs(
    title = "By Years At Company"
  ) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_fill_manual(values = c("gray88", "#7570B3"))
# - visual
gg_Target_SalesRep_YearsAtCompany
# - note: nothing that stands out

# Churn Rate By Years in Current Role
gg_Target_SalesRep_YearsInCurrentRole <- hr_Sales_SalesRep %>% 
  ggplot(aes(Attrition, YearsInCurrentRole)) +
  geom_boxplot(aes(fill = Attrition), show.legend = FALSE) + geom_jitter(alpha = 0.2) +
  facet_wrap(~ JobRole) +
  labs(
    title = "By Years In Current Role"
  ) +
  theme_bw() + theme(
    plot.title = element_text(color = "red", face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_fill_manual(values = c("gray88","#7570B3"))
# visuals
gg_Target_SalesRep_YearsInCurrentRole
# note: Sales Rep that Churn tend to be in their current role for a lesser amount of years

# NOTE: skipped the EDA for the HR and R&D Department, After modeling we will come back and see if we find anything interesting





## Modeling: Preprocess ----

set.seed(0923)
# Split: 60/40
index_train <- createDataPartition(y = hr$Attrition, p = 0.6, list = FALSE)
# Split "Of the 40": split 50/50
hr_other <- hr[-index_train,]
index_test <- createDataPartition(y = hr_other$Attrition, p = 0.5, list = FALSE)
# Split: Train
split_train <- hr[index_train,] %>% select(-EmployeeCount, -EmployeeNumber, -Over18, -StandardHours) %>% 
  mutate(
    BusinessTravel = case_when(
      BusinessTravel == "Non-Travel" ~ 1,
      BusinessTravel == "Travel_Frequently" ~ 2,
      BusinessTravel == "Travel_Rarely" ~ 3),
    Department = case_when(
      Department == "Human Resources" ~ 1,
      Department == "Research & Development" ~ 2,
      Department == "Sales" ~ 3),
    Education = case_when(
      Education == "Below College" ~ 1,
      Education == "College" ~ 2,
      Education == "Bachelor" ~ 3,
      Education == "Master" ~ 4,
      Education == "Doctor" ~ 5),
    EducationField = case_when(
      EducationField == "Human Resources" ~ 1,
      EducationField == "Life Sciences" ~ 2,
      EducationField == "Marketing" ~ 3,
      EducationField == "Medical" ~ 4,
      EducationField == "Technical Degree" ~ 5,
      EducationField == "Other" ~ 6),
    EnvironmentSatisfaction = case_when(
      EnvironmentSatisfaction == "Low" ~ 1,
      EnvironmentSatisfaction == "Medium" ~ 2,
      EnvironmentSatisfaction == "High" ~ 3,
      EnvironmentSatisfaction == "Very High" ~ 4),
    Gender = case_when(
      Gender == "Female" ~ 1,
      Gender == "Male" ~ 2),
    JobInvolvement = case_when(
      JobInvolvement == "Low" ~ 1,
      JobInvolvement == "Medium" ~ 2,
      JobInvolvement == "High" ~ 3,
      JobInvolvement == "Very High" ~ 4),
    JobRole = case_when(
      JobRole == "Healthcare Representative" ~ 1,
      JobRole == "Human Resources" ~ 2,
      JobRole == "Laboratory Technician" ~ 3,
      JobRole == "Manager" ~ 4,
      JobRole == "Manufacturing Director" ~ 5,
      JobRole == "Research Director" ~ 6,
      JobRole == "Research Scientist" ~ 7,
      JobRole == "Sales Executive" ~ 8,
      JobRole == "Sales Representative" ~ 9),
    JobSatisfaction = case_when(
      JobSatisfaction == "Low" ~ 1,
      JobSatisfaction == "Medium" ~ 2,
      JobSatisfaction == "High" ~ 3,
      JobSatisfaction == "Very High" ~ 4),
    MaritalStatus = case_when(
      MaritalStatus == "Divorced" ~ 1,
      MaritalStatus == "Married" ~ 2,
      MaritalStatus == "Single" ~ 3),
    OverTime = case_when(
      OverTime == "No" ~ 1,
      OverTime == "Yes" ~ 2),
    PerformanceRating = case_when(
      PerformanceRating == "Low" ~ 1,
      PerformanceRating == "Good" ~ 2,
      PerformanceRating == "Excellent" ~ 3,
      PerformanceRating == "Outstanding" ~ 4),
    RelationshipSatisfaction = case_when(
      RelationshipSatisfaction == "Low" ~ 1,
      RelationshipSatisfaction == "Medium" ~ 2,
      RelationshipSatisfaction == "High" ~ 3,
      RelationshipSatisfaction == "Very High" ~ 4),
    WorkLifeBalance = case_when(
      WorkLifeBalance == "Bad" ~ 1,
      WorkLifeBalance == "Good" ~ 2,
      WorkLifeBalance == "Better" ~ 3,
      WorkLifeBalance == "Best" ~ 4)
  ) %>% 
  mutate(
    BusinessTravel = factor(BusinessTravel, levels = c(1,2,3), labels = c("Non-Travel","Travel_Frequently","Travel_Rarely")),
    Department = factor(Department, levels = c(1,2,3), labels = c("Human_Resources","Research_Development","Sales")),
    Education = factor(Education, levels = c(1,2,3,4,5), labels = c("Below_College","College","Bachelor","Master","Doctor")),
    EducationField = factor(EducationField, levels = c(1,2,3,4,5,6), 
                            labels = c("Human_Resources","Life_Sciences","Marketing","Medical","Technical_Degree","Other")),
    EnvironmentSatisfaction = factor(EnvironmentSatisfaction, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    Gender = factor(Gender, levels = c(1,2), labels = c("Female", "Male")),
    JobInvolvement = factor(JobInvolvement, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    JobRole = factor(JobRole, levels = c(1,2,3,4,5,6,7,8,9), 
                     labels = c("Healthcare_Representative","Human_Resources","Laboratory_Technician","Manager","Manufacturing_Director",
                                "Research_Director","Research_Scientist","Sales_Executive", "Sales_Representative")),
    JobSatisfaction = factor(JobSatisfaction, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    MaritalStatus = factor(MaritalStatus, levels = c(1,2,3), labels = c("Divorced","Married","Single")),
    OverTime = factor(OverTime, levels = c(1,2), labels = c("No", "Yes")),
    PerformanceRating = factor(PerformanceRating, levels = c(1,2,3,4), labels = c("Low","Good","Excellent","Outstanding")),
    RelationshipSatisfaction = factor(RelationshipSatisfaction, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    WorkLifeBalance = factor(WorkLifeBalance, levels = c(1,2,3,4), labels = c("Bad","Good","Better","Best"))
  )
# Split: Validation
split_val <- hr_other[-index_test,] %>% select(-EmployeeCount, -EmployeeNumber, -Over18, -StandardHours) %>% 
  mutate(
    BusinessTravel = case_when(
      BusinessTravel == "Non-Travel" ~ 1,
      BusinessTravel == "Travel_Frequently" ~ 2,
      BusinessTravel == "Travel_Rarely" ~ 3),
    Department = case_when(
      Department == "Human Resources" ~ 1,
      Department == "Research & Development" ~ 2,
      Department == "Sales" ~ 3),
    Education = case_when(
      Education == "Below College" ~ 1,
      Education == "College" ~ 2,
      Education == "Bachelor" ~ 3,
      Education == "Master" ~ 4,
      Education == "Doctor" ~ 5),
    EducationField = case_when(
      EducationField == "Human Resources" ~ 1,
      EducationField == "Life Sciences" ~ 2,
      EducationField == "Marketing" ~ 3,
      EducationField == "Medical" ~ 4,
      EducationField == "Technical Degree" ~ 5,
      EducationField == "Other" ~ 6),
    EnvironmentSatisfaction = case_when(
      EnvironmentSatisfaction == "Low" ~ 1,
      EnvironmentSatisfaction == "Medium" ~ 2,
      EnvironmentSatisfaction == "High" ~ 3,
      EnvironmentSatisfaction == "Very High" ~ 4),
    Gender = case_when(
      Gender == "Female" ~ 1,
      Gender == "Male" ~ 2),
    JobInvolvement = case_when(
      JobInvolvement == "Low" ~ 1,
      JobInvolvement == "Medium" ~ 2,
      JobInvolvement == "High" ~ 3,
      JobInvolvement == "Very High" ~ 4),
    JobRole = case_when(
      JobRole == "Healthcare Representative" ~ 1,
      JobRole == "Human Resources" ~ 2,
      JobRole == "Laboratory Technician" ~ 3,
      JobRole == "Manager" ~ 4,
      JobRole == "Manufacturing Director" ~ 5,
      JobRole == "Research Director" ~ 6,
      JobRole == "Research Scientist" ~ 7,
      JobRole == "Sales Executive" ~ 8,
      JobRole == "Sales Representative" ~ 9),
    JobSatisfaction = case_when(
      JobSatisfaction == "Low" ~ 1,
      JobSatisfaction == "Medium" ~ 2,
      JobSatisfaction == "High" ~ 3,
      JobSatisfaction == "Very High" ~ 4),
    MaritalStatus = case_when(
      MaritalStatus == "Divorced" ~ 1,
      MaritalStatus == "Married" ~ 2,
      MaritalStatus == "Single" ~ 3),
    OverTime = case_when(
      OverTime == "No" ~ 1,
      OverTime == "Yes" ~ 2),
    PerformanceRating = case_when(
      PerformanceRating == "Low" ~ 1,
      PerformanceRating == "Good" ~ 2,
      PerformanceRating == "Excellent" ~ 3,
      PerformanceRating == "Outstanding" ~ 4),
    RelationshipSatisfaction = case_when(
      RelationshipSatisfaction == "Low" ~ 1,
      RelationshipSatisfaction == "Medium" ~ 2,
      RelationshipSatisfaction == "High" ~ 3,
      RelationshipSatisfaction == "Very High" ~ 4),
    WorkLifeBalance = case_when(
      WorkLifeBalance == "Bad" ~ 1,
      WorkLifeBalance == "Good" ~ 2,
      WorkLifeBalance == "Better" ~ 3,
      WorkLifeBalance == "Best" ~ 4)
  ) %>% 
  mutate(
    BusinessTravel = factor(BusinessTravel, levels = c(1,2,3), labels = c("Non-Travel","Travel_Frequently","Travel_Rarely")),
    Department = factor(Department, levels = c(1,2,3), labels = c("Human_Resources","Research_Development","Sales")),
    Education = factor(Education, levels = c(1,2,3,4,5), labels = c("Below_College","College","Bachelor","Master","Doctor")),
    EducationField = factor(EducationField, levels = c(1,2,3,4,5,6), 
                            labels = c("Human_Resources","Life_Sciences","Marketing","Medical","Technical_Degree","Other")),
    EnvironmentSatisfaction = factor(EnvironmentSatisfaction, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    Gender = factor(Gender, levels = c(1,2), labels = c("Female", "Male")),
    JobInvolvement = factor(JobInvolvement, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    JobRole = factor(JobRole, levels = c(1,2,3,4,5,6,7,8,9), 
                     labels = c("Healthcare_Representative","Human_Resources","Laboratory_Technician","Manager","Manufacturing_Director",
                                "Research_Director","Research_Scientist","Sales_Executive", "Sales_Representative")),
    JobSatisfaction = factor(JobSatisfaction, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    MaritalStatus = factor(MaritalStatus, levels = c(1,2,3), labels = c("Divorced","Married","Single")),
    OverTime = factor(OverTime, levels = c(1,2), labels = c("No", "Yes")),
    PerformanceRating = factor(PerformanceRating, levels = c(1,2,3,4), labels = c("Low","Good","Excellent","Outstanding")),
    RelationshipSatisfaction = factor(RelationshipSatisfaction, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    WorkLifeBalance = factor(WorkLifeBalance, levels = c(1,2,3,4), labels = c("Bad","Good","Better","Best"))
  )
# Split: Test
split_test <- hr_other[index_test,] %>% select(-EmployeeCount, -EmployeeNumber, -Over18, -StandardHours) %>% 
  mutate(
    BusinessTravel = case_when(
      BusinessTravel == "Non-Travel" ~ 1,
      BusinessTravel == "Travel_Frequently" ~ 2,
      BusinessTravel == "Travel_Rarely" ~ 3),
    Department = case_when(
      Department == "Human Resources" ~ 1,
      Department == "Research & Development" ~ 2,
      Department == "Sales" ~ 3),
    Education = case_when(
      Education == "Below College" ~ 1,
      Education == "College" ~ 2,
      Education == "Bachelor" ~ 3,
      Education == "Master" ~ 4,
      Education == "Doctor" ~ 5),
    EducationField = case_when(
      EducationField == "Human Resources" ~ 1,
      EducationField == "Life Sciences" ~ 2,
      EducationField == "Marketing" ~ 3,
      EducationField == "Medical" ~ 4,
      EducationField == "Technical Degree" ~ 5,
      EducationField == "Other" ~ 6),
    EnvironmentSatisfaction = case_when(
      EnvironmentSatisfaction == "Low" ~ 1,
      EnvironmentSatisfaction == "Medium" ~ 2,
      EnvironmentSatisfaction == "High" ~ 3,
      EnvironmentSatisfaction == "Very High" ~ 4),
    Gender = case_when(
      Gender == "Female" ~ 1,
      Gender == "Male" ~ 2),
    JobInvolvement = case_when(
      JobInvolvement == "Low" ~ 1,
      JobInvolvement == "Medium" ~ 2,
      JobInvolvement == "High" ~ 3,
      JobInvolvement == "Very High" ~ 4),
    JobRole = case_when(
      JobRole == "Healthcare Representative" ~ 1,
      JobRole == "Human Resources" ~ 2,
      JobRole == "Laboratory Technician" ~ 3,
      JobRole == "Manager" ~ 4,
      JobRole == "Manufacturing Director" ~ 5,
      JobRole == "Research Director" ~ 6,
      JobRole == "Research Scientist" ~ 7,
      JobRole == "Sales Executive" ~ 8,
      JobRole == "Sales Representative" ~ 9),
    JobSatisfaction = case_when(
      JobSatisfaction == "Low" ~ 1,
      JobSatisfaction == "Medium" ~ 2,
      JobSatisfaction == "High" ~ 3,
      JobSatisfaction == "Very High" ~ 4),
    MaritalStatus = case_when(
      MaritalStatus == "Divorced" ~ 1,
      MaritalStatus == "Married" ~ 2,
      MaritalStatus == "Single" ~ 3),
    OverTime = case_when(
      OverTime == "No" ~ 1,
      OverTime == "Yes" ~ 2),
    PerformanceRating = case_when(
      PerformanceRating == "Low" ~ 1,
      PerformanceRating == "Good" ~ 2,
      PerformanceRating == "Excellent" ~ 3,
      PerformanceRating == "Outstanding" ~ 4),
    RelationshipSatisfaction = case_when(
      RelationshipSatisfaction == "Low" ~ 1,
      RelationshipSatisfaction == "Medium" ~ 2,
      RelationshipSatisfaction == "High" ~ 3,
      RelationshipSatisfaction == "Very High" ~ 4),
    WorkLifeBalance = case_when(
      WorkLifeBalance == "Bad" ~ 1,
      WorkLifeBalance == "Good" ~ 2,
      WorkLifeBalance == "Better" ~ 3,
      WorkLifeBalance == "Best" ~ 4)
  ) %>% 
  mutate(
    BusinessTravel = factor(BusinessTravel, levels = c(1,2,3), labels = c("Non-Travel","Travel_Frequently","Travel_Rarely")),
    Department = factor(Department, levels = c(1,2,3), labels = c("Human_Resources","Research_Development","Sales")),
    Education = factor(Education, levels = c(1,2,3,4,5), labels = c("Below_College","College","Bachelor","Master","Doctor")),
    EducationField = factor(EducationField, levels = c(1,2,3,4,5,6), 
                            labels = c("Human_Resources","Life_Sciences","Marketing","Medical","Technical_Degree","Other")),
    EnvironmentSatisfaction = factor(EnvironmentSatisfaction, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    Gender = factor(Gender, levels = c(1,2), labels = c("Female", "Male")),
    JobInvolvement = factor(JobInvolvement, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    JobRole = factor(JobRole, levels = c(1,2,3,4,5,6,7,8,9), 
                     labels = c("Healthcare_Representative","Human_Resources","Laboratory_Technician","Manager","Manufacturing_Director",
                                "Research_Director","Research_Scientist","Sales_Executive", "Sales_Representative")),
    JobSatisfaction = factor(JobSatisfaction, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    MaritalStatus = factor(MaritalStatus, levels = c(1,2,3), labels = c("Divorced","Married","Single")),
    OverTime = factor(OverTime, levels = c(1,2), labels = c("No", "Yes")),
    PerformanceRating = factor(PerformanceRating, levels = c(1,2,3,4), labels = c("Low","Good","Excellent","Outstanding")),
    RelationshipSatisfaction = factor(RelationshipSatisfaction, levels = c(1,2,3,4), labels = c("Low","Medium","High","Very_High")),
    WorkLifeBalance = factor(WorkLifeBalance, levels = c(1,2,3,4), labels = c("Bad","Good","Better","Best"))
  )


# Store all popular metrics used for Classification: [ ROC | Sens | Spec | Accuracy | Kappa | AUC | Precision | Recall ]
# - We will be focusing on RECALL: "Of all the people that actually Churn, What % did the model predict as Churn"
full_Stats <- function(...) {
  
  c(twoClassSummary(...), defaultSummary(...), prSummary(...))
  
}
# Control: 10-Fold Cross-Validation | Parallel Computing
ctrl_Parallel <- trainControl(method = "cv", number = 10,
                              summaryFunction = full_Stats,
                              classProbs = TRUE,
                              verboseIter = TRUE,
                              allowParallel = TRUE)

# Create seeds for each model
# - Decision Trees
set.seed(0923)
seed_DT <- vector(mode = "list", length = 11)
for(i in 1:10) {
  seed_DT[[i]] <- sample.int(n = 1000, 1)
}
seed_DT[[11]] <- sample.int(1000,1)

# - Random Forrest
set.seed(0923)
seed_RF <- vector(mode = "list", length = 11)
for (i in 1:10) {
  seed_RF[[i]] <- sample.int(n = 1000,40)
}
seed_RF[[11]] <- sample.int(1000,1)

# - XGB
set.seed(0923)
seed_XGB <- vector(mode = "list", length = 11)
for (i in 1:10) {
  seed_XGB[[i]] <- sample.int(1000, 100)
}
seed_XGB[[11]] <- sample.int(1000,1)

# - SVM
set.seed(0923)
seed_SVM <- vector(mode = "list", length = 11)
for(i in 1:10) {
  seed_SVM[[i]] <- sample.int(n = 1000, 42)
}
seed_SVM[[11]] <- sample.int(1000,1)

# - Logistic Regression
set.seed(0923)
seed_LOG <- vector(mode = "list", length = 11)
for(i in 1:10) {
  seed_LOG[[i]] <- sample.int(n = 1000, 1)
}
seed_LOG[[11]] <- sample.int(1000,1)

# - Regularized Regression
set.seed(0923)
seed_ELAS <- vector(mode = "list", length = 11)
for (i in 1:10) {
  seed_ELAS[[i]] <- sample.int(n = 1000, 15)
}
seed_ELAS[[11]] <- sample.int(1000,1)

# - Final Seed
set.seed(0923)
seed_FINAL <- vector(mode = "list", length = 11)
for (i in 1:10) {
  seed_FINAL[[i]] <- sample.int(n = 1000,1)
}
seed_FINAL[[11]] <- sample.int(1000,1)



## Modeling: Baseline Fit ----

# Create and Register Clusters
cl_3 <- makeCluster(3)
registerDoParallel(cl_3)

# DECISION TREE
# - set seed
ctrl_Parallel$seeds <- seed_DT
# - model
mod_DT = train(Attrition ~ ., data = split_train,
               method = "rpart",
               tuneLength = 10,
               trControl = ctrl_Parallel)
# - plot: hyperparameters
gg_mod_DT <- mod_DT$results %>% 
  ggplot(aes(x = cp, y = Recall)) +
  geom_line(color = "blue") + geom_point(color = "blue") +
  theme_bw() +
  labs(
    title = "Decision Tree"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold')
  )
# - visual
gg_mod_DT
# - store best hyperparameters in terms of recall: 
# -- Cost Complexity = 0.06
df_DT_tune <- data.frame(
  cp = mod_DT$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(cp)
) 
# - set final seed
ctrl_Parallel$seeds <- seed_FINAL
# - final model
mod_DT_FINAL <- train(Attrition ~ ., data = split_train,
                      method = "rpart",
                      tuneGrid = df_DT_tune,
                      trControl = ctrl_Parallel)


# RANDOM FORREST
# - set seed
ctrl_Parallel$seeds <- seed_RF
# - model
mod_RF = train(Attrition ~ ., data = split_train,
               method = "ranger", 
               tuneLength = 20,
               trControl = ctrl_Parallel)
# - plot: hyperparameters
gg_mod_RF <- mod_RF$results %>% 
  ggplot(aes(x = mtry, y = Recall, color = splitrule)) +
  geom_line() + geom_point() +
  theme() + theme_bw() +
  labs(
    title = "Random Forrest",
    x = "Number of Features"
  ) + theme(
    legend.position = "bottom"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold')
  )
# - visual
gg_mod_RF
# - store best hyperparameters in terms of recall: 
# -- # Of Randomly Selected Features = 2
# -- Minimal Node Size = 1
# -- Splitrule = gini
df_RF_tune <- data.frame(
  mtry = mod_RF$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(mtry),
  min.node.size = mod_RF$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(min.node.size),
  splitrule = mod_RF$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(splitrule)
)
# - set final seed
ctrl_Parallel$seeds <- seed_FINAL
# - final model
mod_RF_FINAL <- train(Attrition ~ ., data = split_train,
                      method = "ranger",
                      tuneGrid = df_RF_tune,
                      trControl = ctrl_Parallel)


# XGB
# - set seed
ctrl_Parallel$seeds <- seed_XGB
# - model
mod_XGB = train(Attrition ~ ., data = split_train,
                method = "xgbTree",
                tuneLength = 5,
                trControl = ctrl_Parallel)
# - plot Hyperparameters
gg_mod_XGB <- mod_XGB$results %>%
  mutate(eta = factor(eta), gamma = factor(gamma), max_depth = factor(max_depth), nTrees = nrounds) %>% 
  ggplot(aes(x = nTrees, y = Recall, color = eta)) +
  geom_line() + geom_point() +
  facet_wrap(~ max_depth, nrow = 1) +
  labs(
    title = "eXtreme Gradient Boosting",
    x = "# Boosting Iterations"
  ) + 
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    legend.position = "bottom"
  )
# - visual
gg_mod_XGB
# - store best hyperparameters in terms of recall: 
# -- eta "Shrinkage" = 0.3
# -- Max Tree Depth = 1
# -- gamma "Minimum Loss Reduction" = 0
# -- colsample_bytree "Subsample Ratio of Columns" = 0.6
# -- min_child_weight "Minimum Sum of Instance Weight" = 1
# -- subsample = 1
# -- nrounds "# Boosting Iterations" = 50
df_XGB_tune <- data.frame(
  eta = mod_XGB$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(eta),
  max_depth = mod_XGB$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(max_depth),
  gamma = mod_XGB$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(gamma),
  colsample_bytree = mod_XGB$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(colsample_bytree),
  min_child_weight = mod_XGB$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(min_child_weight),
  subsample = mod_XGB$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(subsample),
  nrounds = mod_XGB$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(nrounds)
)
# - set final seed
ctrl_Parallel$seeds <- seed_FINAL
# - final model
mod_XGB_FINAL <- train(Attrition ~ ., data = split_train,
                       method = "xgbTree", 
                       tuneGrid = df_XGB_tune,
                       trControl = ctrl_Parallel)


# SVM
# - set seed
ctrl_Parallel$seeds <- seed_SVM
# - model
mod_SVM = train(Attrition ~ ., data = split_train,
                method = "svmRadialSigma",
                preProcess = c("center", "scale"),
                tuneLength = 7,
                trControl = ctrl_Parallel)
# - plot hyperparameters
gg_mod_SVM <- mod_SVM$results %>% mutate(Cost = factor(C)) %>% 
  ggplot(aes(x = sigma, y = Recall, color = Cost)) +
  geom_point() + geom_line() +
  theme_bw() +
  labs(
    title = "Support Vector Machine"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    legend.position = "bottom"
  )
# - visual
gg_mod_SVM
# - store best hyperparameters in terms of recall: 
# -- sigma = 0.006
# -- cost = 16
df_SVM_tune <- data.frame(
  sigma = mod_SVM$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(sigma),
  C = mod_SVM$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(C)
)
# - set final seed
ctrl_Parallel$seeds <- seed_FINAL
# - final model
mod_SVM_FINAL <- train(Attrition ~ ., data = split_train,
                       method = "svmRadialSigma", 
                       preProcess = c("center", "scale"),
                       tuneGrid = df_SVM_tune,
                       trControl = ctrl_Parallel)


# LOGISTIC REGRESSION
# - set seed
ctrl_Parallel$seeds <- seed_LOG
# - final model
mod_LOG_FINAL = train(Attrition ~ ., data = split_train,
                      method = "glm",
                      preProcess = c("zv","nzv","center","scale"),
                      trControl = ctrl_Parallel)


# REGULARIZED REGRESSION
# - set seed
ctrl_Parallel$seeds <- seed_ELAS
# - model
mod_ELAS = train(Attrition ~ ., data = split_train,
                 method = "glmnet", 
                 preProcess = c("zv","nzv","center","scale"),
                 tuneLength = 15,
                 trControl = ctrl_Parallel)
# - plot hyperparameters
gg_mod_ELAS <- mod_ELAS$results %>% mutate(alpha = case_when(
  alpha >= 0   & alpha < 0.2 ~ "[0 - 0.2)",
  alpha >= 0.2 & alpha < 0.4 ~ "[0.2 - 0.4)",
  alpha >= 0.4 & alpha < 0.6 ~ "[0.4 - 0.6)",
  alpha >= 0.6 & alpha < 0.8 ~ "[0.6 - 0.8)",
  alpha >= 0.8 & alpha <= 1  ~ "[0.8 - 1]"
) %>% factor) %>% 
  ggplot(aes(x = lambda, y = Recall, color = alpha)) +
  geom_line() + geom_point() +
  labs(
    title = "Regularized Regression"
  ) + 
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    legend.position = "bottom",
    legend.text = element_text(size = 5)
  )
# - visual
gg_mod_ELAS
# - store best hyperparameters in terms of recall: 
# -- alpha = 0.1
# -- lambda = 0.11
df_ELAS_tune <- data.frame(
  alpha = mod_ELAS$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(alpha),
  lambda = mod_ELAS$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(lambda)
)
# - set final seed
ctrl_Parallel$seeds <- seed_FINAL
# - final model
mod_ELAS_FINAL <- train(Attrition ~ ., data = split_train,
                        method = "glmnet",
                        preProcess = c("zv","nzv","center","scale"),
                        tuneGrid = df_ELAS_tune,
                        trControl = ctrl_Parallel)

# Stop Cluster
stopCluster(cl_3)


## Modeling: Baseline Diagnostics ----

# EXPLAINER
# In order to use the DALEX package the target value datatype must be adjusted
split_val_Numeric <- split_val %>% mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) 

# Create an Explainer for all Models. This will be used to perform Model Diagnostics
set.seed(0923)
exp_DT <- explain(model = mod_DT_FINAL,
                  data = split_val_Numeric, y = split_val_Numeric$Attrition,
                  type = "classification",
                  label = "Decision Tree")
set.seed(0923)
exp_RF <- explain(model = mod_RF_FINAL,
                  data = split_val_Numeric, y = split_val_Numeric$Attrition,
                  type = "classification",
                  label = "Random Forrest")
set.seed(0923)
exp_XGB <- explain(model = mod_XGB_FINAL,
                   data = split_val_Numeric, y = split_val_Numeric$Attrition,
                   type = "classification",
                   label = "XGB")
set.seed(0923)
exp_SVM <- explain(model = mod_SVM_FINAL,
                   data = split_val_Numeric, y = split_val_Numeric$Attrition,
                   type = "classification",
                   label = "SVM")
set.seed(0923)
exp_LOG <- explain(model = mod_LOG_FINAL,
                   data = split_val_Numeric, y = split_val_Numeric$Attrition,
                   type = "classification",
                   label = "Logistic Regression")
set.seed(0923)
exp_ELAS <- explain(model = mod_ELAS_FINAL,
                    data = split_val_Numeric, y = split_val_Numeric$Attrition,
                    type = "classification",
                    label = "Regularized Regression")

# Model Diagnostics:
# - list
perf_Baseline <- list(DT = model_performance(exp_DT),
                      RF = model_performance(exp_RF),
                      XGB = model_performance(exp_XGB),
                      SVM = model_performance(exp_SVM),
                      LOG = model_performance(exp_LOG),
                      ELAS = model_performance(exp_ELAS))
# - extract: AUC | Accuracy | Precision | Recall | F1
df_perf_Baseline <- perf_Baseline %>% 
  map_df(~ data.frame(
    AUC = .x$measures$auc %>% round(2),
    Accuracy = .x$measures$accuracy %>% round(2),
    Precision = .x$measures$precision %>% round(2),
    Recall = .x$measures$recall %>% round(2),
    F1 = .x$measures$f1 %>% round(2))
  ) %>% 
  mutate(Name = names(perf_Baseline)) %>% 
  select(Name, everything())
# - dataframe
df_perf_Baseline %>% arrange(desc(Recall))
# - note: Best "Baseline" Model in terms of Recall "Logistic Regression (Recall = 53%)"

# Model Diagnostics: Optimal Thresholds
# - calculate optimal thresholds in terms of best "F1" score
thres_Cut_Baseline <- data.frame(
  DT = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                     predictedScores = perf_Baseline$DT$residuals$predicted, 
                     optimiseFor = "Both") %>% round(2),
  RF = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                     predictedScores = perf_Baseline$RF$residuals$predicted, 
                     optimiseFor = "Both") %>% round(2),
  XGB = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Baseline$XGB$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  SVM = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Baseline$SVM$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  LOG = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Baseline$LOG$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  ELAS = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                       predictedScores = perf_Baseline$ELAS$residuals$predicted, 
                       optimiseFor = "Both") %>% round(2)
)
# Plot: ROC Curve + Optimal Threshold
gg_roc_Baseline <- plot(perf_Baseline$DT, perf_Baseline$RF, perf_Baseline$XGB, perf_Baseline$SVM, perf_Baseline$LOG, perf_Baseline$ELAS,
     geom = "roc") + 
  labs(
    title = "ROC: Baseline",
    subtitle = "Optimal Threshold (F1)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = 'blue4', face = 'bold'), 
    plot.subtitle = element_text(hjust = 0.5, size = 15, color = 'lightblue')
  ) +
  # Optimal Threshold: Logistic Regression
  geom_label(
    label=paste0("Logistic Regression: ",thres_Cut_Baseline$DT), 
    x=0.68,
    y=0.40,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: XGB
  geom_label(
    label=paste0("XGB: ",thres_Cut_Baseline$XGB), 
    x=0.68,
    y=0.33,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Random Forrest: ",thres_Cut_Baseline$RF), 
    x=0.68,
    y=0.26,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: SVM
  geom_label(
    label=paste0("SVM: ",thres_Cut_Baseline$SVM), 
    x=0.68,
    y=0.19,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Regularized Regression: ",thres_Cut_Baseline$ELAS), 
    x=0.68,
    y=0.12,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Decision Tree: ",thres_Cut_Baseline$DT), 
    x=0.68,
    y=0.05,
    label.size = 0.1,
    color = "black",
    fill="lightblue"
  )
# - visual
gg_roc_Baseline
# - list (optimal thershold)
perf_Baseline_Thres <- list(DT_Thres.16 = model_performance(exp_DT, cutoff = thres_Cut_Baseline$DT),
                            RF_Thres.21 = model_performance(exp_RF, cutoff = thres_Cut_Baseline$RF),
                            XGB_Thres.21 = model_performance(exp_XGB, cutoff = thres_Cut_Baseline$XGB),
                            SVM_Thres.13 = model_performance(exp_SVM, cutoff = thres_Cut_Baseline$SVM),
                            LOG_Thres.16 = model_performance(exp_LOG, cutoff = thres_Cut_Baseline$LOG),
                            ELAS_Thres.21 = model_performance(exp_ELAS, cutoff = thres_Cut_Baseline$ELAS)
)
# - extract: AUC | Accuracy | Precision | Recall | F1 (optimal thershold)
df_perf_Baseline_Thres <- perf_Baseline_Thres %>% 
  map_df(~ data.frame(
    AUC = .x$measures$auc %>% round(2),
    Accuracy = .x$measures$accuracy %>% round(2),
    Precision = .x$measures$precision %>% round(2),
    Recall = .x$measures$recall %>% round(2),
    F1 = .x$measures$f1 %>% round(2))
  ) %>% 
  mutate(Name = names(perf_Baseline_Thres)) %>% 
  select(Name, everything()) 
# - dataframe
df_perf_Baseline_Thres %>% arrange(desc(Recall))
# - note: Best "Baseline + Threshold" Model in terms of Recall "Logistic Regression.Thres.16% (Recall = 77%)"





## Modeling: Undersample Fit ----

# Change ctrl for Undersampling
ctrl_Parallel_Down <- ctrl_Parallel
ctrl_Parallel_Down$sampling <- "down"

# Register 3 Clusters
cl_3 <- makeCluster(3)
registerDoParallel(cl_3)

# DECISION TREE: Undersample
# - set seed
ctrl_Parallel_Down$seeds <- seed_DT
# - model
mod_DT_down <- train(Attrition ~ .,data = split_train,
                     method = "rpart", metric = "ROC",
                     tuneLength = 10,
                     trControl = ctrl_Parallel_Down)
# - plot: hyperparameters
gg_mod_DT_down <- mod_DT_down$results %>% 
  ggplot(aes(x = cp, y = Recall)) +
  geom_line(color = "blue") + geom_point(color = "blue") +
  theme_bw() +
  labs(
    title = "Decision Tree",
    subtitle = "Undersample"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue')
  )
# - visual
gg_mod_DT_down
# - store best hyperparameters in terms of recall: 
# -- Cost Complexity = 0.02
df_DT_down_tune <- data.frame(
  cp = mod_DT_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(cp)
)
# - set final seed
ctrl_Parallel_Down$seeds <- seed_FINAL
# - final model
mod_DT_down_FINAL <- train(Attrition ~ ., data = split_train,
                           method = "rpart",
                           tuneGrid = df_DT_down_tune,
                           trControl = ctrl_Parallel_Down)


# RANDOM FORREST: Undersample 
# - set seed
ctrl_Parallel_Down$seeds <- seed_RF
# - model
mod_RF_down <- train(Attrition ~ ., data = split_train,
                     method = "ranger",
                     tuneLength = 20,
                     trControl = ctrl_Parallel_Down)
# - plot: hyperparameters
gg_mod_RF_down <- mod_RF_down$results %>% 
  ggplot(aes(x = mtry, y = Recall, color = splitrule)) +
  geom_line() + geom_point() +
  theme() + theme_bw() +
  labs(
    title = "Random Forrest",
    subtitle = "Undersample",
    x = "Number of Features"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom"
  )
# - visual
gg_mod_RF_down
# - store best hyperparameters in terms of recall: 
# -- # Of Randomly Selected Features = 41
# -- Minimal Node Size = 1
# -- Splitrule = extratrees
df_RF_down_tune <- data.frame(
  mtry = mod_RF_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(mtry),
  min.node.size = mod_RF_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(min.node.size),
  splitrule = mod_RF_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(splitrule)
)
# - set final seed
ctrl_Parallel_Down$seeds <- seed_FINAL
# - final model
mod_RF_down_FINAL <- train(Attrition ~ ., data = split_train,
                           method = "ranger",
                           tuneGrid = df_RF_down_tune,
                           trControl = ctrl_Parallel_Down)


# XGB: Undersample
# - set seed
ctrl_Parallel_Down$seeds <- seed_XGB
# - model
mod_XGB_down <- train(Attrition ~ .,data = split_train,
                      method = "xgbTree",
                      tuneLength = 5,
                      trControl = ctrl_Parallel_Down)
# - plot: hyperparameters
gg_mod_XGB_down <- mod_XGB_down$results %>%
  mutate(eta = factor(eta), gamma = factor(gamma), max_depth = factor(max_depth)) %>% 
  ggplot(aes(x = nrounds, y = Recall, color = eta)) +
  geom_line() + geom_point() +
  facet_wrap(~ max_depth, nrow = 1) +
  labs(
    title = "eXtreme Gradient Boosting",
    subtitle = "Undersample"
  ) + 
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom"
  )
# - visual
gg_mod_XGB_down
# - store best hyperparameters in terms of recall: 
# -- eta "Shrinkage" = 0.4
# -- Max Tree Depth = 1
# -- gamma "Minimum Loss Reduction" = 0
# -- colsample_bytree "Subsample Ratio of Columns" = 0.8
# -- min_child_weight "Minimum Sum of Instance Weight" = 1
# -- subsample = 0.625
# -- nrounds "# Boosting Iterations" = 50
df_XGB_down_tune <- data.frame(
  eta = mod_XGB_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(eta),
  max_depth = mod_XGB_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(max_depth),
  gamma = mod_XGB_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(gamma),
  colsample_bytree = mod_XGB_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(colsample_bytree),
  min_child_weight = mod_XGB_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(min_child_weight),
  subsample = mod_XGB_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(subsample),
  nrounds = mod_XGB_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(nrounds)
)
# - set final seed
ctrl_Parallel_Down$seeds <- seed_FINAL
# - final model
mod_XGB_down_FINAL <- train(Attrition ~ ., data = split_train,
                            method = "xgbTree",
                            tuneGrid = df_XGB_down_tune,
                            trControl = ctrl_Parallel_Down)


# SVM: Undersample
# - set seed
ctrl_Parallel_Down$seeds <- seed_SVM
# - model
mod_SVM_down <- train(Attrition ~ ., data = split_train,
                      method = "svmRadialSigma", 
                      preProcess = c("center", "scale"),
                      tuneLength = 7,
                      trControl = ctrl_Parallel_Down)
# - plot: hyperparameters
gg_mod_SVM_down <- mod_SVM_down$results %>% mutate(Cost = factor(C)) %>% 
  ggplot(aes(x = sigma, y = Recall, color = Cost)) +
  geom_point() + geom_line() +
  theme_bw() +
  labs(
    title = "Support Vector Machine",
    subtitle = "Undersample"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom"
  )
# - visual
gg_mod_SVM_down
# - store best hyperparameters in terms of recall:
# -- sigma = 0.009
# -- cost = 2
df_SVM_down_tune <- data.frame(
  sigma = mod_SVM_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(sigma),
  C = mod_SVM_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(C)
)
# - set final seed
ctrl_Parallel_Down$seeds <- seed_FINAL
# - final model
mod_SVM_down_FINAL <- train(Attrition ~ ., data = split_train,
                            method = "svmRadialSigma",
                            preProcess = c("center", "scale"),
                            tuneGrid = df_SVM_down_tune,
                            trControl = ctrl_Parallel_Down)


# LOGISTIC REGRESSION: Undersample
# - set seed
ctrl_Parallel_Down$seeds <- seed_LOG
# - final model
mod_LOG_down_FINAL <- train(Attrition ~ ., data = split_train,
                            method = "glm",
                            preProcess = c("zv","nzv","center","scale"),
                            trControl = ctrl_Parallel_Down)

# REGULARIZED REGRESSION: Undersample
# - set seed
ctrl_Parallel_Down$seeds <- seed_ELAS
# - model
mod_ELAS_down <- train(Attrition ~ ., data = split_train,
                       method = "glmnet",
                       preProcess = c("zv","nzv","center","scale"),
                       tuneLength = 15,
                       trControl = ctrl_Parallel_Down)
# - plot: hyperparameters
gg_mod_ELAS_down <- mod_ELAS_down$results %>% mutate(alpha = case_when(
  alpha >= 0   & alpha < 0.2 ~ "[0 - 0.2)",
  alpha >= 0.2 & alpha < 0.4 ~ "[0.2 - 0.4)",
  alpha >= 0.4 & alpha < 0.6 ~ "[0.4 - 0.6)",
  alpha >= 0.6 & alpha < 0.8 ~ "[0.6 - 0.8)",
  alpha >= 0.8 & alpha <= 1  ~ "[0.8 - 1]"
) %>% factor) %>% 
  ggplot(aes(x = lambda, y = Recall, color = alpha)) +
  geom_line() + geom_point() + 
  labs(
    title = "Regularized Regression",
    subtitle = "Undersample"
  ) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom",
    legend.text = element_text(size = 5)
  )
# - visual
gg_mod_ELAS_down
# - store best hyperparameters in terms of recall: 
# -- alpha = 0.16
# -- lambda = 0.01
df_ELAS_down_tune <- data.frame(
  alpha = mod_ELAS_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(alpha),
  lambda = mod_ELAS_down$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(lambda)
)
# - set final seed
ctrl_Parallel_Down$seeds <- seed_FINAL
# - final model
mod_ELAS_down_FINAL <- train(Attrition ~ ., data = split_train,
                             method = "glmnet",
                             preProcess = c("zv","nzv","center","scale"),
                             tuneGrid = df_ELAS_down_tune,
                             trControl = ctrl_Parallel_Down)

# Stop Cluster
stopCluster(cl_3)









## Modeling: Undersample Diagnostics ----

# Create Explainers for the Undersample models
set.seed(0923)
exp_DT_down <- explain(model = mod_DT_down_FINAL,
                       data = split_val_Numeric, y = split_val_Numeric$Attrition,
                       type = "classification",
                       label = "Decision Tree Down")

set.seed(0923)
exp_RF_down <- explain(model = mod_RF_down_FINAL,
                       data = split_val_Numeric, y =  split_val_Numeric$Attrition,
                       type = "classification",
                       label = "Random Forrest Down")

set.seed(0923)
exp_XGB_down <- explain(model = mod_XGB_down_FINAL,
                        data = split_val_Numeric, y = split_val_Numeric$Attrition,
                        type = "classification",
                        label = "XGB Down")

set.seed(0923)
exp_SVM_down <- explain(model = mod_SVM_down_FINAL,
                        data = split_val_Numeric, y = split_val_Numeric$Attrition,
                        type = "classification",
                        label = "SVM Down")

set.seed(0923)
exp_LOG_down <- explain(model = mod_LOG_down_FINAL,
                        data = split_val_Numeric, y = split_val_Numeric$Attrition,
                        type = "classification",
                        label = "Logistic Regression Down")

set.seed(0923)
exp_ELAS_down <- explain(model = mod_ELAS_down_FINAL,
                         data = split_val_Numeric, y = split_val_Numeric$Attrition,
                         type = "classification",
                         label = "Regularized Regression Down")

# Model Diagnostics:
# - list
perf_Resample_Down <- list(DT_down = model_performance(exp_DT_down),
                           RF_down = model_performance(exp_RF_down),
                           XGB_down = model_performance(exp_XGB_down),
                           SVM_down = model_performance(exp_SVM_down),
                           LOG_down =  model_performance(exp_LOG_down),
                           ELAS_down = model_performance(exp_ELAS_down))
# - extract: AUC | Accuracy | Precision | Recall | F1
df_perf_Resample_Down <- perf_Resample_Down %>% 
  map_df(~ data.frame(
    AUC = .x$measures$auc %>% round(2),
    Accuracy = .x$measures$accuracy %>% round(2),
    Precision = .x$measures$precision %>% round(2),
    Recall = .x$measures$recall %>% round(2),
    F1 = .x$measures$f1 %>% round(2))
  ) %>% 
  mutate(Name = names(perf_Resample_Down)) %>% 
  select(Name, everything()) 
# - dataframe
df_perf_Resample_Down %>% arrange(desc(Recall))
# - note: Best "Undersample" Model in terms of Recall "Regularized Regression (Recall = 79%)"

# Model Diagnostics: Optimal Thresholds
# - calculate optimal thresholds in terms of best "F1" score
thres_Cut_Down <- data.frame(
  DT = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                     predictedScores = perf_Resample_Down$DT_down$residuals$predicted, 
                     optimiseFor = "Both") %>% round(2),
  RF = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                     predictedScores = perf_Resample_Down$RF_down$residuals$predicted, 
                     optimiseFor = "Both") %>% round(2),
  XGB = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Resample_Down$XGB_down$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  SVM = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Resample_Down$SVM_down$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  LOG = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Resample_Down$LOG_down$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  ELAS = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                       predictedScores = perf_Resample_Down$ELAS_down$residuals$predicted, 
                       optimiseFor = "Both") %>% round(2)
)
# Plot: ROC Curve + Optimal Threshold
gg_roc_Down <- plot(perf_Resample_Down$DT_down, perf_Resample_Down$RF_down, perf_Resample_Down$XGB_down, perf_Resample_Down$SVM_down, perf_Resample_Down$LOG_down, perf_Resample_Down$ELAS_down,
     geom = "roc") + 
  labs(
    title = "ROC: Undersample",
    subtitle = "Optimal Threshold (F1)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = 'blue4', face = 'bold'), 
    plot.subtitle = element_text(hjust = 0.5, size = 15, color = 'lightblue')
  ) +
  # Optimal Threshold: Logistic Regression
  geom_label(
    label=paste0("Logistic Regression: ",thres_Cut_Down$DT), 
    x=0.68,
    y=0.40,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: XGB
  geom_label(
    label=paste0("XGB: ",thres_Cut_Down$XGB), 
    x=0.68,
    y=0.33,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Random Forrest: ",thres_Cut_Down$RF), 
    x=0.68,
    y=0.26,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: SVM
  geom_label(
    label=paste0("SVM: ",thres_Cut_Down$SVM), 
    x=0.68,
    y=0.19,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Regularized Regression: ",thres_Cut_Down$ELAS), 
    x=0.68,
    y=0.12,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Decision Tree: ",thres_Cut_Down$DT), 
    x=0.68,
    y=0.05,
    label.size = 0.1,
    color = "black",
    fill="lightblue"
  )
# - visual
gg_roc_Down
# - list (optimal thershold)
perf_Resample_Down_Thres <- 
  list(DT_down_Thres.79 = model_performance(exp_DT_down, cutoff = thres_Cut_Down$DT),
       RF_down_Thres.55 = model_performance(exp_RF_down, cutoff = thres_Cut_Down$RF),
       XGB_down_Thres.43 = model_performance(exp_XGB_down, cutoff = thres_Cut_Down$XGB),
       SVM_down_Thres.46 = model_performance(exp_SVM_down, cutoff = thres_Cut_Down$SVM),
       LOG_down_Thres.79 =  model_performance(exp_LOG_down, cutoff = thres_Cut_Down$LOG),
       ELAS_down_Thres.72 = model_performance(exp_ELAS_down, cutoff = thres_Cut_Down$ELAS)
  )
# - extract: AUC | Accuracy | Precision | Recall | F1 (optimal thershold)
df_perf_Resample_Down_Thres <- perf_Resample_Down_Thres %>% 
  map_df(~ data.frame(
    AUC = .x$measures$auc %>% round(2),
    Accuracy = .x$measures$accuracy %>% round(2),
    Precision = .x$measures$precision %>% round(2),
    Recall = .x$measures$recall %>% round(2),
    F1 = .x$measures$f1 %>% round(2))
  ) %>% 
  mutate(Name = names(perf_Resample_Down_Thres)) %>% 
  select(Name, everything()) 
# - dataframe
df_perf_Resample_Down_Thres %>% arrange(desc(Recall))
# - note: Best "Undersample + Threshold" Model in terms of Recall "XGB.Thres.43% (Recall = 83%)"

## Modeling: Oversample Fit ----

# Change ctrl for Undersampling
ctrl_Parallel_Up <- ctrl_Parallel
ctrl_Parallel_Up$sampling <- "up"

# Register 3 Clusters
cl_3 <- makeCluster(3)
registerDoParallel(cl_3)

# DECISION TREE: Oversample
# - set seed
ctrl_Parallel_Up$seeds <- seed_DT
# - model
mod_DT_up <- train(Attrition ~ .,data = split_train,
                   method = "rpart", metric = "ROC",
                   tuneLength = 10,
                   trControl = ctrl_Parallel_Up)
# - plot: hyperparameters
gg_mod_DT_up <- mod_DT_up$results %>% 
  ggplot(aes(x = cp, y = Recall)) +
  geom_line(color = "blue") + geom_point(color = "blue") +
  theme_bw() +
  labs(
    title = "Decision Tree",
    subtitle = "Oversample"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue')
  )
# - visual
gg_mod_DT_up
# - store best hyperparameters in terms of recall: 
# -- Cost Complexity = 0
df_DT_up_tune <- data.frame(
  cp = mod_DT_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(cp)
)
# - set final seed
ctrl_Parallel_Up$seeds <- seed_FINAL
# - final model
mod_DT_up_FINAL <- train(Attrition ~ ., data = split_train,
                         method = "rpart",
                         tuneGrid = df_DT_up_tune,
                         trControl = ctrl_Parallel_Up)


# RANDOM FORREST: Oversample 
# - set seed
ctrl_Parallel_Up$seeds <- seed_RF
# - model
mod_RF_up <- train(Attrition ~ ., data = split_train,
                   method = "ranger",
                   tuneLength = 20,
                   trControl = ctrl_Parallel_Up)
# - plot: hyperparameters
gg_mod_RF_up <- mod_RF_up$results %>% 
  ggplot(aes(x = mtry, y = Recall, color = splitrule)) +
  geom_line() + geom_point() +
  theme() + theme_bw() +
  labs(
    title = "Random Forrest",
    subtitle = "Oversample",
    x = "Number of Features"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom"
  )
# - visual
gg_mod_RF_up
# - store best hyperparameters in terms of recall: 
# -- # Of Randomly Selected Features = 5
# -- Minimal Node Size = 1
# -- Splitrule = gini
df_RF_up_tune <- data.frame(
  mtry = mod_RF_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(mtry),
  min.node.size = mod_RF_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(min.node.size),
  splitrule = mod_RF_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(splitrule)
)
# - set final seed
ctrl_Parallel_Up$seeds <- seed_FINAL
# - final model
mod_RF_up_FINAL <- train(Attrition ~ ., data = split_train,
                         method = "ranger",
                         tuneGrid = df_RF_up_tune,
                         trControl = ctrl_Parallel_Up)


# XGB: Oversample
# - set seed
ctrl_Parallel_Up$seeds <- seed_XGB
# - model
mod_XGB_up <- train(Attrition ~ .,data = split_train,
                    method = "xgbTree",
                    tuneLength = 5,
                    trControl = ctrl_Parallel_Up)
# - plot: hyperparameters
gg_mod_XGB_up <- mod_XGB_up$results %>%
  mutate(eta = factor(eta), gamma = factor(gamma), max_depth = factor(max_depth)) %>% 
  ggplot(aes(x = nrounds, y = Recall, color = eta)) +
  geom_line() + geom_point() +
  facet_wrap(~ max_depth, nrow = 1) +
  labs(
    title = "eXtreme Gradient Boosting",
    subtitle = "Oversample"
  ) + 
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom"
  )
# - visual
gg_mod_XGB_up
# - store best hyperparameters in terms of recall: 
# -- eta "Shrinkage" = 0.3
# -- Max Tree Depth = 4
# -- gamma "Minimum Loss Reduction" = 0
# -- colsample_bytree "Subsample Ratio of Columns" = 0.6
# -- min_child_weight "Minimum Sum of Instance Weight" = 1
# -- subsample = 0.875
# -- nrounds "# Boosting Iterations" = 200
df_XGB_up_tune <- data.frame(
  eta = mod_XGB_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(eta),
  max_depth = mod_XGB_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(max_depth),
  gamma = mod_XGB_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(gamma),
  colsample_bytree = mod_XGB_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(colsample_bytree),
  min_child_weight = mod_XGB_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(min_child_weight),
  subsample = mod_XGB_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(subsample),
  nrounds = mod_XGB_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(nrounds)
)
# - set final seed
ctrl_Parallel_Up$seeds <- seed_FINAL
# - final model
mod_XGB_up_FINAL <- train(Attrition ~ ., data = split_train,
                          method = "xgbTree",
                          tuneGrid = df_XGB_up_tune,
                          trControl = ctrl_Parallel_Up)


# SVM: Oversample
# - set seed
ctrl_Parallel_Up$seeds <- seed_SVM
# - model
mod_SVM_up <- train(Attrition ~ ., data = split_train,
                    method = "svmRadialSigma", 
                    preProcess = c("center", "scale"),
                    tuneLength = 7,
                    trControl = ctrl_Parallel_Up)
# - plot: hyperparameters
gg_mod_SVM_up <- mod_SVM_up$results %>% mutate(Cost = factor(C)) %>% 
  ggplot(aes(x = sigma, y = Recall, color = Cost)) +
  geom_point() + geom_line() +
  theme_bw() +
  labs(
    title = "Support Vector Machine",
    subtitle = "Oversample"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom"
  )
# - visual
gg_mod_SVM_up
# - store best hyperparameters in terms of recall:
# -- sigma = 0.011
# -- cost = 16
df_SVM_up_tune <- data.frame(
  sigma = mod_SVM_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(sigma),
  C = mod_SVM_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(C)
)
# - set final seed
ctrl_Parallel_Up$seeds <- seed_FINAL
# - final model
mod_SVM_up_FINAL <- train(Attrition ~ ., data = split_train,
                          method = "svmRadialSigma",
                          preProcess = c("center", "scale"),
                          tuneGrid = df_SVM_up_tune,
                          trControl = ctrl_Parallel_Up)


# LOGISTIC REGRESSION: Oversample
# - set seed
ctrl_Parallel_Up$seeds <- seed_LOG
# - final model
mod_LOG_up_FINAL <- train(Attrition ~ ., data = split_train,
                          method = "glm",
                          preProcess = c("zv","nzv","center","scale"),
                          trControl = ctrl_Parallel_Up)

# REGULARIZED REGRESSION: Oversample
# - set seed
ctrl_Parallel_Up$seeds <- seed_ELAS
# - model
mod_ELAS_up <- train(Attrition ~ ., data = split_train,
                     method = "glmnet",
                     preProcess = c("zv","nzv","center","scale"),
                     tuneLength = 15,
                     trControl = ctrl_Parallel_Up)
# - plot: hyperparameters
gg_mod_ELAS_up <- mod_ELAS_up$results %>% mutate(alpha = case_when(
  alpha >= 0   & alpha < 0.2 ~ "[0 - 0.2)",
  alpha >= 0.2 & alpha < 0.4 ~ "[0.2 - 0.4)",
  alpha >= 0.4 & alpha < 0.6 ~ "[0.4 - 0.6)",
  alpha >= 0.6 & alpha < 0.8 ~ "[0.6 - 0.8)",
  alpha >= 0.8 & alpha <= 1  ~ "[0.8 - 1]"
) %>% factor) %>% 
  ggplot(aes(x = lambda, y = Recall, color = alpha)) +
  geom_line() + geom_point() + 
  labs(
    title = "Regularized Regression",
    subtitle = "Oversample"
  ) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom",
    legend.text = element_text(size = 5)
  )
# - visual
gg_mod_ELAS_up
# - store best hyperparameters in terms of recall: 
# -- alpha = 0.74
# -- lambda = 0.001
df_ELAS_up_tune <- data.frame(
  alpha = mod_ELAS_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(alpha),
  lambda = mod_ELAS_up$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(lambda)
)
# - set final seed
ctrl_Parallel_Up$seeds <- seed_FINAL
# - final model
mod_ELAS_up_FINAL <- train(Attrition ~ ., data = split_train,
                           method = "glmnet",
                           preProcess = c("zv","nzv","center","scale"),
                           tuneGrid = df_ELAS_up_tune,
                           trControl = ctrl_Parallel_Up)

# Stop Cluster
stopCluster(cl_3)









## Modeling: Oversample Diagnostics ----

# Create Explainers for the Oversample models
set.seed(0923)
exp_DT_up <- explain(model = mod_DT_up_FINAL,
                     data = split_val_Numeric, y = split_val_Numeric$Attrition,
                     type = "classification",
                     label = "Decision Tree Up")

set.seed(0923)
exp_RF_up <- explain(model = mod_RF_up_FINAL,
                     data = split_val_Numeric, y =  split_val_Numeric$Attrition,
                     type = "classification",
                     label = "Random Forrest Up")

set.seed(0923)
exp_XGB_up <- explain(model = mod_XGB_up_FINAL,
                      data = split_val_Numeric, y = split_val_Numeric$Attrition,
                      type = "classification",
                      label = "XGB Up")

set.seed(0923)
exp_SVM_up <- explain(model = mod_SVM_up_FINAL,
                      data = split_val_Numeric, y = split_val_Numeric$Attrition,
                      type = "classification",
                      label = "SVM Up")

set.seed(0923)
exp_LOG_up <- explain(model = mod_LOG_up_FINAL,
                      data = split_val_Numeric, y = split_val_Numeric$Attrition,
                      type = "classification",
                      label = "Logistic Regression Up")

set.seed(0923)
exp_ELAS_up <- explain(model = mod_ELAS_up_FINAL,
                       data = split_val_Numeric, y = split_val_Numeric$Attrition,
                       type = "classification",
                       label = "Regularized Regression Up")

# Model Diagnostics:
# - list
perf_Resample_Up <- list(DT_up = model_performance(exp_DT_up),
                         RF_up = model_performance(exp_RF_up),
                         XGB_up = model_performance(exp_XGB_up),
                         SVM_up = model_performance(exp_SVM_up),
                         LOG_up =  model_performance(exp_LOG_up),
                         ELAS_up = model_performance(exp_ELAS_up))
# - extract: AUC | Accuracy | Precision | Recall | F1
df_perf_Resample_Up <- perf_Resample_Up %>% 
  map_df(~ data.frame(
    AUC = .x$measures$auc %>% round(2),
    Accuracy = .x$measures$accuracy %>% round(2),
    Precision = .x$measures$precision %>% round(2),
    Recall = .x$measures$recall %>% round(2),
    F1 = .x$measures$f1 %>% round(2))
  ) %>% 
  mutate(Name = names(perf_Resample_Up)) %>% 
  select(Name, everything()) 
# - dataframe
df_perf_Resample_Up %>% arrange(desc(Recall))
# - note: Best "Oversample" Model in terms of Recall "Logistic Regression (Recall = 70%)"

# Model Diagnostics: Optimal Thresholds
# - calculate optimal thresholds in terms of best "F1" score
thres_Cut_Up <- data.frame(
  DT = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                     predictedScores = perf_Resample_Up$DT_up$residuals$predicted, 
                     optimiseFor = "Both") %>% round(2),
  RF = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                     predictedScores = perf_Resample_Up$RF_up$residuals$predicted, 
                     optimiseFor = "Both") %>% round(2),
  XGB = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Resample_Up$XGB_up$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  SVM = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Resample_Up$SVM_up$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  LOG = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Resample_Up$LOG_up$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  ELAS = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                       predictedScores = perf_Resample_Up$ELAS_up$residuals$predicted, 
                       optimiseFor = "Both") %>% round(2)
)
# Plot: ROC Curve + Optimal Threshold
gg_roc_Up <- plot(perf_Resample_Up$DT, perf_Resample_Up$RF, perf_Resample_Up$XGB, perf_Resample_Up$SVM, perf_Resample_Up$LOG, perf_Resample_Up$ELAS,
                    geom = "roc") + 
  labs(
    title = "ROC: Oversample",
    subtitle = "Optimal Threshold (F1)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = 'blue4', face = 'bold'), 
    plot.subtitle = element_text(hjust = 0.5, size = 15, color = 'lightblue')
  ) +
  # Optimal Threshold: Logistic Regression
  geom_label(
    label=paste0("Logistic Regression: ",thres_Cut_Up$DT), 
    x=0.68,
    y=0.40,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: XGB
  geom_label(
    label=paste0("XGB: ",thres_Cut_Up$XGB), 
    x=0.68,
    y=0.33,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Random Forrest: ",thres_Cut_Up$RF), 
    x=0.68,
    y=0.26,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: SVM
  geom_label(
    label=paste0("SVM: ",thres_Cut_Up$SVM), 
    x=0.68,
    y=0.19,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Regularized Regression: ",thres_Cut_Up$ELAS), 
    x=0.68,
    y=0.12,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Decision Tree: ",thres_Cut_Up$DT), 
    x=0.68,
    y=0.05,
    label.size = 0.1,
    color = "black",
    fill="lightblue"
  )
# - visual
gg_roc_Up
# - list (optimal thershold)
perf_Resample_Up_Thres <- 
  list(DT_up_Thres.23 = model_performance(exp_DT_up, cutoff = thres_Cut_Up$DT),
       RF_up_Thres.24 = model_performance(exp_RF_up, cutoff = thres_Cut_Up$RF),
       XGB_up_Thres.2 = model_performance(exp_XGB_up, cutoff = thres_Cut_Up$XGB),
       SVM_up_Thres.03 = model_performance(exp_SVM_up, cutoff = thres_Cut_Up$SVM),
       LOG_up_Thres.23 =  model_performance(exp_LOG_up, cutoff = thres_Cut_Up$LOG),
       ELAS_up_Thres.6 = model_performance(exp_ELAS_up, cutoff = thres_Cut_Up$ELAS)
  )
# - extract: AUC | Accuracy | Precision | Recall | F1 (optimal thershold)
df_perf_Resample_Up_Thres <- perf_Resample_Up_Thres %>% 
  map_df(~ data.frame(
    AUC = .x$measures$auc %>% round(2),
    Accuracy = .x$measures$accuracy %>% round(2),
    Precision = .x$measures$precision %>% round(2),
    Recall = .x$measures$recall %>% round(2),
    F1 = .x$measures$f1 %>% round(2))
  ) %>% 
  mutate(Name = names(perf_Resample_Up_Thres)) %>% 
  select(Name, everything()) 
# - dataframe
df_perf_Resample_Up_Thres %>% arrange(desc(Recall))
# - note: Best "Oversample + Threshold" Model in terms of Recall "Random Forrest.Thres.55% (Recall = 79%)"





## Modeling: SMOTE Fit ----

# Change ctrl for Undersampling
ctrl_Parallel_SMOTE <- ctrl_Parallel
ctrl_Parallel_SMOTE$sampling <- "smote"

# Register 3 Clusters
cl_3 <- makeCluster(3)
registerDoParallel(cl_3)

# DECISION TREE: Smote
# - set seed
ctrl_Parallel_SMOTE$seeds <- seed_DT
# - model
mod_DT_smote <- train(Attrition ~ .,data = split_train,
                      method = "rpart", metric = "ROC",
                      tuneLength = 10,
                      trControl = ctrl_Parallel_SMOTE)
# - plot: hyperparameters
gg_mod_DT_smote <- mod_DT_smote$results %>% 
  ggplot(aes(x = cp, y = Recall)) +
  geom_line(color = "blue") + geom_point(color = "blue") +
  theme_bw() +
  labs(
    title = "Decision Tree",
    subtitle = "SMOTE"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue')
  )
# - visual
gg_mod_DT_smote
# - store best hyperparameters in terms of recall: 
# -- Cost Complexity = 0.02
df_DT_smote_tune <- data.frame(
  cp = mod_DT_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(cp)
)
# - set final seed
ctrl_Parallel_SMOTE$seeds <- seed_FINAL
# - final model
mod_DT_smote_FINAL <- train(Attrition ~ ., data = split_train,
                            method = "rpart",
                            tuneGrid = df_DT_smote_tune,
                            trControl = ctrl_Parallel_SMOTE)


# RANDOM FORREST: Smote 
# - set seed
ctrl_Parallel_SMOTE$seeds <- seed_RF
# - model
mod_RF_smote <- train(Attrition ~ ., data = split_train,
                      method = "ranger",
                      tuneLength = 20,
                      trControl = ctrl_Parallel_SMOTE)
# - plot: hyperparameters
gg_mod_RF_smote <- mod_RF_smote$results %>% 
  ggplot(aes(x = mtry, y = Recall, color = splitrule)) +
  geom_line() + geom_point() +
  theme() + theme_bw() +
  labs(
    title = "Random Forrest",
    subtitle = "SMOTE",
    x = "Number of Features"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom"
  )
# - visual
gg_mod_RF_smote
# - store best hyperparameters in terms of recall: 
# -- # Of Randomly Selected Features = 2
# -- Minimal Node Size = 1
# -- Splitrule = gini
df_RF_smote_tune <- data.frame(
  mtry = mod_RF_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(mtry),
  min.node.size = mod_RF_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(min.node.size),
  splitrule = mod_RF_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(splitrule)
)
# - set final seed
ctrl_Parallel_SMOTE$seeds <- seed_FINAL
# - final model
mod_RF_smote_FINAL <- train(Attrition ~ ., data = split_train,
                            method = "ranger",
                            tuneGrid = df_RF_smote_tune,
                            trControl = ctrl_Parallel_SMOTE)


# XGB: Smote
# - set seed
ctrl_Parallel_SMOTE$seeds <- seed_XGB
# - model
mod_XGB_smote <- train(Attrition ~ .,data = split_train,
                       method = "xgbTree",
                       tuneLength = 5,
                       trControl = ctrl_Parallel_SMOTE)
# - plot: hyperparameters
gg_mod_XGB_smote <- mod_XGB_smote$results %>%
  mutate(eta = factor(eta), gamma = factor(gamma), max_depth = factor(max_depth)) %>% 
  ggplot(aes(x = nrounds, y = Recall, color = eta)) +
  geom_line() + geom_point() +
  facet_wrap(~ max_depth, nrow = 1) +
  labs(
    title = "eXtreme Gradient Boosting",
    subtitle = "SMOTE"
  ) + 
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom"
  )
# - visual
gg_mod_XGB_smote
# - store best hyperparameters in terms of recall: 
# -- eta "Shrinkage" = 0.3
# -- Max Tree Depth = 1
# -- gamma "Minimum Loss Reduction" = 0
# -- colsample_bytree "Subsample Ratio of Columns" = 0.6
# -- min_child_weight "Minimum Sum of Instance Weight" = 1
# -- subsample = 0.625
# -- nrounds "# Boosting Iterations" = 50
df_XGB_smote_tune <- data.frame(
  eta = mod_XGB_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(eta),
  max_depth = mod_XGB_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(max_depth),
  gamma = mod_XGB_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(gamma),
  colsample_bytree = mod_XGB_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(colsample_bytree),
  min_child_weight = mod_XGB_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(min_child_weight),
  subsample = mod_XGB_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(subsample),
  nrounds = mod_XGB_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(nrounds)
)
# - set final seed
ctrl_Parallel_SMOTE$seeds <- seed_FINAL
# - final model
mod_XGB_smote_FINAL <- train(Attrition ~ ., data = split_train,
                             method = "xgbTree",
                             tuneGrid = df_XGB_smote_tune,
                             trControl = ctrl_Parallel_SMOTE)


# SVM: Smote
# - set seed
ctrl_Parallel_SMOTE$seeds <- seed_SVM
# - model
mod_SVM_smote <- train(Attrition ~ ., data = split_train,
                       method = "svmRadialSigma", 
                       preProcess = c("center", "scale"),
                       tuneLength = 7,
                       trControl = ctrl_Parallel_SMOTE)
# - plot: hyperparameters
gg_mod_SVM_smote <- mod_SVM_smote$results %>% mutate(Cost = factor(C)) %>% 
  ggplot(aes(x = sigma, y = Recall, color = Cost)) +
  geom_point() + geom_line() +
  theme_bw() +
  labs(
    title = "Support Vector Machine",
    subtitle = "SMOTE"
  ) + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom"
  )
# - visual
gg_mod_SVM_smote
# - store best hyperparameters in terms of recall:
# -- sigma = 0.011
# -- cost = 4
df_SVM_smote_tune <- data.frame(
  sigma = mod_SVM_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(sigma),
  C = mod_SVM_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(C)
)
# - set final seed
ctrl_Parallel_SMOTE$seeds <- seed_FINAL
# - final model
mod_SVM_smote_FINAL <- train(Attrition ~ ., data = split_train,
                             method = "svmRadialSigma",
                             preProcess = c("center", "scale"),
                             tuneGrid = df_SVM_smote_tune,
                             trControl = ctrl_Parallel_SMOTE)


# LOGISTIC REGRESSION: Smote
# - set seed
ctrl_Parallel_SMOTE$seeds <- seed_LOG
# - final model
mod_LOG_smote_FINAL <- train(Attrition ~ ., data = split_train,
                             method = "glm",
                             preProcess = c("zv","nzv","center","scale"),
                             trControl = ctrl_Parallel_SMOTE)

# REGULARIZED REGRESSION: Smote
# - set seed
ctrl_Parallel_SMOTE$seeds <- seed_ELAS
# - model
mod_ELAS_smote <- train(Attrition ~ ., data = split_train,
                        method = "glmnet",
                        preProcess = c("zv","nzv","center","scale"),
                        tuneLength = 15,
                        trControl = ctrl_Parallel_SMOTE)
# - plot: hyperparameters
gg_mod_ELAS_smote <- mod_ELAS_smote$results %>% mutate(alpha = case_when(
  alpha >= 0   & alpha < 0.2 ~ "[0 - 0.2)",
  alpha >= 0.2 & alpha < 0.4 ~ "[0.2 - 0.4)",
  alpha >= 0.4 & alpha < 0.6 ~ "[0.4 - 0.6)",
  alpha >= 0.6 & alpha < 0.8 ~ "[0.6 - 0.8)",
  alpha >= 0.8 & alpha <= 1  ~ "[0.8 - 1]"
) %>% factor) %>% 
  ggplot(aes(x = lambda, y = Recall, color = alpha)) +
  geom_line() + geom_point() + 
  labs(
    title = "Regularized Regression",
    subtitle = "SMOTE"
  ) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = 'green4', face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'blue'),
    legend.position = "bottom",
    legend.text = element_text(size = 5)
  )
# - visual
gg_mod_ELAS_smote
# - store best hyperparameters in terms of recall: 
# -- alpha = 0.94
# -- lambda = 0.11
df_ELAS_smote_tune <- data.frame(
  alpha = mod_ELAS_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(alpha),
  lambda = mod_ELAS_smote$results %>% arrange(desc(Recall)) %>% slice(1) %>% pull(lambda)
)
# - set final seed
ctrl_Parallel_SMOTE$seeds <- seed_FINAL
# - final model
mod_ELAS_smote_FINAL <- train(Attrition ~ ., data = split_train,
                              method = "glmnet",
                              preProcess = c("zv","nzv","center","scale"),
                              tuneGrid = df_ELAS_smote_tune,
                              trControl = ctrl_Parallel_SMOTE)

# Stop Cluster
stopCluster(cl_3)









## Modeling: SMOTE Diagnostics ----

# Create Explainers for the SMOTE models
set.seed(0923)
exp_DT_smote <- explain(model = mod_DT_smote_FINAL,
                        data = split_val_Numeric, y = split_val_Numeric$Attrition,
                        type = "classification",
                        label = "Decision Tree SMOTE")

set.seed(0923)
exp_RF_smote <- explain(model = mod_RF_smote_FINAL,
                        data = split_val_Numeric, y =  split_val_Numeric$Attrition,
                        type = "classification",
                        label = "Random Forrest SMOTE")

set.seed(0923)
exp_XGB_smote <- explain(model = mod_XGB_smote_FINAL,
                         data = split_val_Numeric, y = split_val_Numeric$Attrition,
                         type = "classification",
                         label = "XGB SMOTE")

set.seed(0923)
exp_SVM_smote <- explain(model = mod_SVM_smote_FINAL,
                         data = split_val_Numeric, y = split_val_Numeric$Attrition,
                         type = "classification",
                         label = "SVM SMOTE")

set.seed(0923)
exp_LOG_smote <- explain(model = mod_LOG_smote_FINAL,
                         data = split_val_Numeric, y = split_val_Numeric$Attrition,
                         type = "classification",
                         label = "Logistic Regression SMOTE")

set.seed(0923)
exp_ELAS_smote <- explain(model = mod_ELAS_smote_FINAL,
                          data = split_val_Numeric, y = split_val_Numeric$Attrition,
                          type = "classification",
                          label = "Regularized Regression SMOTE")

# Model Diagnostics:
# - list
perf_Resample_SMOTE <- list(DT_smote = model_performance(exp_DT_smote),
                            RF_smote = model_performance(exp_RF_smote),
                            XGB_smote = model_performance(exp_XGB_smote),
                            SVM_smote = model_performance(exp_SVM_smote),
                            LOG_smote =  model_performance(exp_LOG_smote),
                            ELAS_smote = model_performance(exp_ELAS_smote))
# - extract: AUC | Accuracy | Precision | Recall | F1
df_perf_Resample_SMOTE <- perf_Resample_SMOTE %>% 
  map_df(~ data.frame(
    AUC = .x$measures$auc %>% round(2),
    Accuracy = .x$measures$accuracy %>% round(2),
    Precision = .x$measures$precision %>% round(2),
    Recall = .x$measures$recall %>% round(2),
    F1 = .x$measures$f1 %>% round(2))
  ) %>% 
  mutate(Name = names(perf_Resample_SMOTE)) %>% 
  select(Name, everything()) 
# - dataframe
df_perf_Resample_SMOTE %>% arrange(desc(Recall))
# - note: Best "SMOTE" Model in terms of Recall "Logistic Regression (Recall = 57%)"

# Model Diagnostics: Optimal Thresholds
# - calculate optimal thresholds in terms of best "F1" score
thres_Cut_SMOTE <- data.frame(
  DT = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                     predictedScores = perf_Resample_SMOTE$DT_smote$residuals$predicted, 
                     optimiseFor = "Both") %>% round(2),
  RF = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                     predictedScores = perf_Resample_SMOTE$RF_smote$residuals$predicted, 
                     optimiseFor = "Both") %>% round(2),
  XGB = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Resample_SMOTE$XGB_smote$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  SVM = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Resample_SMOTE$SVM_smote$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  LOG = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                      predictedScores = perf_Resample_SMOTE$LOG_smote$residuals$predicted, 
                      optimiseFor = "Both") %>% round(2),
  ELAS = optimalCutoff(actuals = split_val_Numeric$Attrition, 
                       predictedScores = perf_Resample_SMOTE$ELAS_smote$residuals$predicted, 
                       optimiseFor = "Both") %>% round(2)
)
# Plot: ROC Curve + Optimal Threshold
gg_roc_SMOTE <-
  plot(perf_Resample_SMOTE$DT_smote, perf_Resample_SMOTE$RF_smote, perf_Resample_SMOTE$XGB_smote, perf_Resample_SMOTE$SVM_smote, perf_Resample_SMOTE$LOG_smote, perf_Resample_SMOTE$ELAS_smote,
       geom = "roc") + 
  labs(
    title = "ROC: SMOTE",
    subtitle = "Optimal Threshold (F1)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = 'blue4', face = 'bold'), 
    plot.subtitle = element_text(hjust = 0.5, size = 15, color = 'lightblue')
  ) +
  # Optimal Threshold: Logistic Regression
  geom_label(
    label=paste0("Logistic Regression: ",thres_Cut_SMOTE$DT), 
    x=0.68,
    y=0.40,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: XGB
  geom_label(
    label=paste0("XGB: ",thres_Cut_SMOTE$XGB), 
    x=0.68,
    y=0.33,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Random Forrest: ",thres_Cut_SMOTE$RF), 
    x=0.68,
    y=0.26,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: SVM
  geom_label(
    label=paste0("SVM: ",thres_Cut_SMOTE$SVM), 
    x=0.68,
    y=0.19,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Regularized Regression: ",thres_Cut_SMOTE$ELAS), 
    x=0.68,
    y=0.12,
    label.size = 0.01,
    color = "black",
    fill="lightblue"
  ) +
  # Optimal Threshold: Random Forrest
  geom_label(
    label=paste0("Decision Tree: ",thres_Cut_SMOTE$DT), 
    x=0.68,
    y=0.05,
    label.size = 0.1,
    color = "black",
    fill="lightblue"
  )
# - visual
gg_roc_SMOTE
# - list (optimal thershold)
perf_Resample_SMOTE_Thres <- list(DT_smote_Thres.14 = model_performance(exp_DT_smote, cutoff = thres_Cut_SMOTE$DT),
                                  RF_smote_Thres.29 = model_performance(exp_RF_smote, cutoff = thres_Cut_SMOTE$RF),
                                  XGB_smote_Thres.23 = model_performance(exp_XGB_smote, cutoff = thres_Cut_SMOTE$XGB),
                                  SVM_smote_Thres.22 = model_performance(exp_SVM_smote, cutoff = thres_Cut_SMOTE$SVM),
                                  LOG_smote_Thres.14 =  model_performance(exp_LOG_smote, cutoff = thres_Cut_SMOTE$LOG),
                                  ELAS_smote_Thres.4 = model_performance(exp_ELAS_smote, cutoff = thres_Cut_SMOTE$ELAS))
# - extract: AUC | Accuracy | Precision | Recall | F1 (optimal thershold)
df_perf_Resample_SMOTE_Thres <- perf_Resample_SMOTE_Thres %>% 
  map_df(~ data.frame(
    AUC = .x$measures$auc %>% round(2),
    Accuracy = .x$measures$accuracy %>% round(2),
    Precision = .x$measures$precision %>% round(2),
    Recall = .x$measures$recall %>% round(2),
    F1 = .x$measures$f1 %>% round(2))
  ) %>% 
  mutate(Name = names(perf_Resample_SMOTE_Thres)) %>% 
  select(Name, everything()) 
# - dataframe
df_perf_Resample_SMOTE_Thres %>% arrange(desc(Recall))
# - note: Best "SMOTE + Threshold" Model in terms of Recall "DecisionTree.Thres.14% (Recall = 79%)"



## Modeling: Validation Results ----

# Combine ALL Model Results
df_perf_ALL <- 
  bind_rows(df_perf_Baseline, df_perf_Baseline_Thres, 
            df_perf_Resample_Down, df_perf_Resample_Down_Thres, 
            df_perf_Resample_Up, df_perf_Resample_Up_Thres, 
            df_perf_Resample_SMOTE, df_perf_Resample_SMOTE_Thres) %>% 
  arrange(desc(Recall),desc(F1))
# - add IDs
df_perf_ALL$ID <- seq.int(nrow(df_perf_ALL))
# - dataframe
df_perf_ALL %>% select(ID, everything()) 
# - note: of all 48 models if I have a cutoff of (Recall 70%) it reduces to 21


## Modeling: Test Results ----
# NOTE: the top 20 models will be revaulated using the testset

# In order to use the DALEX package the target value datatype must be adjusted
split_test_Numeric <- split_test %>% mutate(Attrition = ifelse(Attrition == "Churn", 1,0)) 

# 1: XGB-Down (Thres 43%)
set.seed(0923)
exp_XGB_down_Test <- explain(model = mod_XGB_down_FINAL,
                             data = split_test_Numeric, y = split_test_Numeric$Attrition,
                             type = "classification",
                             label = "XGB (Down)")
# 2: Random Forrest-Up (Thres 24%)
set.seed(0923)
exp_RF_up_Test <- explain(model = mod_RF_up_FINAL,
                          data = split_test_Numeric, y = split_test_Numeric$Attrition,
                          type = "classification",
                          label = "Random Forrest (Up)")
# 3: Regularized Regression-Down
set.seed(0923)
exp_ELAS_down_Test <- explain(model = mod_ELAS_down_FINAL,
                              data = split_test_Numeric, y = split_test_Numeric$Attrition,
                              type = "classification",
                              label = "Regularized Regression (Down)")
# 4: Regularized Regression-SMOTE (Thres 40%)
set.seed(0923)
exp_ELAS_smote_Test <- explain(model = mod_ELAS_smote_FINAL,
                               data = split_test_Numeric, y = split_test_Numeric$Attrition,
                               type = "classification",
                               label = "Regularized Regression (SMOTE)")
# 5: Decision Tree-SMOTE (Thres 14%)
set.seed(0923)
exp_DT_smote_Test <- explain(model = mod_DT_smote_FINAL,
                             data = split_test_Numeric, y = split_test_Numeric$Attrition,
                             type = "classification",
                             label = "Decision Tree (SMOTE)")
# 6: Logistic Regression (Thres 16%)
set.seed(0923)
exp_LOG_Test <- explain(model = mod_LOG_FINAL,
                        data = split_test_Numeric, y = split_test_Numeric$Attrition,
                        type = "classification",
                        label = "Logistic Regression")
# 7: Logistic Regression-SMOTE (Thres 14%)
set.seed(0923)
exp_LOG_smote_Test <- explain(model = mod_LOG_smote_FINAL,
                              data = split_test_Numeric, y = split_test_Numeric$Attrition,
                              type = "classification",
                              label = "Logistic Regression (SMOTE)")
# 8: SVM-Down (Thres 46%)
set.seed(0923)
exp_SVM_down_Test <- explain(model = mod_SVM_down_FINAL,
                             data = split_test_Numeric, y = split_test_Numeric$Attrition,
                             type = "classification",
                             label = "SVM (Down)")
# 9: XGB-SMOTE (Thres 23%)
set.seed(0923)
exp_XGB_smote_Test <- explain(model = mod_XGB_smote_FINAL,
                              data = split_test_Numeric, y = split_test_Numeric$Attrition,
                              type = "classification",
                              label = "XGB (SMOTE)")

# 10: Logisitic Regression-Up (Thres 23%)
set.seed(0923)
exp_LOG_up_Test <- explain(model = mod_LOG_up_FINAL,
                           data = split_test_Numeric, y = split_test_Numeric$Attrition,
                           type = "classification",
                           label = "Logistic Regression (Up)")
# 11: Random Forrest-SMOTE (Thres 29%)
set.seed(0923)
exp_RF_smote_Test <- explain(model = mod_RF_smote_FINAL,
                             data = split_test_Numeric, y = split_test_Numeric$Attrition,
                             type = "classification",
                             label = "Random Forrest (SMOTE)")
# 12: Regularized Regression (Thres 21%)
set.seed(0923)
exp_ELAS_Test <- explain(model = mod_ELAS_FINAL,
                         data = split_test_Numeric, y = split_test_Numeric$Attrition,
                         type = "classification",
                         label = "Regularized Regression")
# 13: Logistic Regression-Down
set.seed(0923)
exp_LOG_down_Test <- explain(model = mod_LOG_down_FINAL,
                             data = split_test_Numeric, y = split_test_Numeric$Attrition,
                             type = "classification",
                             label = "Logistic Regression (Down)")
# 14: SVM-SMOTE (Thres 22%)
set.seed(0923)
exp_SVM_smote_Test <- explain(model = mod_SVM_smote_FINAL,
                              data = split_test_Numeric, y = split_test_Numeric$Attrition,
                              type = "classification",
                              label = "SVM (SMOTE)")
# 15: SVM (Thres 13%)
set.seed(0923)
exp_SVM_Test <- explain(model = mod_SVM_FINAL,
                        data = split_test_Numeric, y = split_test_Numeric$Attrition,
                        type = "classification",
                        label = "SVM")
# 16: Logistic Regression-Down (Thres 79%)
# - same as 13
# 17: Logistic Regression-Up
# - same as 10
# 18: Random Forrest-Down (Thres 55%)
set.seed(0923)
exp_RF_down_Test <- explain(model = mod_RF_down_FINAL,
                            data = split_test_Numeric, y = split_test_Numeric$Attrition,
                            type = "classification",
                            label = "Random Forrest (Down)")
# 19: Random Forrest Down
# - same as 18
# 20: XGB-Down
# - same as 1



# Model Diagnostics: Test Results
# - list
perf_Test <- 
  list(
    # 1: XGB-Down (Thres 43%)
    XGB_down_Thres.43_Test = model_performance(exp_XGB_down_Test, cutoff = thres_Cut_Down$XGB),
    # 2: Random Forrest-Up (Thres 24%)
    RF_up_Thres.24_Test = model_performance(exp_RF_up_Test, cutoff = thres_Cut_Up$RF),
    # 3: Regularized Regression-Down
    ELAS_down_Test = model_performance(exp_ELAS_Test),
    # 4: Regularized Regression-SMOTE (Thres 40%)
    ELAS_smote_Thres.40_Test = model_performance(exp_ELAS_smote_Test, cutoff = thres_Cut_SMOTE$ELAS),
    # 5: Decision Tree-SMOTE (Thres 14%)
    DT_smote_Thres.24_Test = model_performance(exp_DT_smote_Test, cutoff = thres_Cut_SMOTE$DT),
    # 6: Logistic Regression (Thres 16%)
    LOG_down_Thres.16_Test = model_performance(exp_LOG_Test, cutoff = thres_Cut_Baseline$LOG),
    # 7: Logistic Regression-SMOTE (Thres 14%)
    LOG_smote_Thres.14_Test = model_performance(exp_LOG_Test, cutoff = thres_Cut_SMOTE$LOG),
    # 8: SVM-Down (Thres 46%)
    SVM_down_Thres.46_Test = model_performance(exp_SVM_down_Test, cutoff = thres_Cut_Down$SVM),
    # 9: XGB-SMOTE (Thres 23%)
    XGB_smote_Thres.23_Test = model_performance(exp_XGB_smote_Test, cutoff = thres_Cut_SMOTE$XGB),
    # 10: Logisitic Regression-Up (Thres 23%)
    LOG_up_Thres.23_Test = model_performance(exp_LOG_up_Test, cutoff = thres_Cut_Up$LOG),
    # 11: Random Forrest-SMOTE (Thres 29%)
    RF_smote_Thres.29_Test = model_performance(exp_RF_smote_Test, cutoff = thres_Cut_SMOTE$RF),
    # 12: Regularized Regression (Thres 21%)
    ELAS_Thres.21_Test = model_performance(exp_ELAS_Test, cutoff = thres_Cut_Baseline$ELAS),
    # 13: Logistic Regression-Down
    LOG_down_Test = model_performance(exp_LOG_down_Test),
    # 14: SVM-SMOTE (Thres 22%)
    SVM_smote_Thres.22_Test = model_performance(exp_SVM_smote_Test, cutoff = thres_Cut_SMOTE$SVM),
    # 15: SVM (Thres 13%)
    SVM_Thres.13_Test = model_performance(exp_SVM_Test, cutoff = thres_Cut_Baseline$SVM),
    # 16: Logistic Regression-Down (Thres 79%)
    LOG_down_Thres.79_Test = model_performance(exp_LOG_down_Test, cutoff = thres_Cut_Down$LOG),
    # 17: Logistic Regression-Up
    LOG_up_Test = model_performance(exp_LOG_up_Test),
    # 18: Random Forrest-Down (Thres 55%)
    RF_down_Thres.55_Test = model_performance(exp_RF_down_Test, cutoff = thres_Cut_Down$RF),
    # 19: Random Forrest Down
    RF_down_Test = model_performance(exp_RF_down_Test),
    # 20: XGB-Down
    XGB_down_Test = model_performance(exp_XGB_down_Test)
  )
# - extract: AUC | Accuracy | Precision | Recall | F1
df_perf_Test <- perf_Test %>% 
  map_df(~ data.frame(
    AUC = .x$measures$auc %>% round(2),
    Accuracy = .x$measures$accuracy %>% round(2),
    Precision = .x$measures$precision %>% round(2),
    Recall = .x$measures$recall %>% round(2),
    F1 = .x$measures$f1 %>% round(2))
  ) %>% 
  mutate(Name = names(perf_Test)) %>% 
  select(Name, everything()) %>% 
  arrange(desc(Recall), desc(Precision))
# - add IDs
df_perf_Test$ID <- seq.int(nrow(df_perf_Test))
# - dataframe
df_perf_Test %>% select(ID, everything())
# - Note: Top Models I choose for further evaluation
# -- 1 XGB-Down (Thres 43%): Performed the best overall in terms of Recall
# -- 2 Logistic Regression-Up: Performed nearly as well as the best model with a better much Precision (Best in terms of F1 score)
# -- 3 SVM (Thres 13%): Perform as well as the best models with no Resampling


## Modeling: Model Performance ----

# Scores
scores <- prepare_scores_and_ntiles(
  datasets = "split_test", dataset_labels = "Test Data",
  models = list("mod_XGB_down_FINAL","mod_LOG_up_FINAL","mod_SVM_FINAL"),
  model_labels = list("XGB-Down (Thres 43%)","Logistic Regression-Up","SVM (Thres 13%)"),
  target_column = "Attrition",
  ntiles = 100)

# Compare Models
plot_input_comparison <- plotting_scope(
  prepared_input = scores, scope = "compare_models",
  #select_dataset_label = "Test Data", 
  select_targetclass = "Churn",
  select_model_label = list("XGB-Down (Thres 43%)","Logistic Regression-Up","SVM (Thres 13%)")
)

# Plot: Cumulative Gain
gg_GAIN <- plot_input_comparison %>% 
  plot_cumgains() + 
  theme(plot.title = element_text(size = 20, color = 'blue4', face = 'bold'),
        plot.subtitle = element_blank())
# Plot: Cumulative Lift
gg_LIFT <- plot_input_comparison %>% 
  plot_cumlift() + 
  theme(plot.title = element_text(size = 20, color = 'blue4', face = 'bold'),
        plot.subtitle = element_blank())
# Visuals
ggarrange(gg_GAIN, gg_LIFT, ncol = 1)
# -  dataframe: 20th Percentile
plot_input_comparison %>%
  filter(ntile == 20) %>% 
  select(model_label, Percentile = ntile, pct, pcttot, cumpct, gain, cumgain, lift, cumlift) %>% 
  group_by(model_label, Percentile) %>% 
  summarise(`Cumulative Gain` = mean(cumgain) %>% round(3),
            `Cumulative Response` = mean(cumpct) %>% round(3),
            `Base Rate (Test)` = mean(pcttot) %>% round(3),
            `Cumulative Lift` = mean(cumlift) %>% round(2)) %>% 
  arrange(desc(`Cumulative Gain`))
# - note: If we look at the 20th percentile 
# -- "Logistic Regression-Up" performed the best compared to the other models capturing 70% of employees that Churned performing 3.5x better than random
# -- "SVM (Thres 13%)" came in 2nd with a Gain of 61.7% perfroming 3x better than random
# - dataframe: 30th Percentile
plot_input_comparison %>%
  filter(ntile == 30) %>% 
  select(model_label, Percentile = ntile, pct, pcttot, cumpct, gain, cumgain, lift, cumlift) %>% 
  group_by(model_label, Percentile) %>% 
  summarise(`Cumulative Gain` = mean(cumgain) %>% round(3),
            `Cumulative Response` = mean(cumpct) %>% round(3),
            `Base Rate (Test)` = mean(pcttot) %>% round(3),
            `Cumulative Lift` = mean(cumlift) %>% round(2)) %>% 
  arrange(desc(`Cumulative Gain`))
# - note: If we look at the 30th percentile 
# -- "Logistic Regression-Up" performed the best compared to the other models capturing 85.1% of employees that Churned performing 2.84x better than random
# -- "SVM (Thres 13%)" came in 2nd with a Gain of 76.6% perfroming 2.56x better than random
# - dataframe: 50th Percentile
plot_input_comparison %>%
  filter(ntile == 50) %>% 
  select(model_label, Percentile = ntile, pct, pcttot, cumpct, gain, cumgain, lift, cumlift) %>% 
  group_by(model_label, Percentile) %>% 
  summarise(`Cumulative Gain` = mean(cumgain) %>% round(3),
            `Cumulative Response` = mean(cumpct) %>% round(3),
            `Base Rate (Test)` = mean(pcttot) %>% round(3),
            `Cumulative Lift` = mean(cumlift) %>% round(2)) %>% 
  arrange(desc(`Cumulative Gain`))
# - note: If we look at the 50th percentile 
# -- "Logistic Regression-Up" & "SVM (Thres 13%)" performed the best capturing 91.5% of employees that Churned performing 1.83x better than random

## Feature Selection ----

# 1 Logistic Regression-Up
set.seed(0923)
vip_LOG_up <- model_parts(exp_LOG_up_Test, type = "difference")
# - plot
gg_vip_LOG_up <- plot(vip_LOG_up, max_vars = 20, show_boxplots = F) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, colour = "tomato")
  ) +
  geom_hline(yintercept = 0.02, linetype = 3, color = "red") +
  labs(title = "Feature Importance",
       subtitle = "(20 Most Important Variables)")
# - visual
gg_vip_LOG_up
# - note: Overtime | YearsWithCurrentManager | JobLevel | JobRole | EnvironmentSatisfaction | YearsinCurrentRole 

# 2 SVM (Thres 13%)
set.seed(0923)
vip_SVM <- model_parts(exp_SVM_Test, type = "difference")
# - plot
gg_vip_SVM <- plot(vip_SVM, max_vars = 20, show_boxplots = F) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, colour = "tomato")
  ) +
  geom_hline(yintercept = 0.02, linetype = 3, color = "red") +
  labs(title = "Feature Importance",
       subtitle = "(20 Most Important Variables)")
# - visual
gg_vip_SVM
# - note: Overtime | WorkLifeBalance | JobLevel | YearsWithCurrentManager | BusinessTravel | EnvironmentSatisfaction


## Risk Bucket: ----

# Create Predictions and Probabilities for each mode
# 1 Logistic Regression-Up
LOG_Up_prob <- predict(mod_LOG_up_FINAL, newdata = split_test, type = "prob") %>% pull(Churn) %>% round(3)
LOG_Up_pred <- ifelse(LOG_Up_prob > 0.50, "Churn", "Stayed")
# 2 SVM (Thres 13%)
SVM.13_prob <- predict(mod_SVM_FINAL, newdata = split_test, type = "prob") %>% pull(Churn) %>% round(3)
SVM.13_pred <- ifelse(SVM.13_prob > 0.13, "Churn", "Stayed")
# - add results to Test Set
test_results <- cbind(split_test, 
                      LOG_Up_pred, LOG_Up_prob, 
                      SVM.13_pred, SVM.13_prob)


# Create Risk-Buckets for each model
# 1 Logistic Regression-Up
LOG_Up_Risk <- test_results %>% 
  select(LOG_Up_pred, LOG_Up_prob) %>% 
  mutate(LOG_Up_Risk = cut(LOG_Up_prob, breaks = c(0,0.5,0.6,0.8,1),
                           labels = c("no-risk","low-risk","medium-risk","high-risk"),
                           include.lowest = TRUE)) %>% 
  pull(LOG_Up_Risk)
# 2 SVM (Thres 13%)
SVM.13_Risk <- test_results %>% 
  select(SVM.13_pred, SVM.13_prob) %>% 
  mutate(SVM.13_Risk = cut(SVM.13_prob, breaks = c(0,0.13,0.6,0.8,1),
                           labels = c("no-risk","low-risk","medium-risk","high-risk"),
                           include.lowest = TRUE)) %>% 
  pull(SVM.13_Risk)
# - add to Dataframe
test_results_risk <- cbind(test_results,
                           LOG_Up_Risk, SVM.13_Risk)
# - gather dataframe
test_results_gathered <- test_results_risk %>% 
  select(LOG_Up = LOG_Up_Risk, SVM.13 = SVM.13_Risk, everything()) %>%  
  gather(LOG_Up, SVM.13, 
         key = "Model", value = "Risk") %>%  
  mutate(Risk = factor(Risk, levels = c("no-risk","low-risk","medium-risk","high-risk")))


# Plot: Risk Bucket (Model Level)
gg_bucket_HighLevel <- test_results_gathered %>% 
  ggplot(aes(Model, fill = Risk)) + 
  geom_bar(position = "fill") +
  labs(title = "Risk Bucket", y = "Proportion") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("grey95","antiquewhite","darkred","magenta"))
# - visual
gg_bucket_HighLevel
# - dataframe
test_results_gathered %>% 
  group_by(Model) %>%
  count(Risk) %>% 
  mutate(Total = sum(n)) %>% 
  filter(Risk %in% c("medium-risk", "high-risk"))
# - note:
# -- Logistic Regression-Up identified 69:(38-High | 31-Med) Risk employees
# -- SVM (Thres 13%) identified 15:(5-High | 8-Med) Risk employees

# NOTE: Only evaluating Logistic Regression-Up moving forward

# Plot: Risk By Department
gg_bucket_Dept <- test_results_gathered %>% 
  filter(
    Model == "LOG_Up",
    Risk %in% c("medium-risk", "high-risk")
    ) %>% 
  group_by(Model, Risk, Department) %>%
  summarise(n = n()) %>% 
  ggplot(aes(Model, n, fill = Risk)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = .9), 
            vjust = 1.2, size = 5, color = "white") +
  facet_wrap(~Department, nrow = 1) +
  labs(title = "Risk Bucket: Department") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  scale_fill_manual(values = c("darkred","magenta"))
# - visuals: 
gg_bucket_Dept
# - note: Of the 69 high-med risk employees
# -- R&D Department has 42:(22-High | 20-Med)
# -- Sales Department has 24:(15-High | 9-Med)


# Plot: Risk By JobRole
gg_bucket_Research <- test_results_gathered %>% 
  filter(
    Model == "LOG_Up",
    Risk %in% c("medium-risk", "high-risk"),
    Department == "Research_Development"
    ) %>% 
  group_by(Model, Risk, JobRole) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(Model, n, fill = JobRole)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = .9), 
            vjust = 1.2, size = 5, color = "white") +
  facet_wrap(~Risk) +
  labs(title = "Risk Bucket: Research") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  scale_fill_brewer(palette = "Accent") 
# - visuals
gg_bucket_Research
# - note: Of the 42:(22-High | 20-Med) in the R&D Dept
# -- 19:(11-High | 8-Med) are Lab Techs
# -- 16:(9-High | 7-Med) are Research Scientist


# Plot Sales: JobRole (Proportion)
gg_bucket_Sales <- test_results_gathered %>% 
  filter(
    Model == "LOG_Up",
    Risk %in% c("medium-risk", "high-risk"),
    Department == "Sales"
    ) %>% 
  group_by(Model, Risk, JobRole) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(Model, n, fill = JobRole)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = .9), 
            vjust = 1.2, size = 5, color = "white") +
  facet_wrap(~Risk) +
  labs(title = "Risk Bucket: Sales") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  scale_fill_brewer(palette = "Spectral")
# - visuals
gg_bucket_Sales
# note: Of the 24:(15-High | 9-Med) in the Sales Dept
# -- 12:(8-High | 4-Med) are Sales Reps
# -- 11:(6-High | 5-Med) are Sales Execs




## Risk Bucket "Logistic Regression-Up": Overtime ----

# Plot: Overtime Risk
gg_bucket_Overtime <- test_results_gathered %>%
  filter(Model == "LOG_Up") %>%
  ggplot(aes(Model, fill = OverTime)) + 
  geom_bar(position = "fill") +
  facet_wrap(~Risk, nrow = 1) +
  labs(title = "Risk Bucket: Overtime") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set2")
# - visual
gg_bucket_Overtime
# - note: employees that have low, medium, and high risk work significantly more Overtime hours than employees with no risk
# - Plot: Risk by Job Role (Overtime)
gg_LOG_Up_Overtime_JobRole <- test_results_gathered %>%
  filter(
    Model == "LOG_Up",
    Department != "Human_Resources",
    Risk %in% c("medium-risk","high-risk"),
    OverTime == "Yes"
    ) %>%
  group_by(Model, Risk, Department, JobRole, OverTime) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(OverTime, n, fill = Risk, color = Department)) +
  geom_col(position = "dodge", alpha = 0.5) + 
  geom_text(aes(label = n), position = position_dodge(width = .9),
            vjust = 1.2, size = 5, color = "white") +
  facet_wrap(~JobRole, nrow = 2) +
  ylab(label = "proportion") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) + 
  scale_fill_manual(values = c("darkred","magenta")) +
  scale_color_manual(values = c("blue", "yellow"))
# - visual
gg_LOG_Up_Overtime_JobRole
# - note:
# -- 13:(8-High | 5-Med) are Research Scientist
# -- 10:(7-High | 3-Med) are Lab Techs
# -- 8:(3-High  | 5-Med) are Sales Executives
# -- 5:(4_High  | 1-Med) are Sales Reps



## Risk Bucket "Logistic Regression-Up": YearsWithCurrentManager ----

# Plot: YearsWithCurrentManager Risk
gg_bucket_YrsCurrMgr <- test_results_gathered %>%
  filter(Model == "LOG_Up") %>%
  ggplot(aes(Model, YearsWithCurrManager, fill = Risk)) + 
  geom_boxplot() + geom_jitter(alpha = 0.2) +
  facet_wrap(~Risk, nrow = 1) +
  labs(title = "Risk Bucket: Years With Current Manager") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("grey90","antiquewhite","darkred","magenta"))
# - visual
gg_bucket_YrsCurrMgr
# - note: Employees that have low, medium and high risk generally have worked with the current manager for a lesser amount of years
# - Plot: Risk by Job Role (YearsWithCurrentManager)
gg_LOG_Up_YrsCurrMgr_JobRole <- test_results_gathered %>%
  filter(Model == "LOG_Up",
         Department != "Human_Resources") %>%
  ggplot(aes(Risk, YearsWithCurrManager, fill = Risk, color = Department)) +
  geom_boxplot() + geom_jitter(alpha = 0.2) +
  facet_wrap(~JobRole, nrow = 2) +
  ylab(label = "proportion") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) + 
  scale_fill_manual(values = c("grey90","antiquewhite","darkred","magenta")) +
  scale_color_manual(values = c("blue", "yellow"))
# - visual
gg_LOG_Up_YrsCurrMgr_JobRole
# - note:  as Years with Current Manager Decreases will on average clearly effect Sales Reps




## Risk Bucket "Logistic Regression-Up": JobLevel ----

# Plot: Job Level Risk
gg_bucket_JobLevel <- test_results_gathered %>%
  filter(Model == "LOG_Up") %>%
  ggplot(aes(Model, fill = JobLevel)) + 
  geom_bar(position = "fill") +
  facet_wrap(~Risk, nrow = 1) +
  labs(title = "Risk Bucket: JobLevel") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set3")
# - visual
gg_bucket_JobLevel
# - note: Employees that have medium and high risk work are mostly JobLevel 1
# - Plot: Risk by JobRole (Job Level)
gg_LOG_Up_JobLevel_JobRole <- test_results_gathered %>%
  filter(Model == "LOG_Up",
         Department != "Human_Resources",
         Risk %in% c("medium-risk","high-risk")) %>%
  group_by(Model, Risk, JobRole, Department, JobLevel) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(JobLevel, n, fill = Risk, color = Department)) +
  geom_col(position = "dodge", alpha = 0.5) + 
  geom_text(aes(label = n), position = position_dodge(width = .9),
            vjust = 1.2, size = 5, color = "white") +
  facet_wrap(~JobRole, nrow = 2) +
  ylab(label = "proportion") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) + 
  scale_fill_manual(values = c("darkred","magenta")) +
  scale_color_manual(values = c("blue", "yellow"))
# - visual
gg_LOG_Up_JobLevel_JobRole
# - note: Job Level 1
# -- 18:(11-High | 7-Med) are Lab Techs
# -- 15:(8-High  | 7-Med) are Research Scientist
# -- 12:(8-High  | 4-Med) are Sales Reps


## Risk Bucket "Logistic Regression-Up": EnvironmentSatisfaction ----

# Plot: Environment Satisfaction Risk
gg_bucket_EnvSat <- test_results_gathered %>%
  filter(Model == "LOG_Up") %>%
  ggplot(aes(Model, fill = EnvironmentSatisfaction)) + 
  geom_bar(position = "fill") +
  facet_wrap(~Risk, nrow = 1) +
  labs(title = "Risk Bucket: Environment Satisfaction") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Blues")
# - visual
gg_bucket_EnvSat
# - note:  Employees that have medium and high risk tend to a LOW Environment Satisfaction.
# - Plot: Risk by Job Role
gg_LOG_Up_EnvSat_JobRole <- test_results_gathered %>%
  filter(Model == "LOG_Up",
         Department != "Human_Resources",
         Risk %in% c("medium-risk","high-risk")) %>%
  group_by(Model, Risk, JobRole, Department, EnvironmentSatisfaction) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(EnvironmentSatisfaction, n, fill = Risk, color = Department)) +
  geom_col(position = "dodge", alpha = 0.5) + 
  geom_text(aes(label = n), position = position_dodge(width = .9),
            vjust = 1.2, size = 5, color = "white") +
  facet_wrap(~JobRole, nrow = 2) +
  ylab(label = "proportion") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) + 
  scale_fill_manual(values = c("darkred","magenta")) +
  scale_color_manual(values = c("blue", "yellow"))
# - visual
gg_LOG_Up_EnvSat_JobRole
# - note:
# -- 7:(5-High | 2-Med) are Lab Techs
# -- 7:(4-High | 3-Med) are Research Scientist
# -- 4:(4-High) are Sales Execs




## Risk Bucket "Logistic Regression-Up": YearsinCurrentRole ----

# Plot: YearsinCurrentRole Risk
gg_bucket_YrsCurrRole <- test_results_gathered %>%
  filter(Model == "LOG_Up") %>%
  ggplot(aes(Model, YearsInCurrentRole, fill = Risk)) + 
  geom_boxplot() + geom_jitter(alpha = 0.2) +
  facet_wrap(~Risk, nrow = 1) +
  labs(title = "Risk Bucket: Years in Current Role") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("grey90","antiquewhite","darkred","magenta"))
# - visual
gg_bucket_YrsCurrRole
# - note: Employees that have low, medium and high risk generally have worked in their current role for a lesser amount of years
# - Plot: Risk by Job Role (YearsinCurrentRole)
gg_LOG_Up_YrsCurrRole_JobRole <- test_results_gathered %>%
  filter(Model == "LOG_Up",
         Department != "Human_Resources") %>%
  ggplot(aes(Risk, YearsInCurrentRole, fill = Risk, color = Department)) +
  geom_boxplot() + geom_jitter(alpha = 0.2) +
  facet_wrap(~JobRole, nrow = 2) +
  ylab(label = "proportion") +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) + 
  scale_fill_manual(values = c("grey90","antiquewhite","darkred","magenta")) +
  scale_color_manual(values = c("blue", "yellow"))
# - visual
gg_LOG_Up_YrsCurrRole_JobRole
# - note:  as the Years in their Current Role Decreases will on average clearly effect Sales Reps




## Prescriptive Analysis: Overtime ----

# Create a dataframe which Hold the count and Overtime % for JobRoles: "Laboratory_Technician","Research_Scientist","Sales_Executive", "Sales_Representative"
# - churn ONLY 
df_testset_Overtime_Churn <- split_test %>% 
  filter(Attrition == "Churn",
         JobRole %in% c("Laboratory_Technician","Research_Scientist","Sales_Executive","Sales_Representative")) %>%
  mutate(OverTime = ifelse(OverTime == "Yes", 1,0)) %>%
  group_by(JobRole) %>% 
  summarise(n = n(),
            Overtime_Pct = round(mean(OverTime),2)) %>% 
  mutate(Churn = "YES")
# - stayed ONLY
df_testset_Overtime_Stayed <- split_test %>% 
  filter(Attrition == "Stayed",
         JobRole %in% c("Laboratory_Technician","Research_Scientist","Sales_Executive","Sales_Representative")) %>%
  mutate(OverTime = ifelse(OverTime == "Yes", 1,0)) %>%
  group_by(JobRole) %>% 
  summarise(n = n(),
            Overtime_Pct = round(mean(OverTime),2)) %>% 
  mutate(Churn = "NO")
# - combine
df_testset_Overtime_BIND <- df_testset_Overtime_Churn %>% 
  rbind(df_testset_Overtime_Stayed)
# - Plot: Actuals
gg_actual_Overtime <- df_testset_Overtime_BIND %>%
  ggplot(aes(JobRole, n, fill = Churn)) +
  geom_col(position = "dodge") + 
  geom_text(aes(label = paste(Overtime_Pct, "%")), position = position_dodge(width = .9),
            vjust = 1.2, size = 3, color = "white") +
  geom_text(aes(label = n), position = position_dodge(width = .9),
            vjust = -0.3, size = 4, color = "blue") +
  labs(title = "Actuals: Overtime %") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.3),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  scale_fill_manual(values = c("forestgreen","tomato"))
# - visual
gg_actual_Overtime
# - note: ACTUALS
# -- Laboratory Technician: 18 Churn | 61% Overtime
# -- Research Scientis:     8  Churn | 75% Overtime
# -- Sales Executives       8  Churn | 62% Overtime
# -- Sales Representative:  7  Churn | 43% Overtime
# - Plot: Predictions
gg_predict_Overtime <- test_results_gathered %>% 
  filter(Model == "LOG_Up",
         JobRole %in% c("Laboratory_Technician","Research_Scientist","Sales_Executive","Sales_Representative"),
         OverTime == "Yes") %>% 
  group_by(JobRole, OverTime, Risk) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(JobRole, n, fill = Risk)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = .9),
            vjust = 1.2, size = 4, color = "white") +
  labs(title = "Predictions: Overtime") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("grey90","antiquewhite","darkred","magenta"))
# - visual
gg_predict_Overtime
# - note: PREDICTIONS
# -- Laboratory Technician: 15:(7-High | 3-Med | 5-Low)
# -- Research Scientist:    14:(8-High | 5-Med | 1-Low)
# -- Sales Executive:       8: (3-High | 5-Med)
# -- Sales Representative:  5: (4-High | 1-Med)

# STRATEGY:
# - For the employees that actually churned. They had a very high Overtime %
# - I think the focus should be first focused one "Laboratory Technicians"
# -- 18/56 Labs Techs from the test set actually churned
# -- This may be a big change to not effect they current work flow process so we'd have to bring in multiple stakeholders to see if we can come up with a solution



## Prescriptive Analysis: Job Level ----

# Create a dataframe which Hold the count of people in JobLevel 1 by JobRoles: "Laboratory_Technician","Research_Scientist","Sales_Representative"
# - churn ONLY
df_testset_Churn_JobLvl1 <- split_test %>% 
  filter(Attrition == "Churn",
         JobLevel == 1,
         JobRole %in% c("Laboratory_Technician","Research_Scientist","Sales_Representative")) %>%
  group_by(JobRole, JobLevel, Attrition) %>% 
  summarise(n = n())
# - stayed ONLY
df_testset_Stayed_JobLvl1 <- split_test %>% 
  filter(Attrition == "Stayed",
         JobLevel == 1,
         JobRole %in% c("Laboratory_Technician","Research_Scientist","Sales_Representative")) %>%
  group_by(JobRole, JobLevel, Attrition) %>% 
  summarise(n = n())
# - combine 
df_testset_JobLvl1_BIND <- df_testset_Churn_JobLvl1 %>% 
  rbind(df_testset_Stayed_JobLvl1)  
# - Plot: Actuals
gg_actual_JobLvl1 <- df_testset_JobLvl1_BIND %>% 
  ggplot(aes(JobRole, n, fill = Attrition)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = .9),
            vjust = 1.2, size = 5, color = "white") +
  labs(title = "Actuals: Job Level 1") +
  theme_minimal() + theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("forestgreen","tomato"))
# - visual
gg_actual_JobLvl1
# - note: ACTUALS
# -- Laboratory Technician: 16/41 Churned
# -- Research Scientist:    8/46 Churned
# -- Sales Representative:  7/14  Churned
# - Plot: Predictions
gg_predict_JobLvl1 <- test_results_gathered %>% 
  filter(Model == "LOG_Up",
         JobRole %in% c("Laboratory_Technician","Research_Scientist","Sales_Representative"),
         JobLevel == 1) %>% 
  group_by(JobRole, JobLevel, Risk) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(JobRole, n, fill = Risk)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = .9),
            vjust = 1.2, size = 4, color = "white") +
  labs(title = "Predictions: Job Level 1") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("grey90","antiquewhite","darkred","magenta"))
# - visual
gg_predict_JobLvl1
# - note: PREDICTIONs
# -- Laboratory Technician: 24:(11-High | 7-Med | 6-Low)
# -- Research Scientist:    16:(8-High  | 7-Med | 1-Low)
# -- Sales Representative:  12:(8-High  | 4-Med) 

# STRATEGY:
# - Assuming JobLevel 1 is Entry Level, 
# - HR and Management can use this model to help facilitate in recruiting and hiring. 
# - Identifying future canidates into "low, medium, and high risk categories could help Management decide on which canidates to pursue further and how based off talent and level of risk.
# - Hiring more “no to low risk” canidates could potentially decrease the overall Churn Rate for the company in future. 
# - Also identifying med-high risk canidates and employees along with what they contribute to the company can further help leaders decide on what to do going forward.





## Prescriptive Analysis: Environment Satisfaction ----

# Create a dataframe which Hold the count of Environment Satisfaction LOW for JobRoles: "Laboratory_Technician","Research_Scientist","Sales_Executive"
# - churn ONLY
df_testset_EnvSat_Churn <- split_test %>% 
  filter(Attrition == "Churn",
         EnvironmentSatisfaction == "Low",
         JobRole %in% c("Laboratory_Technician","Research_Scientist","Sales_Executive")) %>%
  group_by(JobRole, EnvironmentSatisfaction, Attrition) %>% 
  summarise(n = n())
# - stayed ONLY 
df_testset_EnvSat_Stayed <- split_test %>% 
  filter(Attrition == "Stayed",
         EnvironmentSatisfaction == "Low",
         JobRole %in% c("Laboratory_Technician","Research_Scientist","Sales_Executive")) %>%
  group_by(JobRole, EnvironmentSatisfaction, Attrition) %>% 
  summarise(n = n())
# - combine
df_testset_EnvSat_BIND <- df_testset_EnvSat_Churn %>% 
  rbind(df_testset_EnvSat_Stayed)  
# - Plot: Actuals
# Visual
gg_actual_EnvSat <- df_testset_EnvSat_BIND %>% 
  ggplot(aes(JobRole, n, fill = Attrition)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = .9),
            vjust = 1.2, size = 5, color = "white") +
  labs(title = 'Actuals: Environment Satisfaction "LOW"') +
  theme_minimal() + theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("forestgreen","tomato"))
# - visual
gg_actual_EnvSat
# - note: ACTUALS
# -- Laboratory Technician: 6/11  Churned
# -- Research Scientist:    3/11  Churned
# -- Sales Executive:  2/15  Churned
# - Plot: Predictions
gg_predict_EnvSat <- test_results_gathered %>% 
  filter(Model == "LOG_Up",
         JobRole %in% c("Laboratory_Technician","Research_Scientist","Sales_Executive"),
         EnvironmentSatisfaction == "Low") %>% 
  group_by(JobRole, EnvironmentSatisfaction, Risk) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(JobRole, n, fill = Risk)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = .9),
            vjust = 1.2, size = 4, color = "white") +
  labs(title = 'Predictions: Environment Satisfaction "LOW"') +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("grey90","darkred","magenta"))
# - visual
gg_predict_EnvSat
# - note: PREDICTIONs
# -- Laboratory Technician: 7:(5-High | 2-Med)
# -- Research Scientist:    7:(4-High | 3-Med)
# -- Sales Executive:       4:(4-High) 

# STRATEGY:
# - It looks like the primary concern is for Labratory Technicians (54% leave when environment satisfaction is low)
# -- Perform survey to assess the current state of conditions for Lab Techs, that includes phyical environment, and culture. 
# -- From the results we can then work with employees and management to come up with a variety of potential solutions. 
# -- Finally work with stakeholders to find out which solution to implement first.
# -- Once we decide on a solution I will conduct an A/B Test using the same survey and test for statistical significance after implementing our solution.

