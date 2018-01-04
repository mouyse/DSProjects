## HR Analytics Case Study ####


# Please install the below packages
# install.packages(corrplot)
# install.packages(dplyr)
# install.packages(car)
# install.packages(caTools)
# install.packages(caret)
# install.packages(ggplot2)
# install.packages(lubridate)
# install.packages(tidyr)
# install.packages(MASS)
# install.packages("cowplot")
# install.packages("GGally")
# install.packages("ROCR")
# install.packages("Information") # IV table
# install.packages("InformationValue") # Gain Chart Plot
# install.packages("scales") # ggplot percent display

# Load below libraries
library(ggplot2)
library(corrplot)
library(lubridate)
library(dplyr)
library(MASS)
library(car)
library(caTools)
library(caret)
library(cowplot)
library(tidyr)
library(GGally)
library(ROCR)
library(Information)
library(scales)

# Clear all the Environment variables
rm(list = ls())

# Dataset Loading
emp_survey <- read.csv('employee_survey_data.csv', stringsAsFactors = F)
mgr_survey <- read.csv('manager_survey_data.csv', stringsAsFactors = F)
general_data <- read.csv('general_data.csv', stringsAsFactors = F)
out_time <- read.csv('out_time.csv', stringsAsFactors = F)
in_time <- read.csv('in_time.csv', stringsAsFactors = F)
# Check all the datasets have unique & same emp ids
length(unique(mgr_survey$EmployeeID)) # 4410 Primary key is EmployeeId
length(unique(general_data$EmployeeID)) # 4410 Primary key is EmployeeId
length(unique(in_time$X)) # 4410 unique employee ids
length(unique(out_time$X)) # 4410 employeeID in the 1st column is the key
length(unique(emp_survey$EmployeeID)) # 4410 Primary key is EmployeeId

# Check the structure of the datasets
str(emp_survey)   	# 4410 records and 4 columns
str(mgr_survey)   	# 4410 records and 3 columns
str(general_data) 	# 4410 records and 24 columns
str(out_time)  		# 4410 records and 262 columns
str(in_time)  		# 4410 records and 262 columns


# DERIVE METRICS CREATION ####
# Derive New Metrics from in_time & out_time datasets
# 1. tot_leaves: Total leaves by employees
# 2. avg_hours: average working hours of employees 
# 3. leaves_by_month

# ASSUMPTIONS: ####
# 1. Missing / NA values in in_time dataset is absence (1) & Non missing values as presence (0)
leave_df <- as.data.frame(ifelse(is.na(in_time[,-1]), 1, 0))

# 2. Difference of out_time & in_time is considered as Working Hours of a day
# 3. Public Holiday: day where employees does not have in_time data
# REMOVE PUBLIC HOLIDAYS FROM LEAVE DATAFRAME
leave_df <- leave_df[, which(colSums(leave_df)!=4410)]

# DERIVE NEW METRICS - LEAVE PATTERN
# create emp_leaves df for leave pattern analysis
# total leaves calculation
emp_leaves <- data.frame(EmployeeID = in_time$X, tot_leaves = (rowSums(leave_df))) 
names(leave_df)
# dates_matrix df for leave analysis
dates_matrix <- data.frame(dates=as.Date(gsub('X','', names(leave_df)), format = '%Y.%m.%d'))
dates_matrix$month <- months(dates_matrix$dates, abbreviate = T)
# LEAVE COUNT ANALYSIS ####
leave_by <- 'month'
for(i in leave_by){
  colnames(leave_df) <- dates_matrix[, c(i)]
  calc_leaves <- as.data.frame(sapply(unique(colnames(leave_df)), 
                                      function(x) rowSums(leave_df[, colnames(leave_df)==x, drop=F], na.rm = T)))
  emp_leaves <- cbind(emp_leaves, calc_leaves)
}
head(emp_leaves) # To view employee leaves
#  WORKING HOURS CALCULATIONS ####
# Lets verify if both the in_time & out_time datasets are similar.
identical(colnames(in_time$X), colnames(out_time$X)) 

# EXCLUDE PUBLIC HOLIDAYS FROM THE ORIGINAL DF
out_time <- out_time[, which(sapply(out_time, function(x) sum(is.na(x))) != 4410)]
in_time <- in_time[, which(sapply(in_time, function(x) sum(is.na(x))) != 4410)]


# Coverting date time format for easy calculation of working hours
out_time[,-1] <- sapply(out_time[,-1], ymd_hms)
in_time[,-1] <- sapply(in_time[,-1], ymd_hms)
work_hrs <- (out_time[,-1] - in_time[,-1])/3600 # Taken 60*60 to convert secs into hrs

# Employee Working Hours
# AVG WORKING HRS CALCULATION
emp_hrs <- data.frame(EmployeeID=in_time$X, 
                      Avg_hrs = round(rowMeans(work_hrs, na.rm = T),2))
head(emp_hrs)

# DATA PREPARATION ####
# GENERAL DATA 
# remove columns with single values 
general_data$EmployeeCount <- NULL
general_data$Over18 <- NULL
general_data$StandardHours <- NULL
str(general_data) # To view general dataset after removing unwanted columns

# MISSING VALUES ####
sum(is.na(general_data)) 
# TotalWorkingYears:9 & NumCompaniesWorked:19
# Missing value imputation using WOE analysis


# IDENTIFYING OUTLIERS ####
sapply(general_data[,c(1,5:6,10,13:21)], function(x) boxplot.stats(x)$out)
# columns with outlier values: MonthlyIncome, NumCompaniesWorked,
# StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany,
# YearsSinceLastPromotion, YearsWithCurrManager. These will be treated later using
# WOE analysis


#  MISSING VALUE TREATMENT  ####
sum(is.na(mgr_survey)) # 0 missing values
sum(is.na(general_data)) # 28 missing values
sum(is.na(emp_survey)) # 83 missing values 

sapply(emp_survey, function(x) sum(is.na(x))) # (worklife:38, env:25, jobs:20)
# REMOVE MISSING VALUES ####
emp_survey <- na.omit(emp_survey)

# MERGE ALL FILES INTO ONE SINGLE FILE  ####
hr <- merge(general_data, emp_survey, by="EmployeeID", all = F)
hr <- merge(hr, mgr_survey, by = "EmployeeID", all = F)
hr <- merge(hr, emp_hrs, by = "EmployeeID", all = F)
hr <- merge(hr, emp_leaves, by = "EmployeeID", all = F)
str(hr) # Display the structure finally

#  EXPLORATORY DATA ANALYSIS (EDA) & DATA PREPARATION   ####
sapply(hr[,-c(1:2,6,13:15,17:21, 27:40)], table) # Verifying all categorical variables
# MonthlyIncome, Age, Distance From Home, PercentSalaryHike, NumCompaniesWorked,
# TotalWorkingYears,TrainingTimesLastYear,YearsAtCompany,YearsSinceLastPromotion,YearsWithCurrManager
# Avg_hrs, tot_leaves & all the monthly leaves are continuous variables 
# Here, Attrition is considered as target variable

sum(duplicated(hr)) # check duplication

## check missing values treatment
sapply(hr, function(x){sum(is.na(x))})
sum(is.na(hr))

# Barcharts for categorical features
# make a copy of the combined data set
hr_master <- hr 

# DATA PREPARATION : CONVERTING CATEGORICAL VARIABLES/ DATA FROM NUMERIC TO TEXT VALUES ####
# THIS IS AS PER DEFINED IN DATA DICTIONARY PROVIDED

hr_master$WorkLifeBalance <- ifelse(hr_master$WorkLifeBalance==1, "Bad",
                             ifelse(hr_master$WorkLifeBalance==2, "Good", 
                             ifelse(hr_master$WorkLifeBalance==3, "Better", "Best")))
hr_master$JobSatisfaction <- ifelse(hr_master$JobSatisfaction==1, "Low",
                             ifelse(hr_master$JobSatisfaction==2, "Medium", 
                             ifelse(hr_master$JobSatisfaction==3, "High", "Very High")))			     
hr_master$JobInvolvement <- ifelse(hr_master$JobInvolvement==1, "Low",
                            ifelse(hr_master$JobInvolvement==2, "Medium", 
                            ifelse(hr_master$JobInvolvement==3, "High", "Very High")))
hr_master$EnvironmentSatisfaction <- ifelse(hr_master$EnvironmentSatisfaction==1, "Low",
                                     ifelse(hr_master$EnvironmentSatisfaction==2, "Medium", 
                                     ifelse(hr_master$EnvironmentSatisfaction==3, "High", "Very High")))
hr_master$Education <- ifelse(hr_master$Education==1, "Below College",
                       ifelse(hr_master$Education==2, "College", 
                       ifelse(hr_master$Education==3, "Bachelor",
                       ifelse(hr_master$Education==4, "Master", "Doctors"))))
hr_master$PerformanceRating <- ifelse(hr_master$PerformanceRating==1, "Low",
                               ifelse(hr_master$PerformanceRating==2, "Good", 
                               ifelse(hr_master$PerformanceRating==3, "Excellent", "Outstanding")))


plot_fun <- function(df, col1, Attrition, col3){
ggplot(df,aes(x=col1,fill=Attrition)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), position = 'dodge') + 
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1,position=position_dodge(.9), size=2.5) +
  labs(x='', y='', title = col3)
}

# PLOTTING THE GRAPHS ####
P1 <- plot_fun(hr_master,hr_master$Education,hr_master$Attrition,"EDUCATION")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))
  
P2 <- plot_fun(hr_master,hr_master$BusinessTravel,hr_master$Attrition,"BUSINESS TRAVEL")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P3 <- plot_fun(hr_master,hr_master$Department,hr_master$Attrition,"DEPARTMENT")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P4 <- plot_fun(hr_master,hr_master$EducationField,hr_master$Attrition,"EDUCATION FIELD")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P5 <- plot_fun(hr_master,hr_master$Gender,hr_master$Attrition,"GENDER")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P6 <- plot_fun(hr_master,hr_master$MaritalStatus,hr_master$Attrition,"MARITAL STATUS")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P7 <- plot_fun(hr_master,hr_master$EnvironmentSatisfaction,hr_master$Attrition,"ENV SATISFACTION")  + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P8 <- plot_fun(hr_master,hr_master$PerformanceRating,hr_master$Attrition,"Performance Rating")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P9 <- plot_fun(hr_master,hr_master$JobLevel,hr_master$Attrition,"Job Level")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P10 <- plot_fun(hr_master,hr_master$JobInvolvement,hr_master$Attrition,"Job Involvement")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P11 <- plot_fun(hr_master,hr_master$WorkLifeBalance,hr_master$Attrition,"Work Life Balance")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P12 <- plot_fun(hr_master,hr_master$JobSatisfaction,hr_master$Attrition,"Job Satisfaction")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P13 <- plot_fun(hr_master,hr_master$PercentSalaryHike,hr_master$Attrition,"Percent Salary Hike")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P14 <- plot_fun(hr_master,hr_master$YearsSinceLastPromotion,hr_master$Attrition,"Years Since last promotion")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P15 <- plot_fun(hr_master,hr_master$YearsWithCurrManager,hr_master$Attrition,"Years with Current Manager")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P16 <- plot_fun(hr_master,hr_master$NumCompaniesWorked,hr_master$Attrition,"Number of Companies Worked")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P17 <- plot_fun(hr_master,hr_master$Age,hr_master$Attrition,"Age")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P18 <- plot_fun(hr_master,hr_master$TotalWorkingYears,hr_master$Attrition,"Total Working years")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P19 <- plot_fun(hr_master,hr_master$YearsAtCompany,hr_master$Attrition,"Years at company")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P20 <- plot_fun(hr_master,hr_master$DistanceFromHome,hr_master$Attrition,"Distance from home")  +  
theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
theme(axis.text.x=element_text(angle = 60))

P21 <- plot_fun(hr_master,hr_master$tot_leaves,hr_master$Attrition,"Total Leaves")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P22 <- plot_fun(hr_master,hr_master$StockOptionLevel,hr_master$Attrition,"Stock Option Level")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P23 <- plot_fun(hr_master,hr_master$JobRole,hr_master$Attrition,"Job Role")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P24 <- plot_fun(hr_master,hr_master$TrainingTimesLastYear,hr_master$Attrition,"Training times last year")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

#install.packages("gridExtra")
library(gridExtra)

grid.arrange(P7,P8,P9,P10,P11,P12,ncol=3,nrow=2)
# GRID PLOT ANALYSIS - I: ####
# 1. Employees with high Job Involvement and low Environment Satisfaction are ones whose chances are most for attrition
# 2. Employees with Low or High Job Satisfaction and Job level 1 and 2 are ones whose chances are most for attrition
# 3. Employees with excellent performance rating and have better work life balance are more prone for attrition

grid.arrange(P21,P22,P23,P24,ncol=2,nrow=2)
# GRID PLOT ANALYSIS - II: ####
# 1. Laboratory Technicians, Sales Executive & Research Scientists are the ones who quitted more
# 2. Employees with 0 and 1 stock option levels are the ones most prone to attrition
# 3. Employees without training in past 2 to 3 years are the ones most accustomed to attrition mostly
# 4. No specific major trends for total leaves w.r.t. attritions

  
grid.arrange(P17,P18,P19,P20,ncol=2,nrow=2)
# GRID PLOT ANALYSIS - III: ####
# 1. Single Employees in late 20s to early 30s who have worked between 0-10 years in company are more accustomed to attrition 
# 2. Employees staying within 0-2 kms has high attrition rate & Employees staying within 0-10 kms constitutes most of the attrition population
# 3. Employees Age between 25 to 35 yrs are more accustomed to attrition

grid.arrange(P1,P2,P3,P4,P5,P6,ncol=3,nrow=2)
# GRID PLOT ANALYSIS - IV: ####
# 1. Bachelors Degree Education employees are more accustomed to attrition
# 2. Those Employees who travel rarely and have masters and graduate degrees are more accustomed to attrition
# 3. R&D employees are accustomed to attrition mostly from Life Sciences & Medical education level. 
# 4. Male employees are more accustomed to attrition than female employees.
# 5. Single employees are more accustomed to attrition more than Divorced and almost double times the Married ones.

grid.arrange(P13,P14,P15,P16,ncol=2,nrow=2)
# GRID PLOT ANALYSIS - V: ####
# 1. Employees with salary hike of 11-14 percentages are more accustomed to attrition
# 2. Employees having worked in 1 to 2 companies and are with current manager for 0-1 years are most likely to quit
# 3. Employees recently promoted this year or past year (mainly promoted with past 0-2 yrs) are most likely to leave the company

rm(hr_master) # remove hr_master from workspace


#   Plots for numeric variables   ####
# Average hours scatter plot
ggplot(hr, aes(EmployeeID, Avg_hrs, color=Attrition))+
  geom_point(position = 'jitter', alpha=0.3)+
  labs(title='Average hours worked by Employee ID', 
       y='Average Hours Worked',alpha='', color='')+
  theme(axis.title.x = element_blank())

# This graph shows Maximum employees are working in 6-8 hours are happy and there 
# attrition is "No"

## binned average working hours plot
df_hrs <- hr[,c(1,3,27)]
df_hrs['Avg_hrs_cut'] <- cut(df_hrs$Avg_hrs, breaks = c(5.9, 7, 8, 9, 10, 11.05), 
                             labels = c('6-7', '7-8', '8-9', '9-10', '10-11'))

# attrition by average working hours plot
P25 <- plot_fun(df_hrs,factor(df_hrs$Avg_hrs_cut),df_hrs$Attrition,"Average working hours")  +  
  theme(axis.text=element_text(size=8), axis.title=element_text(size=5)) +
  theme(axis.text.x=element_text(angle = 60))

P25
# Findings: Clearly employees averaging over 7 hours have quit
# The proportion of people leaving is more during 8-9, 9-10, and 10-11 working
# hours slot.

# Leave pattern: Month wise ####
df3 <- hr[c(1,3,28:40)]
df4 <- df3 %>% 
  gather(month, leave_df, Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec) %>% 
  subset(select=c(EmployeeID, Attrition, month, leave_df)) %>% 
  group_by(EmployeeID, month, Attrition) %>% 
  summarise(leave_count = sum(leave_df)) %>% 
  arrange(desc(leave_count))

ggplot(df4, aes(month, leave_count, fill=Attrition))+
  geom_bar(stat='identity', position='dodge') +
  labs(title='Attrition by average leaves Month wise',
       x='', y='leaves taken')+
  theme_gray()
# Findings: No specific patterns

## Monthly Income Plot (before outlier treatment) ##
box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")
# Boxplots of numeric variables relative to attrition
plot_grid(ggplot(hr, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr, aes(x=Attrition,y=Avg_hrs, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Outlier Treatment 
#   Wieght of Evidence (WOE) analysis ####
indices <- which(colnames(hr) %in% 
                   c('Age','DistanceFromHome',
                     'PercentSalaryHike','MonthlyIncome', 
                     'NumCompaniesWorked',
                     'Attrition',
                     'TotalWorkingYears','TrainingTimesLastYear','YearsAtCompany',
                     'YearsSinceLastPromotion','YearsWithCurrManager',
                     'Avg_hrs','tot_leaves','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'
                   ))
hr_woe <- hr[, indices]
## converting target variable Attrition from No/Yes character to factorwith
## levels 0/1 ##
hr_woe$Attrition <- ifelse(hr_woe$Attrition=='Yes', 0, 1)
# create Information Value (IV) Table
iv <- create_infotables(hr_woe, y = "Attrition")
# summary of IV table
iv$Summary
# Variable           IV
# 11                 Avg_hrs 0.3758377295
# 6        TotalWorkingYears 0.3721659998
# 8           YearsAtCompany 0.3454135173
# 1                      Age 0.3034228102
# 10    YearsWithCurrManager 0.3021106316
# 4       NumCompaniesWorked 0.1109778374
# 9  YearsSinceLastPromotion 0.0518842496
# 12              tot_leaves 0.0329163787
# 3            MonthlyIncome 0.0278117396
# 7    TrainingTimesLastYear 0.0250885883
# 2         DistanceFromHome 0.0205046040
# 22                     Oct 0.0185122106
# 5        PercentSalaryHike 0.0114244814
# 18                     Jun 0.0085987939
# 14                     Feb 0.0079492283
# 19                     Jul 0.0074734775
# 20                     Aug 0.0056392016
# 13                     Jan 0.0048648058
# 23                     Nov 0.0047493286
# 17                     May 0.0032074087
# 24                     Dec 0.0022068382
# 16                     Apr 0.0015444323
# 15                     Mar 0.0007120778
# 21                     Sep 0.0003981921
# IV > 0.3 => strong predictive power
# IV [0.1, 0.3] => medium predictive power, 
# IV < 0.1 => weak predictive power
# So, we will use WOE variable transformation for:


## 1. TotalWorkingYears 
hr_woe$TotalWorkingYears <- ifelse(is.na(hr_woe$TotalWorkingYears), iv$Tables[['TotalWorkingYears']]['WOE'][[1]][1], 
                                   ifelse(hr_woe$TotalWorkingYears >=0 & hr_woe$TotalWorkingYears<=2, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][2], 
                                          ifelse(hr_woe$TotalWorkingYears >=3 & hr_woe$TotalWorkingYears<=4, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][3],
                                                 ifelse(hr_woe$TotalWorkingYears==5, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][4],
                                                        ifelse(hr_woe$TotalWorkingYears >=6 & hr_woe$TotalWorkingYears<=7, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][5],
                                                               ifelse(hr_woe$TotalWorkingYears >=8 & hr_woe$TotalWorkingYears<=9, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][6],
                                                                      ifelse(hr_woe$TotalWorkingYears >=10 & hr_woe$TotalWorkingYears<=12, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][7],
                                                                             ifelse(hr_woe$TotalWorkingYears >=13 & hr_woe$TotalWorkingYears<=16, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][8],
                                                                                    ifelse(hr_woe$TotalWorkingYears >=17 & hr_woe$TotalWorkingYears<=22, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][9],
                                                                                           ifelse(hr_woe$TotalWorkingYears >=23 & hr_woe$TotalWorkingYears<=40, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][10], NA))))))))))

## 2. YearsAtCompany 
hr_woe$YearsAtCompany <- ifelse(hr_woe$YearsAtCompany ==0, iv$Tables[['YearsAtCompany']]['WOE'][[1]][1], 
                         ifelse(hr_woe$YearsAtCompany ==1, iv$Tables[['YearsAtCompany']]['WOE'][[1]][2],
                         ifelse(hr_woe$YearsAtCompany==2, iv$Tables[['YearsAtCompany']]['WOE'][[1]][3],
                         ifelse(hr_woe$YearsAtCompany >=3 & hr_woe$YearsAtCompany<=4, iv$Tables[['YearsAtCompany']]['WOE'][[1]][4],
                         ifelse(hr_woe$YearsAtCompany >=5 & hr_woe$YearsAtCompany<=6, iv$Tables[['YearsAtCompany']]['WOE'][[1]][5],
                         ifelse(hr_woe$YearsAtCompany >=7 & hr_woe$YearsAtCompany<=8, iv$Tables[['YearsAtCompany']]['WOE'][[1]][6],
                         ifelse(hr_woe$YearsAtCompany ==9, iv$Tables[['YearsAtCompany']]['WOE'][[1]][7],
                         ifelse(hr_woe$YearsAtCompany >=10 & hr_woe$YearsAtCompany<=14, iv$Tables[['YearsAtCompany']]['WOE'][[1]][8],
                         ifelse(hr_woe$YearsAtCompany >=15 & hr_woe$YearsAtCompany<=40, iv$Tables[['YearsAtCompany']]['WOE'][[1]][9], NA)))))))))

## 3. YearsWithCurrManager & 
hr_woe$YearsWithCurrManager <- ifelse(hr_woe$YearsWithCurrManager ==0, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][1], 
                               ifelse(hr_woe$YearsWithCurrManager ==1, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][2],
                               ifelse(hr_woe$YearsWithCurrManager==2, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][3],
                               ifelse(hr_woe$YearsWithCurrManager ==3, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][4],
                               ifelse(hr_woe$YearsWithCurrManager >=4 & hr_woe$YearsWithCurrManager<=6, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][5],
                               ifelse(hr_woe$YearsWithCurrManager >=7 & hr_woe$YearsWithCurrManager<=8, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][6],
                               ifelse(hr_woe$YearsWithCurrManager >=9 & hr_woe$YearsWithCurrManager<=17, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][7], NA)))))))

#4. NumCompaniesWorked
hr_woe$NumCompaniesWorked <- ifelse(is.na(hr_woe$NumCompaniesWorked), iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][1], 
                             ifelse(hr_woe$NumCompaniesWorked ==0, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][2], 
                             ifelse(hr_woe$NumCompaniesWorked ==1, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][3],
                             ifelse(hr_woe$NumCompaniesWorked==2, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][4],
                             ifelse(hr_woe$NumCompaniesWorked ==3, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][5],
                             ifelse(hr_woe$NumCompaniesWorked ==4, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][6],
                             ifelse(hr_woe$NumCompaniesWorked >=5 & hr_woe$NumCompaniesWorked<=6, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][7],
                             ifelse(hr_woe$NumCompaniesWorked >=7 & hr_woe$NumCompaniesWorked<=9, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][8], NA))))))))

#5. Avg_hrs 
hr_woe$Avg_hrs <- ifelse(hr_woe$Avg_hrs >=5.95 & hr_woe$Avg_hrs <=6.19, iv$Tables[['Avg_hrs']]['WOE'][[1]][1], 
              ifelse(hr_woe$Avg_hrs >=6.2 & hr_woe$Avg_hrs<=6.5, iv$Tables[['Avg_hrs']]['WOE'][[1]][2], 
              ifelse(hr_woe$Avg_hrs >=6.51 & hr_woe$Avg_hrs<=6.79, iv$Tables[['Avg_hrs']]['WOE'][[1]][3],
              ifelse(hr_woe$Avg_hrs>=6.8 & hr_woe$Avg_hrs<=7.1, iv$Tables[['Avg_hrs']]['WOE'][[1]][4],
              ifelse(hr_woe$Avg_hrs >=7.11 & hr_woe$Avg_hrs<=7.4, iv$Tables[['Avg_hrs']]['WOE'][[1]][5],
              ifelse(hr_woe$Avg_hrs >=7.41 & hr_woe$Avg_hrs<=7.69, iv$Tables[['Avg_hrs']]['WOE'][[1]][6],
              ifelse(hr_woe$Avg_hrs >=7.7 & hr_woe$Avg_hrs<=7.99, iv$Tables[['Avg_hrs']]['WOE'][[1]][7],
              ifelse(hr_woe$Avg_hrs >=8 & hr_woe$Avg_hrs<=8.87, iv$Tables[['Avg_hrs']]['WOE'][[1]][8],
              ifelse(hr_woe$Avg_hrs >=8.88 & hr_woe$Avg_hrs<=9.99, iv$Tables[['Avg_hrs']]['WOE'][[1]][9],
              ifelse(hr_woe$Avg_hrs >=10 & hr_woe$Avg_hrs<=11.03, iv$Tables[['Avg_hrs']]['WOE'][[1]][10], NA))))))))))

#6. Age 
hr_woe$Age <-     ifelse(hr_woe$Age >=18 & hr_woe$Age <=25, iv$Tables[['Age']]['WOE'][[1]][1],
              ifelse(hr_woe$Age >=26 & hr_woe$Age<=28, iv$Tables[['Age']]['WOE'][[1]][2],
              ifelse(hr_woe$Age >=29 & hr_woe$Age<=30, iv$Tables[['Age']]['WOE'][[1]][3],
              ifelse(hr_woe$Age>=31 & hr_woe$Age<=33, iv$Tables[['Age']]['WOE'][[1]][4],
              ifelse(hr_woe$Age >=34 & hr_woe$Age<=35, iv$Tables[['Age']]['WOE'][[1]][5],
              ifelse(hr_woe$Age >=36 & hr_woe$Age<=37, iv$Tables[['Age']]['WOE'][[1]][6],
              ifelse(hr_woe$Age >=38 & hr_woe$Age<=40, iv$Tables[['Age']]['WOE'][[1]][7],
              ifelse(hr_woe$Age >=41 & hr_woe$Age<=44, iv$Tables[['Age']]['WOE'][[1]][8],
              ifelse(hr_woe$Age >=45 & hr_woe$Age<=49, iv$Tables[['Age']]['WOE'][[1]][9],
              ifelse(hr_woe$Age >=50 & hr_woe$Age<=60, iv$Tables[['Age']]['WOE'][[1]][10], NA))))))))))

hr$TotalWorkingYears <- hr_woe$TotalWorkingYears
hr$YearsAtCompany <- hr_woe$YearsAtCompany
hr$YearsWithCurrManager <- hr_woe$YearsWithCurrManager
hr$NumCompaniesWorked <- hr_woe$NumCompaniesWorked
hr$Avg_hrs <- hr_woe$Avg_hrs
hr$Age <- hr_woe$Age

# there are outliers in monthly income & Avg_hrs
# numerical dataset
# excluding woe treated variables & categorical variables
hr_num <- hr[,-c(1, 2:5, 7:12,14, 16:17,19,21, 22:27)] 
sapply(hr_num, function(x) length(boxplot.stats(x)$out))
# we have outliers in the following features:
# 1.MonthlyIncome 2. TrainingTimesLastYear
# 3. YearsSinceLastPromotion 4. tot_leaves
hr_num$Attrition <- ifelse(hr$Attrition=='Yes',0,1)

# Outlier treatment for: MonthlyIncome ####
## binning monthly income
hr$MonthlyIncome <- cut(hr$MonthlyIncome, breaks = c(10000, 40000, 70000, 100000, 150000, 200000), 
                        labels = c('10-40k','40-70k','70-100k','100-150k', '150-200k'))


# Monthly Income bar plot
ggplot(hr, aes(MonthlyIncome, fill=Attrition))+
  geom_bar(position = 'dodge', aes(y = ((..count..)/sum(..count..)))) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1, size=3, position=position_dodge(.9)) +
  scale_y_continuous(labels =  scales::percent) +
  labs(x='', fill='', y='', title='Attrition by Monthly Income')

#Findings: Employees earning less than 70,000 per month mostly quit and
#constitute ~75% of attrition population Top earners and working little above
#standard working hours are more likely to attrition

corrplot(cor(hr[,-c(1,3:5,8:9,11:13)]), tl.cex = 0.5, mar = c(1,0,0,0)) #c(bottom, left, top, right)
# Findings: Performance Rating & PercentageSalaryHike is highly correlated so does yearswithchurnmanager
  # & yearsatcompany, Also totatl working years is directly correlated with age

####################################
##    Feature standardisation   ## 
###################################
# Normalising continuous features#
hr[,-c(1,3:5,8:9,11:13)] <- as.data.frame(scale(hr[,-c(1,3:5,8:9,11:13)]))

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
hr$Attrition<- ifelse(hr$Attrition=="Yes",1,0)

# Checking attrition rate of prospect customer
Attrition <- sum(hr$Attrition)/nrow(hr)
Attrition # 16.20% attrition rate which is near to 15% as mentioned in the case study


# creating a dataframe of categorical features
hr_chr<- hr[,c(4:5,8,11:13)]
# converting categorical attributes to factor
hr_fact<- data.frame(sapply(hr_chr, function(x) factor(x)))
str(hr_fact)
# creating dummy variables for factor attributes
dummies<- data.frame(sapply(hr_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =hr_fact))[,-1]))
# for gender male is 1 and female is 0
hr$Gender <- ifelse(hr$Gender=='Male', 1, 0)

# Final dataset ####
hra_final<- cbind(hr[,-c(1,4:5,8, 11:13)],dummies) # removed the ID field: EmployeeID
View(hra_final) # 4327 obs. of  70 variables

# splitting the data between train and test ####
set.seed(4327) # for reproducibility

ntrain = sample.split(hra_final$Attrition, SplitRatio = 0.7) #splitting indices
train = hra_final[ntrain, ] # train data consisting of 70% of original obs.
test = hra_final[!ntrain, ] # test data consisting of 30% of original obs.

########################################################################
# Logistic Regression: 
#Initial model
model = glm(Attrition ~ ., data = train, family = "binomial")
summary(model) #AIC 2061.6....69 coeff..nullDev 2684.5 ...resDev 1933.6

# Stepwise selection
model2 <- stepAIC(model, direction="both")
summary(model2)
##################################################
## Removing Multicollinearity through VIF check #
#################################################
vif(model2)

# removing Jun due to high p-value
model3 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + Avg_hrs + tot_leaves + 
                Mar + Apr + Sep + Oct + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + EducationField.xOther + JobRole.xManager + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Representative + MaritalStatus.xSingle + MonthlyIncome.x150.200k + 
                MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model3)
vif(model3)

# removing Mar due to high p-value
model4 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + Avg_hrs + tot_leaves + 
                Apr + Sep + Oct + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + EducationField.xOther + JobRole.xManager + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Representative + MaritalStatus.xSingle + MonthlyIncome.x150.200k + 
                MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model4)
vif(model4)

# removing tot_leaves with high p-value
model5 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + Avg_hrs +  
                Apr + Sep + Oct + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + EducationField.xOther + JobRole.xManager + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Representative + MaritalStatus.xSingle + MonthlyIncome.x150.200k + 
                MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model5)
vif(model5)

# removing Sep with high p-value
model6 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + Avg_hrs +  
                Apr + Oct + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + EducationField.xOther + JobRole.xManager + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Representative + MaritalStatus.xSingle + MonthlyIncome.x150.200k + 
                MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model6)
vif(model6)

# removing Education with vif high p-value
model7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + Avg_hrs +  
                Apr + Oct + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + EducationField.xOther + JobRole.xManager + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Representative + MaritalStatus.xSingle + MonthlyIncome.x150.200k + 
                MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model7)
vif(model7)

# removing JobInvolvement with high p-value
model8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Avg_hrs +  
                Apr + Oct + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + EducationField.xOther + JobRole.xManager + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Representative + MaritalStatus.xSingle + MonthlyIncome.x150.200k + 
                MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model8)
vif(model8)

# removing JobRole.xResearch.Director with high p-value
model9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Avg_hrs +  
                Apr + Oct + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + EducationField.xOther + JobRole.xManager + 
                JobRole.xManufacturing.Director + 
                JobRole.xSales.Representative + MaritalStatus.xSingle + MonthlyIncome.x150.200k + 
                MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model9)
vif(model9)

# removing MonthlyIncome.x150.200k with high p-value
model10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs +  
                 Apr + Oct + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xOther + JobRole.xManager + 
                 JobRole.xManufacturing.Director + 
                 JobRole.xSales.Representative + MaritalStatus.xSingle + 
                 MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model10)
vif(model10)

# removing JobRole.xSales.Representative with p-value high
model11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs +  
                 Apr + Oct + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xOther + JobRole.xManager + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + 
                 MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model11)
vif(model11)

# removing Apr with high p-value
model12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs +  
                 Oct + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xOther + JobRole.xManager + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + 
                 MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model12)
vif(model12)

# removing TrainingTimesLastYear with p-value high
model13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs +  
                 Oct + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xOther + JobRole.xManager + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + 
                 MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model13)
vif(model13)

# removing JobRole.xManager with p-value high
model14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs +  
                 Oct + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + EducationField.xOther + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + 
                 MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model14)
vif(model14)

# removing EducationField.xOther with high p-value
model15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs +  
                 Oct + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + 
                 MonthlyIncome.x40.70k, family = "binomial", data = train)
summary(model15)
vif(model15)

# removing MonthlyIncome.x40.70k with p-value high
model16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs +  
                 Oct + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model16)
vif(model16)

# removing Oct with high p-value
model17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model17)
vif(model17)

# removing NumCompaniesWorked with p-value high
model18 <- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model18)
vif(model18)

# removing BusinessTravel.xTravel_Rarely with p-value high
model19 <- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model19)

# removing JobRole.xManufacturing.Director with p-value high
model20 <- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Avg_hrs + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + 
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model20)

# Final Model: with 12 significant variables in the model ####
final_model<- model20

### MODEL EVALUATION ####

# Predicted Probabilities of Attrition
test_predicted = predict(final_model, type = "response", 
                         newdata = test)
summary(test_predicted) # to view the predicted values

test$probability <- test_predicted
# View(test)

# Use the probability cutoff as 60% ################
test_predicted_attrition <- factor(ifelse(test_predicted >= 0.60, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition, test_predicted_attrition) 

#install.packages('e1071')
library(e1071)
tst_confusionmatrix <- confusionMatrix(test_predicted_attrition, test_actual_attrition)
tst_confusionmatrix
# Found 87% Accuracy, 99% Sensitivity & 26% Specificity

# Use the probability cutoff as 50% ###################
test_predicted_attrition <- factor(ifelse(test_predicted >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition, test_predicted_attrition) 

tst_confusionmatrix <- confusionMatrix(test_predicted_attrition, test_actual_attrition)
tst_confusionmatrix # We were facing some issue here, calculating manually now
# Below manual calculation is done, in case confusion matrix function doesn't work properly
# Accuracy
(1069+67)/(1069+67+143+19)
# Sensitivity
(1069)/(1069+19)
# Specificity
(67)/(143+67)
# Found 87% accuracy, 98% sensitivity & 32% Specificity


# Use the probability cutoff as 40% ###################
test_predicted_attrition <- factor(ifelse(test_predicted >= 0.40, "Yes", "No"))
table(test_actual_attrition, test_predicted_attrition) 
tst_confusionmatrix <- confusionMatrix(test_predicted_attrition, test_actual_attrition, positive = "Yes")
tst_confusionmatrix
# Found 87% accuracy, 38% sensitivity & 96% Specificity


## Calculate the Optimal Cut-Off value ####
populateCMdata <- function(cutoff)  {
  predicted_attrition <- factor(ifelse(test_predicted >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) #transpose the matrix
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_predicted)

# Matrix for model performance data
cutoff.data = seq(.01,.80,length=100)
cmdata = matrix(0,100,3) # matrix to hold model performance data

for(i in 1:100){
  cmdata[i,] = populateCMdata(cutoff.data[i])
} 
(cutoff <- cutoff.data[which(abs(cmdata[,1]-cmdata[,2]) < 0.01)])
plot(cutoff.data, cmdata[,1], xlab="Cutoff", ylab="Value", cex.lab=1,
     cex.axis=1, ylim=c(0,1), type="l", lwd=2, axes=FALSE, col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff.data,cmdata[,2],col="green",lwd=2)
lines(cutoff.data,cmdata[,3],col=4,lwd=2)
box()
legend(0.10,0.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))
text(0.22, 0.35, labels=sprintf("cutoff value: %0.7f", cutoff))

# Cut-off value of 0.1536364 chosen for final model
test_cutoff_attrition <- factor(ifelse(test_predicted >= 0.1536364, "Yes", "No"))
conf.final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc <- conf.final$overall[1]
sens <- conf.final$byClass[1]
spec <- conf.final$byClass[2]
acc # 76%
sens # 74%
spec # 76%
#View(test)


### KS -statistic - using Test Data ######
test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)
#on testing  data
pred.object.test<- prediction(test_cutoff_attrition, test_actual_attrition)
performance.measures.test<- performance(pred.object.test, measure = "tpr", x.measure = "fpr")


##
ks.table.test <- attr(performance.measures.test, "y.values")[[1]] - 
  (attr(performance.measures.test, "x.values")[[1]])
max(ks.table.test) # 0.5688

######### KS PLOT #########
library(InformationValue)
ks_plot(test_actual_attrition, test_cutoff_attrition) # Gain chart plot

########## END OF ASSIGNMENT ########

