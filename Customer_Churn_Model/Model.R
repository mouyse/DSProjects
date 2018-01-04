####  Clear the environment by removing all the pre-loaded variables----
#===============================-
rm(list=ls())

### Loading required packages----
library(ggplot2)
library("magrittr")  # For using pipeline character
library(dplyr) # For using select
# Please install the library if missing
library(sqldf)

### Load the dataset----
#===============================-
lc <- read.csv("loan.csv",stringsAsFactors = F)

####  Data Cleansing----
#===============================-

####  Data Cleansing - Removing unnecessary columns----
# Get rid of all columns having only 1 unique value as it can't help in our analysis.
lc <- lc[vapply(lc, function(x) length(unique(x)) > 1, logical(1L))]

####  Data Cleansing -  Remove columns with little information----
# 1.emp_title not needed here
# 2.url not needed
# 3.desc not needed
# 4.title not needed
# 5.zip_code not needed for this level of analysis
# 6.id and member_id are all unique,which is a bit misleading.every record is a single customer. So removing this as well.
# 7.collections_12_mths_ex_med,chargeoff_within_12_mths and tax_liens contains only 0 and NAs. Thus, removing it.
# 8.last_credit_pull_d and next_pymnt_d not needed fir this kind of analysis
# 9.Ignoring the columns: mths_since_last_delinq,mths_since_last_record. Because more than 80% of its values are missing.
# 10. Ignoring all the other columns which are not mentioned here as they were not important for the kind of analysis
drop_columns <- c("emp_title","url","desc","title","zip_code","member_id","id",
                  "collections_12_mths_ex_med","chargeoff_within_12_mths","tax_liens","last_credit_pull_d","next_pymnt_d",
                  "mths_since_last_delinq","mths_since_last_record","earliest_cr_line","addr_state",
                  "out_prncp","out_prncp_inv","recoveries","collection_recovery_fee","last_pymnt_amnt",
                  "next_pymnt_d","installment","revol_bal","verification_status",
                  "delinq_2yrs","inq_last_6mths","funded_amnt_inv",
                  "open_acc","total_acc",
                  "total_pymnt","total_pymnt_inv","total_rec_int","total_rec_late_fee","total_rec_prncp",
                  "last_pymnt_d","pub_rec_bankruptcies","funded_amnt","dti","home_ownership","annual_inc","pub_rec"
                  )
lc <- lc[ , !(names(lc) %in% drop_columns)]

####  Data Cleansing - Remove duplicate records----
lc <- lc[!duplicated(lc),]

# Replacing n/a(string) in emp_length with NA
lc[which(lc$emp_length == "n/a"),"emp_length"] <- NA

####  Business Driven Derived Metrics Analysis----
# 1. default
####  Assumption----
# Considering Fully paid and Current as 0 and Charged Off as 1 for newly derived column "default"
# condidered as default if not current or fully paid
lc$default = ifelse(lc$loan_status == "Fully Paid", 0, 
             ifelse(lc$loan_status == "Current", 0, 1))  
lc$default = as.numeric(lc$default)
# 2.emp_length
# Deriving emplyee length in a proper format for further analysis
lc$emp_length = ifelse(lc$emp_length == "< 1 year", 0, 
    ifelse(lc$emp_length == "1 year", 1, 
    ifelse(lc$emp_length == "10+ years", 10, 
    ifelse(lc$emp_length == "2 years", 2, 
    ifelse(lc$emp_length == "3 years", 3, 
    ifelse(lc$emp_length ==  "4 years", 4, 
    ifelse(lc$emp_length == "5 years", 5, 
    ifelse(lc$emp_length == "6 years", 6, 
    ifelse(lc$emp_length == "7 years", 7, 
    ifelse(lc$emp_length == "8 years", 8, 
    ifelse(lc$emp_length == "9 years", 9, NA))))))))))
)

####  Data Cleansing - Replacing NA values with median----
lc[which(is.na(lc$revol_util)),"revol_util"] <- median(lc$revol_util,na.rm = T)
lc[which(is.na(lc$emp_length)),"emp_length"] <- median(lc$emp_length,na.rm = T)

####  Data Cleansing - Ignoring bad values----
# i.e. Open account > Total account
# Since we're not considering these fields in analysis, we will ignore it here

####  Data Cleansing - Ignoring outliers----
# Would not be possible because most of the outliers belongs to Grade E, F or G and it would be useful in our analysis

####  Data Cleansing - Converting data into suitable format----
lc$issue_d <- as.Date(paste("01",lc$issue_d,sep="-"),"%d-%b-%y")

####  Data Cleansing - Change the var types for ploting----
# Converting columns containing % or other non-numerical data to numeric
# 1.The Interest rate is a string. Remove % and make it a numeric
lc$int_rate <- as.numeric(gsub("[^0-9.]", "", lc$int_rate))
lc$revol_util <- as.numeric(gsub("[^0-9.]", "", lc$revol_util))
# Converting categorical columns into factors
# Writing function to avoid writing toupper() and as.factor() multiple times
convertToFactor <- function(x){
  x <- as.factor(toupper(x))
  return(x)
}
# Purposfully not converted term into numeric as we didn't want another computation later on to add "months" everywhere
lc$term <- convertToFactor(lc$term)
lc$grade <- convertToFactor(lc$grade)
lc$purpose <- convertToFactor(lc$purpose)
lc$emp_length <- convertToFactor(lc$emp_length)
lc$loan_status <- convertToFactor(lc$loan_status)


####  Analysis - Univariate Analysis----
# Univariate Analysis - Loan Status(unordered categorical variable)
# The no of requests which gets "Charged off" is 14% of total requests as shown in graph for unordered categorical variable Loan Status
ggplot(lc,aes(x=lc$loan_status,fill=loan_status)) + geom_bar() + 
  geom_text(stat = "count",aes(x=loan_status,label=..count..),position=position_stack(vjust=0.5)) +
  labs(x="Loan Status",y="No of requests",title="Loan Status")

# Univariate Analysis - Loan Term(ordered categorical variable)
# We are getting more request for term period of 36 months as shown in the second graph for ordered categorical variable Loan Term.
ggplot(lc,aes(x=term,fill=term)) + geom_bar() +
  geom_text(stat = "count",aes(x=term,label=..count..),position=position_stack(vjust=0.5)) +
  labs(x="Loan Term",y="No of requests",title="Loan Term")


# Univariate Analysis - Grade(Ordered categorical)
# More requests belongs to people having Grades A,B and C compared to Grades F and G
ggplot(lc,aes(x=grade,fill=grade)) + geom_bar() +
  geom_text(stat = "count",aes(x=grade,label=..count..),position=position_stack(vjust=0.5)) +
  labs(x="Grade",y="No of requests",title="Grade")

# Univariate Analysis - Loan Purpose (Unordered categorical)
# More requests for Debt consolidation and least for renewable energy purpose.
ggplot(lc,aes(x=purpose,fill=purpose)) + geom_bar() +
  geom_text(stat = "count",aes(x=purpose,label=..count..),position=position_stack(vjust=0.5)) +
  labs(x="Loan Purpose",y="No of requests",title="Loan Purpose") +
  theme(axis.text.x=element_text(angle = 90, hjust = 0))

# Univariate Analysis - Employment Length(ordered categorical variable)
# The Interesting pattern can be observed in the graph of ordered categorical variable is people 
# with 10+ years of employment experience have requested more no of loans compared to 
# the Total of those with 6-9 years of experience.
ggplot(lc,aes(x=emp_length,fill=emp_length)) + geom_bar() +
  geom_text(stat = "count",aes(x=emp_length,label=..count..),position=position_stack(vjust=0.5)) +
  labs(x="Employment Length",y="No of requests",title="Employment Length")


####  Analysis - Segmented Univariate Analysis----

# Segmented Univariate Analysis -Loan Amount across years
# Shows the trend in the total loan amount increased over the 3 years that is from 2010-2012 with respect to the grades of applicants.
ggplot(lc, aes(issue_d, loan_amnt,fill=grade)) + geom_bar(position="stack",stat = "identity") +
  theme(plot.title=element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14)) +
  labs(list(
    title = "Total Loan Amount across the years",
    x = "Issue Year",
    y = "Total Loan Amount")
  )  

# Segmented Univariate Analysis - Loan Status by Purpose 
# Proportion of DEFAULT by different purposes
# Small businesses, Renewable energy have a higher risk compared to other purposes such as wedding and major_purchase.
# In PPT, we've shown better graph for aesthetic purporse.
Loan_by_purpose <- as.data.frame(lc %>%
         select(purpose,default) %>%
         group_by(purpose,default) %>%
         summarise(total = n()) %>%
         arrange(desc(total)))

ggplot(Loan_by_purpose, aes(purpose, total, fill = default)) + 
  geom_bar(position = "fill", stat = "identity") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=18)) +
  theme(axis.text.x=element_text(angle = 90)) +
  labs(list(
    title = "Proportion of DEFAULT by different purposes",
    x = "Purpose",
    y = "Default")
  )

# Segmented Univariate Analysis - Loan Status by Grades
# Proportion of DEFAULT over the Sub-Grade
# Applicants having E,F,G grades makes them highly risky applicants.
# In PPT, we've shown better graph for aesthetic purporse.
loan_by_grade <- as.data.frame(lc %>%
           select(grade,default) %>%
           group_by(grade,default) %>%
           summarise(total = n()) %>%
           arrange(desc(total)))

gbar <- ggplot(loan_by_grade, aes(grade, total, fill = default)) 
gbar + geom_bar(position = "fill", stat = "identity")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18)) +
  theme(axis.text.x=element_text(angle = 90)) +
  labs(list(
    title = "Proportion of DEFAULT over the Grades",
    x = "Grades",
    y = "Default Status")
  )


# Proportion of DEFAULT over the Sub-Grade
# As it can be clearly seen from the graph, the no of people getting bankrupt increases as their sub-grade increases.
loan_by_grade <- as.data.frame(lc %>%
           select(sub_grade,default) %>%
           group_by(sub_grade,default) %>%
           summarise(total = n()) %>%
           arrange(desc(total)))

gbar <- ggplot(loan_by_grade, aes(sub_grade, total, fill = default)) 
gbar + geom_bar(position = "fill", stat = "identity")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18)) +
  theme(axis.text.x=element_text(angle = 90)) +
  labs(list(
    title = "Proportion of DEFAULT over the Grades",
    x = "Sub-Grades",
    y = "Default Status")
  )


# Segmented Univariate Analysis - Interest Rate by Grade
# As the grades are increasing, the interest rates also increases proportionally.
box_status <- ggplot(lc, aes(x=lc$grade, y=lc$int_rate))
box_status + geom_boxplot(aes(fill = grade)) +
  theme(plot.title=element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14)) +
  labs(list(
    title = "Interest rate by Grade",
    x = "Grade",
    y = "Interest Rate")
  )  


# Segmented Univariate Analysis- Loan Amount by Grade
# As the grades are increasing, the loan amount also increases proportionally.
box_status <- ggplot(lc, aes(x=lc$grade, y=lc$loan_amnt))
box_status + geom_boxplot(aes(fill = grade)) +
  theme(plot.title=element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14)) +
  labs(list(
    title = "Loan Amount by Grade",
    x = "Grade",
    y = "Loan Amount")
  )  

# Segmented Univariate Analysis - Revol. Util Rate by Grade
# As the grades are increasing, the revolving utilization rates also increases proportionally.
box_status <- ggplot(lc, aes(x=lc$grade, y=lc$revol_util))
box_status + geom_boxplot(aes(fill = grade)) +
  theme(plot.title=element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14)) +
  labs(list(
    title = "Revolving Utilization Rate by Grades",
    x = "Grades",
    y = "Revolving Utilization Rate")
  )  

####  Analysis - Bivariate Analysis----

# Bivariate Analysis - Revol.  Util. Rate vs Purpose
#In slide multivariate analysis - revol.  Util. Rate vs purpose 
#conclusion: most of the charged-off's are with high revol util rate in each purpose.
util_group <- sqldf("select purpose,loan_status,median(revol_util) as median_util from lc group by purpose,loan_status")

ggplot(util_group,aes(x=purpose,y=median_util,fill=loan_status))+
  geom_bar(position = "stack",stat="identity")+
  geom_text(aes(label=median_util),position = position_stack(vjust = 0.5), size=4) +
  labs(x = "Loan Purpose", y = "Median Revol util", fill = "loan_status" )+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  theme(axis.text.x=element_text(angle = 90)) +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1,
    vjust = 0
  )) +
  ggtitle("Loan Purpose by Median Revolv. Util. Rate")


####  CONCLUSION----  
#	Based on the analysis shown in the previous graphs, we can put some criteria and their -corresponding actions.
#	For example, as the revolving utilization rate increases, their grades will be higher and thus risk increases 
#	which leads to put them into a ‘High’, ’Medium’ or ‘Low’ risk customers
#	Purpose wise the most risky is Small businesses as per the proportions 25% are defaulters. So for Small Businesses risk factor increases.
#	In the next slide, we’ve mentioned few criteria and which categories can we put the customers in and what actions can be taken(can be changed).
