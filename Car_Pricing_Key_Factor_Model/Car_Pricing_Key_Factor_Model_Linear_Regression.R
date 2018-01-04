## BUSINESS CASE: Geely Auto
## Goal: To understand how exactly the prices vary with the independent variables. 
##       They can accordingly manipulate the design of the cars, the business strategy etc. 
##       to meet certain price levels. 

####  Clear the environment by removing all the pre-loaded variables----
#===============================-
rm(list=ls())

### Loading required packages----
library(ggplot2)
library("magrittr")  # For using pipeline character
library(dplyr) # For using select
#You have to install the below packages before starting this assignment.
#install.packages("MASS") for StepAIC
#install.packages("car") for VIF
library(car);

### Load the dataset----
#===============================-
# Cast blank values to NA as well
car_price <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = F, na.strings = TRUE)

####  Data Cleansing----
#===============================-

####  Data Cleansing - Removing unnecessary columns----
# Get rid of all columns having only 1 unique value as it can't help in our analysis.
car_price <- car_price[vapply(car_price, function(x) length(unique(x)) > 1, logical(1L))]

####  Data Cleansing - Remove duplicate records----
car_price <- car_price[!duplicated(car_price),]

# Data Cleansing - Replacing blank values with NA(In case, if it escaped while loading steps)
car_price[car_price==""] <- NA

# Data Cleansing - Replacing n/a(string) with NA
car_price[which(car_price == "n/a"),] <- NA

####  Data Cleansing - Ignoring outliers----
# Checking curbweight for outlier  values
quantile(car_price$curbweight,seq(0,1,0.01))
# Huge gap exists between 0% and 1%
car_price$curbweight[which(car_price$curbweight<1819.72)] <- 1819.72
# Gap increases drastically high for values above 92%. Thus capping the values at 3292.48.
car_price$curbweight[which(car_price$curbweight>3292.48)] <- 3292.48

# Checking compressionratio for outlier  values
quantile(car_price$compressionratio,seq(0,1,0.01))
# After 90%, there is huge jump in the compression ratio values
car_price$compressionratio[which(car_price$compressionratio>10.9400)] <- 10.9400
# 

# Converting categorical data into factor
car_price$fueltype <- factor(car_price$fueltype)
car_price$aspiration <- factor(car_price$aspiration)
car_price$doornumber <- factor(car_price$doornumber)
car_price$carbody <- factor(car_price$carbody)
car_price$drivewheel <- factor(car_price$drivewheel)
car_price$enginelocation <- factor(car_price$enginelocation)
car_price$enginetype <- factor(car_price$enginetype)
car_price$cylindernumber <- factor(car_price$cylindernumber)
car_price$fuelsystem <- factor(car_price$fuelsystem)
car_price$symboling <- factor(car_price$symboling)

str(car_price)
####  Derived Matrics----
# Binning the levels of "symboling" into equ*al bins of 3. 
levels(car_price$symboling)[1:2] <- "low-risk"
levels(car_price$symboling)[2:3] <- "medium-risk"
levels(car_price$symboling)[3:4] <- "high-risk"

# Deriving Company column based on CarName column for analysis
car_price$CompanyName <- factor(gsub("([A-Za-z]+).*", "\\1", (car_price$CarName)))
summary(car_price$CompanyName)
# Correcting the spelling mistakes
levels(car_price$CompanyName)[levels(car_price$CompanyName)=="vw"] <- "vokswagen"
levels(car_price$CompanyName)[levels(car_price$CompanyName)=="vokswagen"] <- "volkswagen"
levels(car_price$CompanyName)[levels(car_price$CompanyName)=="porcshce"] <- "porsche"
levels(car_price$CompanyName)[levels(car_price$CompanyName)=="toyouta"] <- "toyota"
levels(car_price$CompanyName)[levels(car_price$CompanyName)=="maxda"] <- "mazda"

# Let's quickly look at the summary of all useful categorical variables
# CTRL + SHIFT + C
# summary(car_price$fueltype) #
# summary(car_price$aspiration) #
# summary(car_price$doornumber) #
# summary(car_price$carbody)
# summary(car_price$drivewheel)
# summary(car_price$enginelocation) #
# summary(car_price$enginetype)
# summary(car_price$cylindernumber)
# summary(car_price$fuelsystem)
# summary(car_price$symboling)
# 
# Check structure once again to see if nothing is missed
str(car_price)

# Removing CarName it is not useful for our analysis 
# PS: car_ID has been kept for drawing plots in the end to compare the error across actual and predicted values
car_price <- within(car_price,rm(CarName))

####  DUMMY VARIABLE CREATION ----
# convert factors with 2 levels to numerical variables

#  0-diesel 1-gas
levels(car_price$fueltype)<-c(1,0)
car_price$fueltype <- as.numeric(levels(car_price$fueltype))[car_price$fueltype]

# 0-turbo    1-std
levels(car_price$aspiration)<-c(1,0)
car_price$aspiration <- as.numeric(levels(car_price$aspiration))[car_price$aspiration]

# 0-two 1-four 
levels(car_price$doornumber)<-c(1,0)
car_price$doornumber <- as.numeric(levels(car_price$doornumber))[car_price$doornumber]

# 0-rear 1-front 
levels(car_price$enginelocation)<-c(1,0)
car_price$enginelocation <- as.numeric(levels(car_price$enginelocation))[car_price$enginelocation]

# Converting categorical variables with more than two levels/categories

carbody <- data.frame(model.matrix(~carbody,data=car_price))
carbody <- carbody[,-1]

drivewheel <- data.frame(model.matrix(~drivewheel,data=car_price))
drivewheel <- drivewheel[,-1]

enginetype <- data.frame(model.matrix(~enginetype,data=car_price))
enginetype <- enginetype[,-1]

cylindernumber <- data.frame(model.matrix(~cylindernumber,data=car_price))
cylindernumber <- cylindernumber[,-1]

fuelsystem <- data.frame(model.matrix(~fuelsystem,data=car_price))
fuelsystem <- fuelsystem[,-1]

symboling <- data.frame(model.matrix(~symboling,data=car_price))
symboling <- symboling[,-1]

company <- data.frame(model.matrix(~CompanyName,data=car_price))
company <- company[,-1]

# Merging all the data frames into car_price which is a master data frame
car_price <- cbind(car_price[,-(ncol(car_price)+1)],carbody,drivewheel,enginetype,fuelsystem,symboling,company)

# Removing converted columns and dummy data frames
car_price <- within(car_price,rm("carbody","drivewheel","enginetype","cylindernumber","fuelsystem","symboling","CompanyName"))
rm("carbody","drivewheel","enginetype","cylindernumber","fuelsystem","symboling","company")

####  Model 1 - Multiple R-squared:  0.964,	Adjusted R-squared:  0.9501  ----
# Build model 1 containing all variables
# Build the linear model using lm() and store it into a object "model_1"
model_1 <- lm(price~.,data=car_price)
summary(model_1)

####  stepAIC----
# Now, lets see how to use stepAIC

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 


# Lets load the library in which stepAIC function exists
#install.packages("MASS")
library(MASS)

# We have a total of 42 variables considered into the model 
#Now let;s run the code. 

step <- stepAIC(model_1, direction="both")
#Great, so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2
# You can notice that stepAIC removed variables - fuelsystemmfi,carlength,enginetypedohcv, etc.

####  Model 2 - Multiple R-squared:  0.9622,	Adjusted R-squared:  0.9529    ----
model_2 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen, 
              data = car_price)
summary(model_2)
vif(model_2)

####Explanation for variable removals---- 
# Variables removed and kept based on their VIF and p-Value comparison
# Variables removed based on the multicollinearity with other independent variable


####  Model 3 - Multiple R-squared:  0.9622,	Adjusted R-squared:  0.9529    ----
# Removing enginetypeohcf because of higher p-Value
model_3 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspfi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen, 
              data = car_price)
summary(model_3)
vif(model_3)



####  Model 4 - Multiple R-squared:  0.9622,	Adjusted R-squared:  0.9529    ----
# Removing fuelsystemspfi because of higher p-Value
model_4 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen, 
              data = car_price)
summary(model_4)
vif(model_4)



####  Model 4 - Multiple R-squared:  0.9612,	Adjusted R-squared:  0.9523    ----
# Removing fuelsystemspfi because of higher p-Value
model_4 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen, 
              data = car_price)
summary(model_4)
vif(model_4)



####  Model 5 - Multiple R-squared:  0.9609,	Adjusted R-squared:  0.9522    ----
# Removing CompanyNameisuzu because of higher p-Value
model_5 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen, 
              data = car_price)
summary(model_5)
vif(model_5)




####  Model 6 - Multiple R-squared:  0.9603,	Adjusted R-squared:  0.9518    ----
# Removing fuelsystemmpfi because of higher p-Value
model_6 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen, 
              data = car_price)
summary(model_6)
vif(model_6)



####  Model 7 - Multiple R-squared:  0.9596,	Adjusted R-squared:  0.9513    ----
# Removing fueltype because of higher p-Value
model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNamemazda + 
                CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen, 
              data = car_price)
summary(model_7)
vif(model_7)





####  Model 8 - Multiple R-squared:  0.9591,	Adjusted R-squared:  0.951    ----
# Removing CompanyNamemazda because of higher p-Value
model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen, 
              data = car_price)
summary(model_8)
vif(model_8)


####  Model 9 - Multiple R-squared:  0.9585,	Adjusted R-squared:  0.9504    ----
# Removing CompanyNamevolkswagen because of higher p-Value
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                fuelsystem2bbl + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota, 
              data = car_price)
summary(model_9)
vif(model_9)



####  Model 10 - Multiple R-squared:  0.9577,	Adjusted R-squared:  0.9498    ----
# Removing enginetypel because of higher p-Value
model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carlength + carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetypeohcv + enginetyperotor + 
                 fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_10)
vif(model_10)


####  Model 11 - Multiple R-squared:  0.9571,	Adjusted R-squared:  0.9494    ----
# Removing CompanyNamechevrolet because of higher p-Value
model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carlength + carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetypeohcv + enginetyperotor + 
                 fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamerenault + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_11)
vif(model_11)



####  Model 12 - Multiple R-squared:  0.9563,	Adjusted R-squared:  0.9487    ----
# Removing CompanyNamerenault because of higher p-Value
model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carlength + carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetypeohcv + enginetyperotor + 
                 fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_12)
vif(model_12)





####  Model 13 - Multiple R-squared:  0.9554,	Adjusted R-squared:  0.948    ----
# Removing enginetypeohcv because of higher p-Value
model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carlength + carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_13)
vif(model_13)





####  Model 14 - Multiple R-squared:  0.9546,	Adjusted R-squared:  0.9473    ----
# Removing CompanyNamehonda because of higher p-Value
model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carlength + carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_14)
vif(model_14)




####  Model 15 - Multiple R-squared:  0.9536,	Adjusted R-squared:  0.9466    ----
# Removing carlength because of higher p-Value
model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_15)
vif(model_15)



####  Multicollinearity Check for Model 15 ----
# Draw scatter plot using jittered points to understand the collinearity between two variables
ggplot(car_price, aes(x=carbodyhatchback, y=carbodysedan)) + geom_jitter()
# Find correlation between the variables whose VIFs are higher and p-Values are low
# i.e., carbodyhatchback and carbodysedan
cor(car_price$carbodyhatchback,car_price$carbodysedan)
# Conclusion: Remove carbodysedan


####  Model 16 - Multiple R-squared:  0.9475,	Adjusted R-squared:  0.9398    ----
# Removing carbodysedan because of higher p-Value and VIF
model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhardtop + carbodyhatchback + carbodywagon + 
                 enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_16)
vif(model_16)



####  Model 17 - Multiple R-squared:  0.9473,	Adjusted R-squared:  0.9399    ----
# Removing carbodywagon because of higher p-Value
model_17 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhardtop + carbodyhatchback + 
                 enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_17)
vif(model_17)



####  Model 18 - Multiple R-squared:  0.947,	Adjusted R-squared:  0.9399    ----
# Removing carbodyhardtop because of higher p-Value
model_18 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhatchback + enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_18)
vif(model_18)

##TURNING POINT


####  Model 19 - Multiple R-squared:  0.9438,	Adjusted R-squared:  0.9366    ----
# Removing carbodyhardtop because of higher VIF
model_19 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhatchback + enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + CompanyNamepeugeot + 
                 CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_19)
vif(model_19)




####  Model 20 - Multiple R-squared:  0.9434,	Adjusted R-squared:  0.9365    ----
# Removing CompanyNamepeugeot because of higher p-Val
model_20 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhatchback + enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamesaab + CompanyNametoyota, 
               data = car_price)
summary(model_20)
vif(model_20)



####  Model 21 - Multiple R-squared:  0.9423,	Adjusted R-squared:  0.9357    ----
# Removing CompanyNametoyota because of higher p-Val
model_21 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + carheight + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhatchback + enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamesaab, 
               data = car_price)
summary(model_21)
vif(model_21)




####  Model 22 - Multiple R-squared:  0.9412,	Adjusted R-squared:  0.9348    ----
# Removing carheight because of higher p-Val
model_22 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                 carbodyhatchback + enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamesaab, 
               data = car_price)
summary(model_22)
vif(model_22)





####  Model 23 - Multiple R-squared:  0.9399,	Adjusted R-squared:  0.9337    ----
# Removing highwaympg because of higher p-Val and higher VIF
model_23 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + 
                 enginesize + boreratio + compressionratio + peakrpm + 
                 carbodyhatchback + enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamesaab, 
               data = car_price)
summary(model_23)
vif(model_23)




####  Model 24 - Multiple R-squared:  0.9393,	Adjusted R-squared:  0.9334     ----
# Removing compressionratio because of higher p-Val
model_24 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + 
                 carbodyhatchback + enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamesaab, 
               data = car_price)
summary(model_24)
vif(model_24)

####  Multicollinearity Check for Model 24 ----
# Draw scatter plot using jittered points to understand the collinearity between two variables
ggplot(car_price, aes(x=enginesize, y=curbweight)) + geom_jitter()
# Find correlation between the variables whose VIFs are higher and p-Values are low
# i.e., enginesize and carwidth
cor(car_price$enginesize,car_price$curbweight) # 0.78
# Conclusion: remove curbweight


####CHECKPOINT

####  Model 25 - Multiple R-squared:  0.9373,	Adjusted R-squared:  0.9316      ----
# Removing curbweight 
model_25 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth +  
                 enginesize + boreratio + peakrpm + 
                 carbodyhatchback + enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamesaab, 
               data = car_price)
summary(model_25)
vif(model_25)



####  Multicollinearity Check for Model 25 ----
# Draw scatter plot using jittered points to understand the collinearity between two variables
ggplot(car_price, aes(x=CompanyNameporsche, y=enginelocation)) + geom_jitter()
# Find correlation between the variables whose VIFs are higher and p-Values are low
# i.e., enginesize and carwidth
cor(car_price$enginelocation,car_price$CompanyNameporsche) # -0.77
# Conclusion: remove enginelocation

####  Model 26 - Multiple R-squared:  0.932,	Adjusted R-squared:  0.9263      ----
# Removing enginelocation 
model_26 <- lm(formula = price ~ aspiration + carwidth +  
                 enginesize + boreratio + peakrpm + 
                 carbodyhatchback + enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamesaab, 
               data = car_price)
summary(model_26)
vif(model_26)



####  Multicollinearity Check for Model 26 ----
# Draw scatter plot using jittered points to understand the collinearity between two variables
ggplot(car_price, aes(x=carwidth, y=enginesize)) + geom_jitter()
# Find correlation between the variables whose VIFs are higher and p-Values are low
# i.e., enginesize and carwidth
cor(car_price$enginesize,car_price$carwidth) # 0.73
# Conclusion: remove carwidth because if we remove enginewidth, R-squared values goes down by 10% as tried and tested

####  Model 27 - Multiple R-squared:  0.9222,	Adjusted R-squared:  0.916       ----
# Removing carwidth
model_27 <- lm(formula = price ~ aspiration +  
                 enginesize + boreratio + peakrpm + 
                 carbodyhatchback + enginetyperotor + fuelsystem2bbl + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamesaab, 
               data = car_price)
summary(model_27)
vif(model_27)


####  Model 28 - Multiple R-squared:  0.9215,	Adjusted R-squared:  0.9157       ----
# Removing fuelsystem2bbl
model_28 <- lm(formula = price ~ aspiration +  
                 enginesize + boreratio + peakrpm + 
                 carbodyhatchback + enginetyperotor + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamesaab, 
               data = car_price)
summary(model_28)
vif(model_28)




####  Model 29 - Multiple R-squared:  0.9215,	Adjusted R-squared:  0.9157       ----
# Removing boreratio
model_29 <- lm(formula = price ~ aspiration +  
                 enginesize + peakrpm + 
                 carbodyhatchback + enginetyperotor + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche + CompanyNamesaab, 
               data = car_price)
summary(model_29)
vif(model_29)



####  Model 30 - Multiple R-squared:  0.9174,	Adjusted R-squared:  0.9122       ----
# Removing boreratio
model_30 <- lm(formula = price ~ aspiration +  
                 enginesize + peakrpm + 
                 carbodyhatchback + enginetyperotor + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNameporsche, 
               data = car_price)
summary(model_30)
vif(model_30)


####  Model 31 - Multiple R-squared:  0.9137,	Adjusted R-squared:  0.9088       ----
# Removing CompanyNameplymouth
model_31 <- lm(formula = price ~ aspiration +  
                 enginesize + peakrpm + 
                 carbodyhatchback + enginetyperotor + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameporsche, 
               data = car_price)
summary(model_31)
vif(model_31)

####  Model 32 - Multiple R-squared:  0.9103,	Adjusted R-squared:  0.9057       ----
# Removing CompanyNamedodge
model_32 <- lm(formula = price ~ aspiration +  
                 enginesize + peakrpm + 
                 carbodyhatchback + enginetyperotor + CompanyNamebmw + 
                 CompanyNamebuick + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameporsche, 
               data = car_price)
summary(model_32)
vif(model_32)


##############  TEST PREDICTION #####################

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(car_price), 0.7*nrow(car_price))
train = car_price[trainindices,]
test = car_price[-trainindices,]

####  Predicting the results in test dataset  ----
test$test_price <- predict(model_32,test[,-1])
test$error <- test$price - test$test_price

####  Test the r square between actual and predicted price  ----
r_test <- cor(test$price,test$test_price)# 0.931923
rsquared_test <- r_test^2
rsquared_test #0.8684804

######################################################


################# ORIGINAL DATASET PREDICTION ###################

####  Predicting the results in original dataset  ----
car_price$test_price <- predict(model_32,car_price[,-1])
car_price$error <- car_price$price - car_price$test_price

####  Test the r square between actual and predicted price  ----
r_full <- cor(car_price$price,car_price$test_price)
rsquared_full <- r_full^2 #0.9541214
rsquared_full #0.9103477
  
################################################################

####  PLOTS and GRAPHS-----
# Plot - Actual vs Predicted Views Model_32
# On test dataset
ggplot(test, aes(car_ID, price)) + geom_line(aes(colour = "Actual Price" )) +
  scale_x_continuous(name = "CarId", breaks = seq(0,204,7), limits = c(0,204)) +
  scale_y_continuous(name = "Test Price", breaks = seq(0,42000,11000), limits = c(0,42000)) +
  geom_line(aes(car_ID, y=test_price, colour="Test Price"))
# On Original dataset
ggplot(car_price, aes(car_ID, price)) + geom_line(aes(colour = "Test Price" )) +
  scale_x_continuous(name = "CarId", breaks = seq(0,204,7), limits = c(0,204)) +
  scale_y_continuous(name = "Price", breaks = seq(0,42000,11000), limits = c(0,42000)) +
  geom_line(aes(car_ID, y=test_price, colour="Actual Price"))

####  Actual and Predicted price comparision plot----
# Alternate- Using a library to generate a plot with 2 Y axis
library(plotrix)
# Generating the plot comparing actual price and model generated price to draw the comparision
# On test dataset
twoord.plot( lx=test$car_ID,ly=test$price,ry=test$test_price, 
             rx = test$car_ID, ylab="Actual Price",rylab="Test Price")
# On Original dataset
twoord.plot( lx=car_price$car_ID,ly=car_price$price,ry=car_price$test_price, 
             rx = car_price$car_ID, ylab="Actual Price",rylab="Test Price")

####  Error distribution plots----
# Plot Model_32 errors on test dataset
ggplot(test, aes(car_ID, error)) + geom_point() +
  scale_y_continuous(name = "Error", breaks = seq(-8800,14000,2000), limits = c(-8800,14000)) +
  geom_hline(yintercept = 0)
# Plot Model_32 errors on original dataset
ggplot(car_price, aes(car_ID, error)) + geom_point() +
  scale_y_continuous(name = "Error", breaks = seq(-8800,14000,2000), limits = c(-8800,14000)) +
  geom_hline(yintercept = 0)

# Plot shows that errors doesn't form any kind of pattern. It's randomly distributed. 
# And thus, we're not missing any extra variable which could've helped form the pattern.

################################### Main Questions ###################################
######Q: Which variables are significant in predicting the price of a car#############
###   A: aspiration, enginetyperotor, peakrpm, carbodyhatchback, enginetyperotor    ##

######Q: How well those variables describe the price of a car#########################
##  1.86-94% of the variablility in the prices can be explained using our model.         
##  2.Significantly small difference between R-Squared and Adjusted-R-Squared shows that we've not used any 
##    extra variable
##  3. As shown in the Plot - Actual vs Predicted Views Model_32, actual and predicted valus are quite overlapping. Which shows the error is quite small.
##  4. Cars having hatchback body and enginetype as rotor sells well in U.S. Market and especially with BMW, Buick, Mitsubishi, Nissan and Porsche-
######################################################################################