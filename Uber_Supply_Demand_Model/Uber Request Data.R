####  Clear the environment by removing all the pre-loaded variables----
#===============================-
rm(list=ls())

### Load the dataset----
#===============================-
uber_trips <- read.csv("Uber Request Data.csv",stringsAsFactors = F)

####  DATA CLEANSING----
#===============================-
# Converting timestamp column from character to date type

# Two points to note down
# 1.We won't be splitting the  timestamp column into Date and time as two separate columns as 
# it is not really adding any value for creating multiple columns for the kind of analysis we're doing.
# 2.Not replacing NA as it is not affecting the analysis we're performing.

# Have used multiple variables to store the correct date for different formates
a <- strptime(uber_trips$Request.timestamp,format="%d-%m-%Y %H:%M:%S")
b <- strptime(uber_trips$Request.timestamp,format="%d/%m/%Y %H:%M")
# Merging all the correctly formated dates in one vector
a[is.na(a)] <- b[!is.na(b)]
# Replacing existing timestamp data with correctly formatted data
uber_trips$Request.timestamp <- a

# Repeating same as above steps for Drop timestamp 
a <- strptime(uber_trips$Drop.timestamp,format="%d-%m-%Y %H:%M:%S")
b <- strptime(uber_trips$Drop.timestamp,format="%d/%m/%Y %H:%M")
# Merging all the correctly formated dates in one vector
a[is.na(a)] <- b[!is.na(b)]
# Replacing existing timestamp data with correctly formatted data
uber_trips$Drop.timestamp <- a


####  DERIVING NEW COLUMNS----
#===============================-
# Deriving Hour column from Request and Drop timestamp for further timeslot analysis
# We're not deriving other columns like Year, Month, Minute and Second because it is not required for 
# this analysis at this point of time
uber_trips$Request.Hour <- format(uber_trips$Request.timestamp,"%H")
uber_trips$Drop.Hour <- format(uber_trips$Drop.timestamp,"%H")

# Deriving another column for bucketing the time slots for easier understanding 
breaks <- c("00", "04", "08","11", "16","21","24")  # times are internally fractions of a day
labels <- c("Mid-night(00-04)", "Early morning(05-08)", 
            "Morning(09-11)","Afternoon(12-16)","Late evening(17-21)","Night(22-24)")
# The cut function is useful for turning continuous variables into factors. 
uber_trips$TimeSlot <- cut(
  as.numeric(uber_trips$Request.Hour), 
  breaks, 
  labels, 
  include.lowest = TRUE
)

####  RESULTS EXPECTED - POINT 1----
#===================================-
# Visually identify the most pressing problems for Uber. 
# Hint: Create plots to visualise the frequency of requests that get cancelled or 
# show 'no cars available'; identify the most problematic types of requests 
# (city to airport / airport to city etc.) and the time slots 
# (early mornings, late evenings etc.) using plots

# If not installed, please install it using install.packages("ggplot2")
library(ggplot2)
# If not installed, please install it using install.packages("gridExtra")
# Useful If one wants to see multiple plots at the same time
library(gridExtra)
#detach("package:magrittr")
library(dplyr)


#### Chart type used : Bar Chart ----
#===============================-
# Reasons: 
# 1. It's easier for anyone to understand the data 
# 2. Helps in showing each category in frequency distribution
# 3. Clarifies trends better than tables

# Overall Analysis of Frequency of Requests
# REASON: To understand the status of overall requests
plot_1_1 <- ggplot(uber_trips,aes(x=Status))+ 
  geom_bar() + 
  labs(x="Request Status",y="No of requests",title="Overall request status chart")

# Overall Analysis of Types of Requests
# Excluding "Trip Completed" requests as it won't add up any value here
# REASON: Helpts understanding the kind of request with their respective status
# Legends:  "City" in Blue color represents rides from "City to Airport"
#           "Airport" in Light red color represents rides from "Airport to City"
# Have added Dodge overlapping objects to understand request side-by-side
plot_1_2_ds <- subset(uber_trips,Status!="Trip Completed");
plot_1_2 <- ggplot(plot_1_2_ds,aes(x=Status,fill=Pickup.point))+ 
  geom_bar(position="dodge") + 
  labs(x="Request Status",y="No of requests",title="Overall request status & type of request chart")

# Overall Analysis of Different Time Slots
# REASON: Helps understand at which time slot are we getting more demand
# Legends:  "City" in Blue color represents rides from "City to Airport" throughout the day
#           "Airport" in Light red color represents rides from "Airport to City" throughout the day
plot_1_3 <- ggplot(uber_trips,aes(x=uber_trips$TimeSlot, fill=Pickup.point)) + 
  geom_bar(position="dodge") + 
  labs(x="Timeslot",y="No of requests",title="Overall time slot chart")

# Plotting 3 graphys row by row to understand it clearly the difference between all three
# Please click on "ZOOM" option to see the full screen graph
grid.arrange(plot_1_1,plot_1_2,plot_1_3,nrow=3)

####  RESULTS EXPECTED - POINT 2----
#==================================-
# Find out the gap between supply and demand and show the same using plots.
#   1. Find the time slots when the highest gap exists
#   2. Find the types of requests (city-airport or airport-city) for which the gap is the most 
#     severe in the identified time slots

#### ASSUMPTION MADE ----
#===============================-
# SUPPLY: All the trips which have got completed successfully
# DEMAND: It includes the request which have got either cancelled by Driver or 
#         shows "No cars available" status. I have kept it intentionally such that it won't 
#         include supply. It clearly means that this is the demand which we couldn't fulfill.

# Deriving new dataframe by summarizing required details
demand_supply <- uber_trips %>%
  select(Pickup.point,Status,TimeSlot) %>%
  group_by(Pickup.point,Status,TimeSlot) %>%
  summarise(freq=n())

# Converting Status column to Categorical variables by converting its factor to DEMAND & SUPPLY
demand_supply$Status <- gsub("Cancelled|No Cars Available","Demand",demand_supply$Status)
demand_supply$Status <- gsub("Trip Completed","Supply",demand_supply$Status)

# Further grouping the demand_supply to get the final numbers
demand_supply <- demand_supply %>%
  select(Pickup.point,Status,TimeSlot,freq) %>%
  group_by(Pickup.point,Status,TimeSlot) %>%
  summarise(freq=sum(freq))

# Plotting the final data to show the difference between the DEMAND and SUPPLY between
# various time slots and request type to provide the possible solution to this business problem
# LEGEND: City -> From City to Airport Rides
#         Airport -> From Airport to City Rides
# Creating multiple time slot panel using "facet_grid" to make the graph most useful
ggplot(demand_supply,aes(x=Status,y=freq)) +
  geom_bar(stat="identity",aes(fill=Pickup.point)) +
  scale_fill_manual(values=c("chartreuse1", "cyan4")) + 
  facet_grid(~demand_supply$TimeSlot)

# Answer 1. 
# There are mainly two slots in which we can see the highest gap exits
# 1. Early morning(5-8)
# 2. Late evening(17-21)

# Answer 2.
# The gap exists for the "cars going to the airport" early in the morning which is 5 to 8
# The gap exists for the "cars coming to the city" late in the evening which is 17 to 21



####  RESULTS EXPECTED - POINT 3----
#==================================-
# What do you think is the reason for this issue for the supply-demand gap? 
# Write the answer in less than 100 words. You may accompany the write-up with plot(s).

# Answer
# Two main reasons for it could be:
# 1. Too much time being spent on going to the airport, waiting at the airport for another city ride 
# could lead the drivers to cancel their city to airport ride in the early morning i.e. 5 to 8, 
# as they could earn the same money by doing multiple in-city rides.
# 2. Less no of drivers available for late evening i.e. 17 to 21 drive from airport to city ride 
# as most of the driver's day shift would be getting over at that point of time  and 
# less organic supply of cabs as there won't be many flights taking off during that point of time.

####  RESULTS EXPECTED - POINT 4----
#==================================-
# Recommend some ways to resolve the supply-demand gap.

# ANSWER

#Here are two ways Uber can try to solve the problem of non-availability and cancellation:
  
#--- 1.  Boosting incentives for specific location and timeslot
# Give more incentive to driver for specific location and time based rides 
# i.e. Late evening airport to city rides and early morning city to airport rides. 
# Like trip request from that specific location and time should boost their earning 
# by 2 times or 2.5 times. But trip requested from outside that location will not be multiplied. 
# They will be earning same regular fare.

#--- 2.  Penalize the driver for unnecessary cancellation
# Penalizing the drivers for more cancellation for certain time slot and destination. 
# Like in early morning they aren't allowed to cancel more than 1 airport ride request 
# from city. If they do, no more request should be assigned to that driver 
# for particular time frame.
