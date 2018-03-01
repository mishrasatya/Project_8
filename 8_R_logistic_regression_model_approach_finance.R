##------------Bank Marketing Analysis---------------------##

#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Preparation
# 3. Modelling
# 4. Model Evaluation
# 5. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:- Prospect Profiling
#-------------------------------------------------------

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

#-------------------------------------------------------

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response

# Checking missing values

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 
quantile(bank_data$age,seq(0,1,0.01))

# Box plot 
boxplot(bank_data$age)

# Capping the upper values of age with 71.
bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".
bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"
bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket
agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))
agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot
ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset
str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job
levels(bank_data$job)


# Plotting bar graph for job variable.
# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 
str(bank_data)

# Checking Marital status
summary(bank_data$marital)

# Let's replace Unknown level to married
levels(bank_data$marital)[4] <- "married"

# Plotting marital status
plot_response(bank_data$marital,"marital")

# Let's see the education variables
plot_response(bank_data$education,"Education")

# Reducing the levels of education variable
levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot
plot_response(bank_data$education,"Education_levels")


#-------------------------------------------------------
# Let's see the default variable
table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 
summary(bank_data$housing)

plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"
summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

#  Next variable is Contact, Let's see the response rate of each mode 
summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 
plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable
plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable
# Let's check the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 
summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset
# So let's check the percentile distribution of duration 
quantile(bank_data$duration,seq(0,1,0.01))


# So, capping the duration seconds at 99% which is 1271.3sec 
bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 
summary(bank_data$campaign)

# Let's see the percentile distribution of this variable
boxplot(bank_data$campaign)

quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14
bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot
ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary
summary(bank_data$pdays)
levels(bank_data$pdays)

# Reducing the levels of this variable to 3.
levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the respose rate of each levels. 
plot_response(bank_data$pday,"Pday")

# Number of prospects under each category
table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"


summary(bank_data$previous)

plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')
summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------

#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

#----------------------------------------------------------------------------

# Creating the variable cost_per_call as a new column and adding it to the dataframe.
# Cost per call is calculated as duration * 0.033 + 0.8

bank_data$cost_per_call <- bank_data[,10]*0.033 + 0.8

#----------------------------------------------------------------------------

# We will create an Id for each data point, as it will be helpful in later stages. 

bank_data <- cbind("prospect_ID" = sprintf("%03d", 1:nrow(bank_data)), bank_data)

#----------------------------------------------------------------------------

# Keeping a copy of the dataframe before moving to dummmy variable creation

bank_data_reserve <- bank_data

#---------------------------------------------------------------------------
# DATA PREPARATION
#---------------------------------------------------------------------------
# Column Job

#Checking the summary of variable "job"#
summary(factor(bank_data$job))

#Converting "job" into dummies#
job_dummy <- data.frame(model.matrix( ~job, data = bank_data))

#Removing the x-intercept from the newly created job_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 11 variables#
Job_dummy <- job_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
bank_data <- cbind(bank_data[,-3], Job_dummy)

#---------------------------------------------------------------------------
# Column marital

#Checking the summary of variable "marital"#
summary(factor(bank_data$marital))

#Converting "marital" into dummies#
marital_dummy <- data.frame(model.matrix( ~marital, data = bank_data))

#Removing the x-intercept from the newly created marital_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
Marital_dummy <- marital_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
bank_data <- cbind(bank_data[,-3], Marital_dummy)

#---------------------------------------------------------------------------
# Column education

#Checking the summary of variable "education"#
summary(factor(bank_data$education))

#Converting "education" into dummies#
edu_dummy <- data.frame(model.matrix( ~education, data = bank_data))

#Removing the x-intercept from the newly created edu_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 4 variables#
Edu_dummy <- edu_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
bank_data <- cbind(bank_data[,-3], Edu_dummy)

#---------------------------------------------------------------------------
# Column housing

#Checking the summary of variable "housing"#
summary(factor(bank_data$housing))

#Converting "housing" into dummies#
house_dummy <- data.frame(model.matrix( ~housing, data = bank_data))

#Removing the x-intercept from the newly created house_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
House_dummy <- house_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
bank_data <- cbind(bank_data[,-3], House_dummy)

#---------------------------------------------------------------------------
# Column loan

#Checking the summary of variable "loan"#
summary(factor(bank_data$loan))

#Converting "loan" into dummies#
loan_dummy <- data.frame(model.matrix( ~loan, data = bank_data))

#Removing the x-intercept from the newly created loan_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
Loan_dummy <- loan_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
bank_data <- cbind(bank_data[,-3], Loan_dummy)

#---------------------------------------------------------------------------
# Column contact

#Checking the structure & summary of variable "contact"#
str(bank_data$contact)
summary(factor(bank_data$contact))

#Converting contact variable to numeric so as to replace the levels- cellular and telephone with 1 and 0#
levels(bank_data$contact)<-c(0,1)

#Storing the numeric values in the same variable#
bank_data$contact <- as.numeric(levels(bank_data$contact))[bank_data$contact]

# Checking the summary of variable after conversion#
summary(factor(bank_data$contact))

#---------------------------------------------------------------------------
# Column month

#Checking the summary of variable "month"#
summary(factor(bank_data$month))

#Converting "month" into dummies#
month_dummy <- data.frame(model.matrix( ~month, data = bank_data))

#Removing the x-intercept from the newly created month_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 9 variables#
Month_dummy <- month_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
bank_data <- cbind(bank_data[,-4], Month_dummy)

#---------------------------------------------------------------------------
# Column day_of_week

#Checking the summary of variable "day_of_week"#
summary(factor(bank_data$day_of_week))

#Converting "day_of_week" into dummies#
day_of_week_dummy <- data.frame(model.matrix( ~day_of_week, data = bank_data))

#Removing the x-intercept from the newly created day_of_week_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 3 variables#
Day_of_week_dummy <- day_of_week_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
bank_data <- cbind(bank_data[,-4], Day_of_week_dummy)

#---------------------------------------------------------------------------
# Column pdays

#Checking the summary of variable "pdays"#
summary(factor(bank_data$pdays))

#Converting "pdays" into dummies#
pdays_dummy <- data.frame(model.matrix( ~pdays, data = bank_data))

#Removing the x-intercept from the newly created pdays_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
Pdays_dummy <- pdays_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
bank_data <- cbind(bank_data[,-6], Pdays_dummy)

#---------------------------------------------------------------------------
# Column previous

#Checking the summary of variable "previous"#
summary(factor(bank_data$previous))

#Converting "previous" into dummies#
previous_dummy <- data.frame(model.matrix( ~previous, data = bank_data))

#Removing the x-intercept from the newly created previous_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
Previous_dummy <- previous_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
bank_data <- cbind(bank_data[,-6], Previous_dummy)

#---------------------------------------------------------------------------
# Column poutcome

#Checking the summary of variable "poutcome"#
summary(factor(bank_data$poutcome))

#Converting "poutcome" into dummies#
poutcome_dummy <- data.frame(model.matrix( ~poutcome, data = bank_data))

#Removing the x-intercept from the newly created poutcome_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
Poutcome_dummy <- poutcome_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
bank_data <- cbind(bank_data[,-6], Poutcome_dummy)

#---------------------------------------------------------------------------

# Removing columns like duration, binning age and cost per callfrom the dataframe
# that we will use for model building.

bank_data <- bank_data[,-c(4,12,13)]

#---------------------------------------------------------------------------

# We will scale the nr.employed column as the values in this column is significantly high

bank_data$nr.employed <- scale(bank_data$nr.employed)


#---------------------------------------------------------------------------

# MODELLING

# Loading the required libraries

library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(dplyr)

# Final dataframe to be used for model building

bank_final <- bank_data
bank_final$response <- as.factor(ifelse(bank_final$response == 1, "yes", "no"))

# splitting into train and test data

set.seed(100)
split_indices <- sample.split(bank_final$response, SplitRatio = 0.70)
train_1 <- bank_final[split_indices, ]
test <- bank_final[!split_indices, ]

train <- train_1[-1]

# first model
model_1 = glm(response ~ ., data = train, family = "binomial")
summary(model_1)

# Stepwise selection
model_2 <- stepAIC(model_1, direction="both")
summary(model_2)
vif(model_2)

# removing educationTertiary_Education it has high p-value
model_3 <- glm(formula = response ~ age + contact + campaign + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
                 jobblue.collar + jobretired + jobservices + 
                 housingunknown + loanyes + monthaug + monthdec + monthjun + 
                 monthmar + monthmay + monthnov + day_of_weekmon + day_of_weekwed + 
                 pdaysFirst_time_contacted + previousLess_than_3_times + previousMore.than_3_times + 
                 poutcomesuccess, family = "binomial", data = train)


summary(model_3)
vif(model_3)

# removing housingunknown it has high p-value
model_4 <- glm(formula = response ~ age + contact + campaign + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
                 jobblue.collar + jobretired + jobservices + 
                 loanyes + monthaug + monthdec + monthjun + monthmar + monthmay + 
                 monthnov + day_of_weekmon + day_of_weekwed + pdaysFirst_time_contacted + 
                 previousLess_than_3_times + previousMore.than_3_times + poutcomesuccess, 
               family = "binomial", data = train)


summary(model_4)
vif(model_4)

# removing loanyes it has high p-value
model_5 <- glm(formula = response ~ age + contact + campaign + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
                 jobblue.collar + jobretired + jobservices + monthaug + 
                 monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                 day_of_weekwed + pdaysFirst_time_contacted + previousLess_than_3_times + 
                 previousMore.than_3_times + poutcomesuccess, family = "binomial", 
               data = train)


summary(model_5)
vif(model_5)

# removing nr.employed it has high VIF
model_6 <- glm(formula = response ~ age + contact + campaign + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + euribor3m + 
                 jobblue.collar + jobretired + jobservices + monthaug + monthdec + 
                 monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                 day_of_weekwed + pdaysFirst_time_contacted + previousLess_than_3_times + 
                 previousMore.than_3_times + poutcomesuccess, family = "binomial", 
               data = train)


summary(model_6)
vif(model_6)

# removing jobretired it has high VIF & p value
model_7 <- glm(formula = response ~ age + contact + campaign + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + euribor3m + 
                 jobblue.collar + jobservices + monthaug + monthdec + 
                 monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                 day_of_weekwed + pdaysFirst_time_contacted + previousLess_than_3_times + 
                 previousMore.than_3_times + poutcomesuccess, family = "binomial", 
               data = train)


summary(model_7)
vif(model_7)

# removing age it has high p value
model_8 <- glm(formula = response ~ contact + campaign + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + euribor3m + 
                 jobblue.collar + jobservices + monthaug + monthdec + 
                 monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                 day_of_weekwed + pdaysFirst_time_contacted + previousLess_than_3_times + 
                 previousMore.than_3_times + poutcomesuccess, family = "binomial", 
               data = train)


summary(model_8)
vif(model_8)

# removing monthdec it has high p value
model_9 <- glm(formula = response ~ contact + campaign + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + euribor3m + 
                 jobblue.collar + jobservices + monthaug + 
                 monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                 day_of_weekwed + pdaysFirst_time_contacted + previousLess_than_3_times + 
                 previousMore.than_3_times + poutcomesuccess, family = "binomial", 
               data = train)


summary(model_9)
vif(model_9)

# removing day_of_weekwed it has high p value
model_10 <- glm(formula = response ~ contact + campaign + emp.var.rate + 
                 cons.price.idx + cons.conf.idx + euribor3m + 
                 jobblue.collar + jobservices + monthaug + 
                 monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                 pdaysFirst_time_contacted + previousLess_than_3_times + 
                 previousMore.than_3_times + poutcomesuccess, family = "binomial", 
               data = train)


summary(model_10)
vif(model_10)

# removing jobservices it has high p value
model_11 <- glm(formula = response ~ contact + campaign + emp.var.rate + 
                  cons.price.idx + cons.conf.idx + euribor3m + 
                  jobblue.collar + monthaug + 
                  monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                  pdaysFirst_time_contacted + previousLess_than_3_times + 
                  previousMore.than_3_times + poutcomesuccess, family = "binomial", 
                data = train)


summary(model_11)
vif(model_11)

# removing previousMore.than_3_times it has high p value
model_12 <- glm(formula = response ~ contact + campaign + emp.var.rate + 
                  cons.price.idx + cons.conf.idx + euribor3m + 
                  jobblue.collar + monthaug + 
                  monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                  pdaysFirst_time_contacted + previousLess_than_3_times + 
                  poutcomesuccess, family = "binomial", 
                data = train)


summary(model_12)
vif(model_12)

#---------------------------------------------------------------------------

model_final <- model_12

# Predicting probabilities of responding for the test data

predictions_logit <- predict(model_final, newdata = test[,-1], type = "response")
summary(predictions_logit)
predictions_logit

# Adding the predicted values to the test dataframe

test$predict <- predictions_logit

# sorting the predict column of test dataframe in descending order

test <- test[rev(order(test$predict)),]
  
#---------------------------------------------------------------------------

## MODEL EVALUATION

# Let's use the probability cutoff of 50%.
predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf

# Accuracy : 0.8992
# Sensitivity : 0.20905
# Specificity : 0.98677

#---------------------------------------------------------    
# Let's find out the optimal probalility cutoff

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
}

# plotting sensitivity , specificity and accuracy with different values of probability
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#--------------------------------------------------------- 

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Let's choose a cutoff value of 11.5% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.115, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf_final

# Accuracy : 0.8239
# Sensitivity : 0.62284
# Specificity : 0.84942

#---------------------------------------------------------

# Adding the predicted response to the test dataframe

test$predicted_response <- predicted_response

# Creating a dataframe with prospect id, duration and cost per call,
# this will be used for merging with test dataframe

pros_cost_per_call <- bank_data_reserve[,c(1,11,23)] 

# merging the test dataframe and the newly created pros_cost_per_call dataframe

test <- merge(test,pros_cost_per_call,by.x = "prospect_ID",by.y = "prospect_ID")

# We will now take only the six required variables and create a new dataframe called new_test

new_test <- test[,c(1,10,52,51,53,54)]

#Re-naming the columns as per requirement#
colnames(new_test)[colnames(new_test)=="response"] <- "actual_response"
colnames(new_test)[colnames(new_test)=="predict"] <- "predicted_probability_of_response"
colnames(new_test)[colnames(new_test)=="duration"] <- "duration_of_call_in_seconds"
colnames(new_test)[colnames(new_test)=="cost_per_call"] <- "cost_of_call"

#---------------------------------------------------------

# MODEL DEPLOYMENT AND RECOMMENDATIONS

# sorting the predicted_probability_of_response column of new_test dataframe in descending order

new_test <- new_test[rev(order(new_test$predicted_probability_of_response)),]

summary(new_test$actual_response)

# no     yes 
# 10964  1392

summary(new_test$predicted_response)

# no   yes 
# 9838 2518

#---------------------------------------------------------

# Creating a lift chart function

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}


# converting the actual response to factor

new_test$actual_response <- as.factor(ifelse(new_test$actual_response=="yes",1,0))

# Creating a lift and gain table, with 10 groups

LG = as.data.frame(lift(new_test$actual_response, new_test$predicted_probability_of_response, groups = 10))

#---------------------------------------------------------

# How much prospect do we need to target to meet the business objective.
# that would be the first five decile.

No_to_contact <- 1236 + 1236 + 1235 + 1236 + 1235
Total_no <- 12356
percent <- (No_to_contact/Total_no)*100

# Average of call duration and cost for the first 6178 prospects.

average_data <- new_test[1:6178,]
average_cost <- mean(average_data$cost_of_call)
average_duration <- mean(average_data$duration_of_call_in_seconds)

# 9.657092 Cost
# 268.3967 Duration

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")
plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

#---------------------------------------------------------
# By contacting the first five decile we would be able to acheive 80% of our target.
# The first five decile consists of 1133 respondants.
# So we need to call 1236 + 1236 + 1235 + 1236 + 1235 = 6178 people to acheive the above target.
# The total numbers of prospects that should be contacted is 50%
# The average call cost for targeting the top 50% prospects is 9.65.
# The average call duration for targeting the top 50% prospects is 268.3967.
# In the 5th decile (50% people targeted), we can capture about 80% of the responders, this is
# what the gain chart says.
#---------------------------------------------------------
















