# Bank Predictive modelling data
# https://rpubs.com/shienlong/wqd7004_RRookie
# https://www.kaggle.com/code/chebotinaa/exploratory-data-analysis-with-r-bank-marketing/notebook
# https://rpubs.com/nrnjn_adhikari/391990

rm(list = ls()) # to clear R's environment

library(tidyverse) # for data manipulation
library(dplyr)

# to read in dataset with ; as separator 
df <- read.table('C:/Users/miche/Desktop/RDataAnalysis/termdeposit/bank/bank-full.csv',sep=';',header = T)

# to display preview of variables
head(df, 10)

# generate descriptive statistics of variables
summary(df)

# to check for missing values
anyNA(df)
sum(is.na(df))

# to check for duplicated values in dataset
duplicated(df)
sum(duplicated(df)) # there are 0 duplicated values

# Dealing with missing data
# to convert any cell with a '?' to an NA value
df[df == 'unknown'] <- NA

# to check for missing values, from unknown to NA
sapply(df, function(x) sum(is.na(x)))

glimpse(df)

library(Amelia)

# to visualise missing data using missmap function
missmap(df)

# to color missing values as Yellow
missmap(df,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# to delete away NA missing values from dataset, 
# as character variables cannot be imputed
help("drop_na")
#df1 <- na.omit(df)  

df1 <- df %>% drop_na()

# to check for missing values again
sapply(df1, function(x) sum(is.na(x)))

# to use the missmap() to check all the NAs are removed
missmap(df1,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# to display frequency count in character variables
table(df1$job)

# there are many categorical factors in dataset
# to reduce the number of factors Data cleaning
# to combine unemployed, retired and student into one vector: Not working
not <- function(job){
  job <- as.character(job)# convert job to chr to do string as comparison
  if (job=='unemployed' | job=='student'){
    return('Not-working')
  }else{
    return(job)
  }
}
#####
# to apply the 'not' vector to job column
 df1$job <- sapply(df1$job, not)

# to combine housemaid and services under 1 vector: pink-collar
pink <- function(job){
  job <- as.character(job)# convert job to chr to do string as comparison
  if (job=='housemaid' | job=='services'){
    return('pink-collar')
  }else{
    return(job)
  }
}
####
# to apply the pink function to job column
df1$job <- sapply(df1$job, pink)

# to place technician under blue-collar job
blue <- function(job){
  job <- as.character(job)# convert job to chr to do string as comparison
  if (job=='blue-collar ' | job=='technician'){
    return('blue-collar')
  }else{
    return(job)
  }
}
####
# to apply the pink function to job column
df1$job <- sapply(df1$job, blue)

# frequency count of all levels in type_employer col.
table(df1$job)
table(df1$marital)
table(df1$education)
table(df1$default)
table(df1$loan)
table(df1$contact)
table(df1$poutcome)
table(df1$y)
table(df1$housing)
table(df1$month)

# to check the distribution of age group of banks' clients
summary(df1$age)

# to check data types
glimpse(df1)

# to convert job, marital, education, default, housing, loan, contact,
# month and poutcome into factor for exploratory data analysis
  df2 <- df1 %>%
    mutate(job = as.factor(job),
           marital = as.factor(marital),
           education = as.factor(education),
           default = as.factor (default),
           loan = as.factor (loan),
           contact = as.factor (contact),
           poutcome = as.factor (poutcome),
           housing = as.factor (housing),
           month = as.factor (month))

# to recode 'Age' a continuous variable into 4 categories
df2 <- df2 %>% mutate(age_group = case_when(age >= 18 & age <= 30 ~ " 18 to 30",
                                            age >= 31 & age <= 45 ~ " 31 to 45",
                                            age >= 46 & age <= 65 ~ " 46 to 65",
                                            age >= 66~ "above 65"))

# a graph of Age group by customers that subscribed to Bank's term deposit
p1 <- ggplot(df2, aes(x = age_group)) +
  geom_bar(aes(fill = y)) + 
  labs(title='Age groups of customers that subscribe')

# to move legend to the bottom
p1 + theme(legend.position = "bottom") 

# number of customers who subscribed term deposit by age group
agesub_df <- df2 %>% group_by(age_group, y) %>%
  summarise(count=n(), .groups = 'drop')
agesub_df

# Average bank balance of customers by Age group
avgbal_agedf <- df2 %>% group_by(age_group) %>%
  summarise(Average_balance = mean(balance), .groups = 'drop')
avgbal_agedf

# average bank balance of customers that subscribed to term 
# deposit by age groups
avgbalsub_agedf <- df2 %>% group_by(age_group, y) %>%
  summarise(Average_balance = mean(balance), .groups = 'drop')
avgbalsub_agedf

# maximum bank balance of customers that subscribed to Term deposit
maxbalsub_agedf <- df2 %>% group_by(age_group, y) %>%
  summarise(Maximum_balance = max(balance), .groups = 'drop')
maxbalsub_agedf

# to explore dataset to see if there is any distinct difference between term deposit subscribers
# Using "y" as target variable for all independent variables
ggplot(df2, aes(marital)) + geom_bar(aes(fill= y)) + labs(x = "Marital") + labs(y = " ")

# Computing the Contingency Table (marital)
table(df2$y, df2$marital)

# to compute contingency table (crosstabs) in %
prop.table(table(df2$y, df2$marital))

# number of customers who subscribed term deposit by marital status
marsub_df <- df2 %>% group_by(marital, y) %>%
  summarise(count=n(), .groups = 'drop')
marsub_df

# average bank balance of customers who subscribed term deposit by marital status
avgmarsub_df <- df2 %>% group_by(marital, y) %>%
  summarise(Average_bank_balance = mean(balance), .groups = 'drop')
avgmarsub_df

# to explore dataset to see if there is any distinct difference between term deposit subscribers 
# Using "y" as target variable for all independent variables
ggplot(df2, aes(job)) + geom_bar(aes(fill=y)) + labs(x = "Job") + labs(y = " ")

# Computing the Contingency Table
table(df2$y, df2$job)

# to explore dataset to see if there is any distinct difference between term deposit subscribers 
# Using "y" as target variable for all independent variables
ggplot(df2, aes(education)) + geom_bar(aes(fill=y)) + 
  labs(x = "Education") + labs(y = " ")

# Computing the Contingency Table
table(df2$y, df2$education)

# average balance in account by education level
eduavgbal_df <- df2 %>% group_by(education) %>% 
  summarise(Average_Balance = mean(balance), .groups = 'drop')
eduavgbal_df

# to explore dataset to see if there is any distinct difference between term deposit subscribers 
# Using "y" as target variable for all independent variables
ggplot(df2, aes(default)) + geom_bar(aes(fill=y)) + 
  labs(x = "Default") + labs(y = " ")

# Computing the Contingency Table (default)
table(df2$y, df2$default)

# to explore dataset to see if there is any distinct difference between term deposit subscribers
# Using "y" as target variable for all independent variables
ggplot(df2, aes(housing)) + geom_bar(aes(fill=y)) + 
  labs(x = "Housing") + labs(y = " ")

# Computing the Contingency Table (housing)
table(df2$y, df2$housing)

# to explore dataset to see if there is any distinct difference between term deposit subscribers
# Using "y" as target variable for all independent variables
ggplot(df2, aes(loan)) + geom_bar(aes(fill=y)) + 
  labs(x = "Loan") + labs(y = " ")

# Computing the Contingency Table (loan)
table(df2$y, df2$loan)

# to explore dataset to see if there is any distinct difference between term deposit subscribers
# Using "y" as target variable for all independent variables
ggplot(df2, aes(contact)) + geom_bar(aes(fill=y)) + 
  labs(x = "Contact") + labs(y = " ")

# Computing the Contingency Table (contact)
table(df2$y, df2$contact)

# to explore dataset to see if there is any distinct difference between term deposit subscribers
# Using "y" as target variable for all independent variables
ggplot(df2, aes(month)) + geom_bar(aes(fill=y)) + 
  labs(x = "Month") + labs(y = " ")

# Computing the Contingency Table (month)
table(df2$y, df2$month)

# to explore dataset to see if there is any distinct difference between term deposit subscribers 
# Using "y" as target variable for all independent variables
ggplot(df2, aes(poutcome)) + geom_bar(aes(fill=y)) + 
  labs(x = "Poutcome") + labs(y = " ")

# Computing the Contingency Table (poutcome)
table(df2$y, df2$poutcome)

# to explore dataset to see if there is any distinct difference between term deposit subscribers
# Using "y" as target variable for all independent variables
ggplot(df2, aes(duration)) + geom_bar(aes(fill=y)) + 
  labs(x = "Duration") + labs(y = " ")

# to explore dataset to see if there is any distinct difference between term deposit subscribers
# Using "y" as target variable for all independent variables
ggplot(df2, aes(campaign)) + geom_bar(aes(fill=y)) + 
  labs(x = "Campaign") + labs(y = " ")

# Computing the Contingency Table (campaign)
table(df2$y, df2$campaign)

# to dummy code all categorical variables before logistic regression
# if marital = married, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(marital_married = case_when(marital == "married" ~ 1,
                                 marital == "single" ~ 0,
                                 marital == "divorced" ~ 0))

# if marital = single, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(marital_single = case_when(marital == "married" ~ 0,
                                     marital == "single" ~ 1,
                                     marital == "divorced" ~ 0))

# if marital = divorced, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(marital_divorced = case_when(marital == "married" ~ 0,
                                    marital == "single" ~ 0,
                                    marital == "divorced" ~ 1))

# if education = tertiary, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(education_tertiary = case_when(education == "tertiary" ~ 1,
                                    education == "secondary" ~ 0,
                                    education == "unknown" ~ 0,
                                    education == "primary" ~ 0))

# if education = secondary, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(education_secondary = case_when(education == "tertiary" ~ 0,
                                        education == "secondary" ~ 1,
                                        education == "unknown" ~ 0,
                                        education == "primary" ~ 0))

# if education = primary, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(education_primary = case_when(education == "tertiary" ~ 0,
                                       education == "secondary" ~ 0,
                                       education == "unknown" ~ 0,
                                       education == "primary" ~ 1))

# if default = no, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(default_no = case_when(default == "no" ~ 1,
                                       default == "yes" ~ 0))

# if default = yes, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(default_yes = case_when(default == "no" ~ 0,
                                default == "yes" ~ 1))

# if loan = no, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(loan_no = case_when(loan == "no" ~ 1,
                                loan == "yes" ~ 0))
                               
# if loan = yes, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(loan_yes = case_when(loan == "no" ~ 0,
                             loan == "yes" ~ 1))

# if contact = cellular, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(contact_cellular = case_when(contact == "unknown" ~ 0,
                                     contact == "cellular" ~ 1,
                                     contact == "telephone" ~ 0))

# if contact = telephone, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(contact_telephone = case_when(contact == "unknown" ~ 0,
                                      contact == "cellular" ~ 0,
                                      contact == "telephone" ~ 1))

# if housing = yes, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(housing_yes = case_when(housing == "no" ~ 0,
                             housing == "yes" ~ 1))

# if housing = no, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(housing_no = case_when(housing == "no" ~ 1,
                                housing == "yes" ~ 0))

# if month = may, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_may = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 0,
                               month == "mar" ~ 0,
                               month == "apr" ~ 0,
                               month == "may" ~ 1,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 0,
                               month == "sep" ~ 0,
                               month == "oct" ~ 0,
                               month == "nov" ~ 0,
                               month == "dec" ~ 0,))

#if month = jun, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_jun = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 0,
                               month == "mar" ~ 0,
                               month == "apr" ~ 0,
                               month == "may" ~ 0,
                               month == "jun" ~ 1, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 0,
                               month == "sep" ~ 0,
                               month == "oct" ~ 0,
                               month == "nov" ~ 0,
                               month == "dec" ~ 0,))

# if month = jul, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_jul = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 0,
                               month == "mar" ~ 0,
                               month == "apr" ~ 0,
                               month == "may" ~ 0,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 1,
                               month == "aug" ~ 0,
                               month == "sep" ~ 0,
                               month == "oct" ~ 0,
                               month == "nov" ~ 0,
                               month == "dec" ~ 0,))

# if month = aug, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_aug = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 0,
                               month == "mar" ~ 0,
                               month == "apr" ~ 0,
                               month == "may" ~ 0,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 1,
                               month == "sep" ~ 0,
                               month == "oct" ~ 0,
                               month == "nov" ~ 0,
                               month == "dec" ~ 0,))

# if month = oct, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_oct = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 0,
                               month == "mar" ~ 0,
                               month == "apr" ~ 0,
                               month == "may" ~ 0,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 0,
                               month == "sep" ~ 0,
                               month == "oct" ~ 1,
                               month == "nov" ~ 0,
                               month == "dec" ~ 0,))

# if month = nov, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_nov = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 0,
                               month == "mar" ~ 0,
                               month == "apr" ~ 0,
                               month == "may" ~ 0,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 0,
                               month == "sep" ~ 0,
                               month == "oct" ~ 0,
                               month == "nov" ~ 1,
                               month == "dec" ~ 0,))

# if month = dec, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_dec = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 0,
                               month == "mar" ~ 0,
                               month == "apr" ~ 0,
                               month == "may" ~ 0,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 0,
                               month == "sep" ~ 0,
                               month == "oct" ~ 0,
                               month == "nov" ~ 0,
                               month == "dec" ~ 1,))

# if month = jan, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_jan = case_when(month == "jan" ~ 1,
                               month == "feb" ~ 0,
                               month == "mar" ~ 0,
                               month == "apr" ~ 0,
                               month == "may" ~ 0,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 0,
                               month == "sep" ~ 0,
                               month == "oct" ~ 0,
                               month == "nov" ~ 0,
                               month == "dec" ~ 0,))

# if month = feb, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_feb = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 1,
                               month == "mar" ~ 0,
                               month == "apr" ~ 0,
                               month == "may" ~ 0,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 0,
                               month == "sep" ~ 0,
                               month == "oct" ~ 0,
                               month == "nov" ~ 0,
                               month == "dec" ~ 0,))

# if month = mar, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_mar = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 0,
                               month == "mar" ~ 1,
                               month == "apr" ~ 0,
                               month == "may" ~ 0,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 0,
                               month == "sep" ~ 0,
                               month == "oct" ~ 0,
                               month == "nov" ~ 0,
                               month == "dec" ~ 0,))

# if month = apr, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_apr = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 0,
                               month == "mar" ~ 0,
                               month == "apr" ~ 1,
                               month == "may" ~ 0,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 0,
                               month == "sep" ~ 0,
                               month == "oct" ~ 0,
                               month == "nov" ~ 0,
                               month == "dec" ~ 0,))

# if month = sep, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(month_sep = case_when(month == "jan" ~ 0,
                               month == "feb" ~ 0,
                               month == "mar" ~ 0,
                               month == "apr" ~ 0,
                               month == "may" ~ 0,
                               month == "jun" ~ 0, 
                               month == "jul" ~ 0,
                               month == "aug" ~ 0,
                               month == "sep" ~ 1,
                               month == "oct" ~ 0,
                               month == "nov" ~ 0,
                               month == "dec" ~ 0,))

table(df2$age_group)


# to recode outcome variable 'default', 'loan', 'housing'  into binary values using ifelse function
# logistic regression can only use numeric response variable
#df2$default <- ifelse(df2$default == "yes", 1, 0)
#table(df2$default)

#df2$housing <- ifelse(df2$housing == "yes", 1, 0)
#table(df2$housing)

#df2$loan <- ifelse(df2$loan == "yes", 1, 0)
#table(df2$loan)

# to check total count of subscribers of term deposit 'y' outcome variable
table(df2$y)

# to generate probabilities of outcome variable 'y'
prop.table(table(df2$y))

# to plot a visual of outcome variable 'y'
# huge class imbalance observed, 'y' is more than 88%
ggplot(df2, aes(y)) +
  geom_bar(aes(fill=y)) + 
  labs(title='y target variable distribution')

# to recode outcome variable 'y' into binary values using ifelse function
# logistic regression can only use numeric response variable
df2$y <- ifelse(df2$y == "yes", 1, 0)

# to convert 'Class' target variable into a factor before machine learning (Logistic Regression)
df2$y <- factor(df2$y)

table(df2$y)

# to count total missing values in each column of dataset
sapply(df2, function(x) sum(is.na(x)))

glimpse(df2)

df2 <- select(df2, -c(age_group, marital, loan, month,
                  default, housing, education, contact))


# Load Random Forest & Varimportance plot libraries
#library(randomForest)
#library(varImp)

# to build a random forest prediction model
#rf.model <- randomForest(y ~ ., data = trainData, importance = TRUE )
#rf.model

#importance(rf.model)
#varImpPlot(rf.model,sort=TRUE, main= 'Features Importance by RF')


#x <- trainData
# https://stats.stackexchange.com/questions/49243/rs-randomforest-can-not-handle-more-than-32-levels-what-is-workaround

# Create model with default paramters, estimated accuracy is 90.68%
#control <- trainControl(method="repeatedcv", number=10, repeats=3)
#seed <- 7
#metric <- "Accuracy"
#set.seed(seed)
#mtry <- sqrt(ncol(x))
#tunegrid <- expand.grid(.mtry=mtry)
#rf_default <- train(y~., data=trainData, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
#print(rf_default)

# Random Search for mtry using CARET package
#control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
#set.seed(seed)
#mtry <- sqrt(ncol(x))
#rf_random <- train(y~., data=trainData, method="rf", metric=metric, tuneLength=15, trControl=control)
#print(rf_random)
#plot(rf_random)


#rf.fit <- randomForest(y ~ ., data= df2, ntree=1000,
#                       keep.forest=FALSE, importance=TRUE)
#rf.fit

#library(caret)
#varImpPlot(rf.fit,type=2)

#(VI_F=importance(rf.fit))

#varImp(rf.fit)


# to drop poutcome and age_group from dataframe before logistic regression
#df3 <- select(df2, -c(age_group, marital, default, 
#                      housing, loan, contact, month, education))

# to check dataset before logistic regression
#glimpse(df3)

# to split dataset into training and testing 
library(caret)

# To Prep Training (70%) and Test (30%) data.
set.seed(100)
trainDataIndex <- createDataPartition(df2$y, p=0.7, list = FALSE)
trainData <- df2[trainDataIndex, ]
testData <- df2[-trainDataIndex, ]

# there are 7.5 times more positive or 'yes' cases than 'no' non-subcribers
table(trainData$y)

# to Upsample 'y' variable using upsample function in CARET package
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData)],
                     y = trainData$y)

#glimpse(up_train)

# we see that case imbalance in Outcome variable 'y' been fixed contain equal numbers
table(up_train$Class)

#To build a Logistic Model using the Upsample version
logitmod <- glm(formula = Class ~.,
                family = "binomial", 
               data=up_train)

# to print logistic regression results
summary(logitmod)

#To predict the response on testdata
# *Note: when you use logistic regression, need to set type='response 
pred <- predict(logitmod, newdata = testData, type = "response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$y

# to compute accuracy rate
mean(y_pred == y_act)

# to generate Confusion Matrix from CARET package
# 'yes' is positive, positive = "1"
confusionMatrix(y_pred, y_act, positive="1", mode="everything")

# measure of predictive capability of a categorical x variable to
# accurately predict the goods and bads
library(InformationValue)

plotROC(y_act, pred)

AUROC(y_act, pred)

# To calculate 'True Positive Rate' or 'Events' that is correctly
# predicted by the model
sensitivity(y_act, pred)

# Precision Logistic Regression
precision(y_act, pred)

library(rpart) # for Decision tree
library(rpart.plot) # to plot decision tree

# to build a decision tree model
dt <- rpart(y~., data = trainData, method = 'class')

# to visualize the decision tree with rpart.plot
rpart.plot(dt)

# to generate confusion table and accuracy
treePred <- predict(dt, testData, type = 'class')

table(treePred, testData$y)
mean(treePred==testData$y)




















