# Required packages and libraries used for the project in R
install.packages("dplyr")
library(dplyr) 
install.packages("plyr")
library(plyr)
install.packages("ggplot2")
library(ggplot2)
install.packages(c("corrplot"))
library(corrplot)
install.packages(c("ggm", "gmodels", "vcd", "Hmisc",                
                   "pastecs", "psych", "doBy")) 
library(ggm)
library(Hmisc)
library(pastecs)
library(psych)
library(doBy)
library(vcd)
library(gmodels)
install.packages("car")
library(car)
install.packages('MASS')
library(MASS)

# This command is used to get the location of current working directory
getwd() 
# This command is used to point to the folder containing the required file   
setwd("U:/Intro to DA/Project")

#Read the file bank-full.csv
#This command imports the required data set and saves it to the Bank data frame.
Bank <- read.csv("bank-full.csv",header=TRUE)
Bank

# Structure of Bank dataframe to see if the data is structured or not
str(Bank)

#Checking for the structure and other possible incompleteness
summary(Bank)

sum(is.na(Bank))
#[1] 0
#The data set now has 0 missing values.

#recoding categorical variables into numerical data to perform regression algorithms.

Bank_reg1 <- Bank%>%  mutate(marital= ifelse(marital == "married",1,ifelse(marital== "single",2,0)))
str(Bank_reg1)


Bank_reg2 <- Bank_reg1%>%  mutate(education= ifelse(education == "primary",1,ifelse(education== "secondary",2,ifelse(education== "tertiary",3,0))))
str(Bank_reg2)


Bank_reg3 <- Bank_reg2%>%  mutate(default= ifelse(default == "yes",1,0))
str(Bank_reg3)

Bank_reg4 <- Bank_reg3%>%  mutate(housing= ifelse(housing == "yes",1,0))
str(Bank_reg4)

Bank_reg5 <- Bank_reg4%>%  mutate(loan= ifelse(loan == "yes",1,0))
str(Bank_reg5)

Bank_reg6 <- Bank_reg5%>%  mutate(contact= ifelse(contact == "cellular",1,ifelse(contact == "telephone",2,0)))
str(Bank_reg6)

Bank_reg7 <- Bank_reg6%>%  mutate(month= ifelse(month == "jan",1,ifelse(month == "feb",2,ifelse(month == "mar",3,ifelse(month == "apr",4,ifelse(month == "may",5,ifelse(month == "jun",6,ifelse(month == "jul",7,ifelse(month == "aug",8,ifelse(month == "sep",9,ifelse(month == "oct",10, ifelse( month == "nov",11,12))))))))))))
str(Bank_reg7)

Bank_reg8 <- Bank_reg7%>%  mutate(poutcome= ifelse(poutcome == "success",1,ifelse(poutcome == "other",2,ifelse(poutcome == "unknown",3,0))))
str(Bank_reg8)

Bank_reg9 <- Bank_reg8%>%  mutate(job= ifelse(job == "admin.",1,ifelse(job == "blue-collar",2,ifelse(job == "entrepreneur",3,ifelse(job == "housemaid",4,ifelse(job == "management",5,ifelse(job == "retired",6,ifelse(job == "self-employed",7,ifelse(job == "services",8,ifelse(job == "student",9,ifelse(job == "technician",10, ifelse( job == "unemployed",11,0))))))))))))
str(Bank_reg9)

Bank_reg <- Bank_reg9%>%  mutate(y= ifelse(y == "yes",1,0))
str(Bank_reg)

#save the file in our current working directory
write.table(Bank_reg,file="Bank_reg.csv",row.names=F,sep=",")



# Splitting into Train and Test Data
set.seed(222)
split = sample(2,nrow(Bank_reg),prob = c(0.75,0.25),replace = TRUE)
train_set = Bank_reg[split == 1,]
test_set = Bank_reg[split == 2,]

#It is the usual practice in Machine Learning field to divide the data set into train and test set. 
#The model will be built on the train set and the performance of the model will be tested on the test.


#checking dimensions of train and test data sets
dim(train_set)
dim(test_set)

# dim(train_set)
#[1] 33887    16
# dim(test_set)
#[1] 11324    16

##**************************
#LOGISTIC REGRESSION

#Logistic regression uses sigmoid function to classify variables into classes 
#and its basically applicable to classification problems

# Fitting Logistic Regression to the Training set
logistics_classifier = glm(formula = y ~ .,
                           family = binomial,
                           data = train_set)

summary(logistics_classifier)


#Call:
#  glm(formula = y ~ ., family = binomial, data = train_set)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-5.8211  -0.4428  -0.2903  -0.1774   3.0725  

#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -3.305e+00  1.762e-01 -18.756  < 2e-16 ***
# age          2.688e-03  1.953e-03   1.376  0.16874    
# job          1.091e-02  6.204e-03   1.759  0.07862 .  
# marital      2.033e-01  3.593e-02   5.659 1.52e-08 ***
# education    1.825e-01  2.599e-02   7.024 2.15e-12 ***
# default     -2.113e-01  1.795e-01  -1.177  0.23924    
# housing     -1.104e+00  4.359e-02 -25.336  < 2e-16 ***
# loan        -6.903e-01  6.500e-02 -10.621  < 2e-16 ***
# contact      6.585e-01  4.237e-02  15.542  < 2e-16 ***
# day         -5.319e-03  2.371e-03  -2.244  0.02485 *  
# month        1.521e-03  7.381e-03   0.206  0.83672    
# duration     4.037e-03  7.121e-05  56.686  < 2e-16 ***
# campaign    -1.472e-01  1.180e-02 -12.480  < 2e-16 ***
# pdays        8.679e-04  2.880e-04   3.013  0.00258 ** 
# previous     6.610e-02  9.338e-03   7.079 1.45e-12 ***
# poutcome    -2.173e-01  2.943e-02  -7.383 1.55e-13 ***
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 24481  on 33886  degrees of freedom
#Residual deviance: 18212  on 33871  degrees of freedom
#AIC: 18244

#Number of Fisher Scoring iterations: 6

#Based on the output of the Logistic regression, only 9 variables are significant while other are insignificant.
#The significant variables to be considered in our prediction model are
#marital, education, housing, loan, contact, duration, campaign, previous and poutcome.


#Prediction using Logistics Regressor

# Predicting the Test set results
prob_pred = predict(logistics_classifier, type = 'response', newdata = test_set)
My_pred = ifelse(prob_pred > 0.5, 1, 0)

#If the probability of the prediction is greater than 0.5 then we are predicting it 
#as Yes to term deposit, otherwise No.

output <- cbind(test_set, My_pred)
dim(output)
# dim(output)
# [1] 11324    17

#Confusion Matrix
#estimating the performance of the model

cm = table(ActualValue=test_set$y, PredictedValue=prob_pred > 0.5)
cm

#           PredictedValue
#ActualValue   FALSE TRUE
#         0    9799   206
#         1    1030   289

#We can check by building a confusion matrix to display the success rate of our model's predictions on the test_set data we created earlier. 
#The table function builds the confusion matrix. 
#Going diagonally, (True Negative=9799, True Positive=289) represent the number of correct predictions. 
#Conversely, the going up diagonally, (False Negative=1030, False Positive=206) represent the number of incorrect predictions.

#Estimating the percentage of performance
sum(diag(cm))/sum(cm)
#[1] 0.8908513


#Logistics Regression was able to give us an accuracy of 89.08%, 
#which means that we can expect our model to classify correct about 9 observations in every 10.