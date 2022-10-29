#------------------------------------------------------
#####     Consulting Project        ###########
#-------------------------------------------------------
#####        Home Equity             #########
#---------------------------------------------------------

# Clearing the environment
rm(list=ls())


#Setting Working Directory
setwd("D:/Drexel2021/Course/Summer/Consulting/Data")

#Reading target file, importing blank spaces as NA
equity <- read.csv(file = "loan_home equity.csv",
                   na.strings = c("", " "),
                   stringsAsFactors = FALSE)


#Loading packages
library("caret")
library("RColorBrewer")
library("tidyverse")
library("ggplot2")
library("mice")

## Previewing the data
head(x = equity)
tail(x = equity)

##Viewing the structure of the dataset
str(equity)

#Viewing the rows and columns

nrow(equity) # number of rows = 5964
ncol(equity) # number of columns = 14
colnames(equity) 

##Conversion to Alternate Variable Types

# Setting up vectors to  convert
# groups of variables at the same time.

#Nominal categorical variables as facs
facs <- c( "Reason_HE","Occupation")
#Excluding Default from this vector as it is our target
#variable.

#Dataset does not have ordinal variables.

#Numerical Variable as nums
nums <- c("Loan_Amount", "Home_Val", "Mort_Bal", "YOJ",
          "Num_Derog","Num_Delinq","CL_Age","Num_Inq","Num_CL",
          "Debt_Inc")

equity <- equity[ ,-1]
##Excluded variable "HEID" as it is only a unique identifier
##Not relevant to the scope of our analysis.

vars <- c(facs, nums)

#Checking the unique values that the variables can take on.

#For all facs and ords variables
lapply(X = equity[ ,facs], 
       FUN = unique)

# Converting facs variables to factors
equity[ ,facs] <- lapply(X = equity[ , facs], 
                         FUN = factor)

#Converting Target Variable to factor
equity$Default <- factor(x = equity$Default)

#----------------------------------------------------------
#########   Data Exploration   ############
#----------------------------------------------------------
#Rechecking the structure
str(equity)


#Mode function from Professor Hill's codes
modefun <- function(x){
  if(any(tabulate(match(x, unique(x))) > 1)){
    outp <- unique(x)[which(tabulate(match(x, unique(x))) == max(tabulate(match(x, unique(x)))))]
  } else {
    outp <- "No mode exists!"}
  return(outp)
}

# Applying mode to both categorical variables in the equity dataframe
lapply(X = equity[ ,facs], 
       FUN = modefun)

# Statistical Summary of Numerical variables from equity dataframe
sapply(X = equity[,nums], 
       FUN = summary)

#Standard deviation of numerical variables
sapply(X = equity[ , nums], 
       FUN = sd)
###Getting NA values coz lots of missing value. Lets try 
#treated dataset.
#sapply(X = equity2_noNA[ , nums],
#       FUN = sd)

#Exploring categorical variables through 1-way frequency table
lapply(X = equity[ ,facs], FUN = table)


## 2-Way Frequency Table

#Default and loan amount
ct1 <- table(Default = equity$Default,
             Occupation = equity$Occupation)
ct1
#Default seems proportionately higher in Occupation: 
#Sales. More than half have defaulted. About 1/3rd defaults
# in Self Employed. Seems lowest among ProfExe, around 16%..

#Reason_HE and Default
ct2 <- table(Default = equity$Default,
             Reason = equity$Reason_HE 
)
ct2
#Seems proportional


###Grouped Summary Information

##Debt Income and Default
aggregate(formula = Debt_Inc ~ Default, 
          data = equity, 
          FUN = summary)
#Debt to Income ratio slightly higher in defaulters.

##Credit Line and Default
aggregate(formula = Num_CL ~ Default, 
          data = equity, 
          FUN = summary)
#Credit line not so different.

##Number of Inquiry and Default
aggregate(formula = Num_Inq ~ Default, 
          data = equity, 
          FUN = summary)
#Num of inquiry not so different until median. Diff after
#3rd quadrant.

##Credit Line Age and Default
aggregate(formula = CL_Age ~ Default, 
          data = equity, 
          FUN = summary)
##CL Age is higher in non-defaulters.

##Delinquent and Default
aggregate(formula = Num_Delinq ~ Default, 
          data = equity, 
          FUN = summary)
##Num delinq higher in defaulters.

##Num Derog and Default
aggregate(formula = Num_Derog ~ Default, 
          data = equity, 
          FUN = summary)
##Higher in defaulters.

##YOJ and Default
aggregate(formula = YOJ ~ Default, 
          data = equity, 
          FUN = summary)
##Slightly higher in non default.

##Home Val and Default
aggregate(formula = Home_Val ~ Default, 
          data = equity, 
          FUN = summary)
##Mixed patterns.

##Mortgage Balance and Default
aggregate(formula = Mort_Bal ~ Default, 
          data = equity, 
          FUN = summary)
##Mostly higher in non-defaulters.

##loan amount and Default
aggregate(formula = Loan_Amount ~ Default, 
          data = equity, 
          FUN = summary)
##Higher in non default.

#-----------------------------------------------------------
######      Data Visualization   #######
#-----------------------------------------------------------

##Boxplots
#----------------------------------------------------------
#Loan Amount
boxplot(x = equity$Loan_Amount, 
        main="Loan Amount Plot",
        col = "cadetblue2")

#Mortgage Balance
boxplot(x = equity$Mort_Bal, 
        main="Mortgage Balance Plot",
        col = "azure1")

#YOJ
boxplot(x = equity$YOJ, 
        main="Years on Job Plot",
        col = "azure1")

#CL_Age
boxplot(x = equity$CL_Age, 
        main="Credit Line Age Plot",
        col = "azure1")

#Debt to Income Ratio
boxplot(x = equity$Debt_Inc, 
        main="Debt to Income Ratio Plot",
        col = "azure1")


## Grouped Barplots
#-----------------------------------------------------------
ct1
barplot(height = ct1,
        col = c("cadetblue4","cadetblue1"),
        legend.text = TRUE,
        args.legend = list(x = "topright"),
        main = "Default by Occupation")

ct2
barplot(height = ct2,
        col = c("cadetblue4","cadetblue1"),
        legend.text = TRUE,
        args.legend = list(x = "topright"),
        main = "Default by Reason for Home Equity")

#Num_Derog
ggplot(data = equity, mapping = aes(x = Num_Derog,
                                    fill = Default)) +
  labs(x="Derogatory Reports", y="", 
       title = "Number of Derogatory Reports and Default")+
  geom_bar()

#Num_Delinq
ggplot(data = equity, mapping = aes(x = Num_Delinq,
                                    fill = Default)) +
  labs(x="Delinquent Credit Lines", y="", 
       title = "Delinquent Credit Lines and Default")+
  geom_bar()

#Num_INQ
ggplot(data = equity, mapping = aes(x = Num_Inq,
                                    fill = Default)) +
  labs(x="Number of Inquiries", y="", 
       title = "Number of Inquiries and Default")+
  geom_bar()


##Histograms
#----------------------------------------------------------
#CL_Age
hist(x = equity$CL_Age, 
     main = "Credit Line Age", 
     xlab = "", 
     col = "cadetblue3")

#Loan Amount
hist(x = equity$Loan_Amount, 
     main = "Loan Amount", 
     xlab = "", 
     col = "cadetblue3")


#Loan amount with default fill
ggplot(data = equity, 
       mapping = aes(x = Loan_Amount, fill = Default))+
  labs(title = "Loan Amount and Default", x="", y="") +
  geom_histogram(bins = 15)


#Mortgage Balance
ggplot(data = equity, 
       mapping = aes(x = Mort_Bal, fill = Default))+
  labs(title = "Mortage Balance and Default", x="", y="") +
  geom_histogram(bins = 15)

#CL_Age
ggplot(data = equity, 
       mapping = aes(x = CL_Age, fill = Default))+
  labs(title = "Credit Line Age and Default", x="", y="") +
  geom_histogram(bins = 15)

#YOJ
ggplot(data = equity, 
       mapping = aes(x = YOJ, fill = Default))+
  labs(title = "Years on Job and Default", x="Years on Job", y="") +
  geom_histogram(bins = 15)




#----------------------------------------------------------
########    Data Quality       ############
#----------------------------------------------------------


## Checking for Outliers

# Identifying Outliers by Z-Score Method
# We use the abs() function to identify observations in which 
#the absolute value of the Z-Score is greater than 3.

outs <- sapply(equity[ ,nums], function(x) which(abs(x) > 3))
outs
#Might need to standardize and select model before checking 
#again and treating. We will let it be for now.

## Duplicate Values
# Using duplicated() function to identify duplicate observations 
equity[duplicated(x = equity), ]

#Removing duplicate values
equity <- equity[!duplicated(x = equity), ]


#-----------------------------------------------------------
##### Missing Values
#-----------------------------------------------------------

###Checking for missing Values using complete cases

equity[!complete.cases(equity), ]
#2620 rows with at least 1 missing value

na_rows <- rownames(equity)[!complete.cases(equity)]
na_rows

##Using mice to impute missing values

init = mice(equity, maxit=0) 
set.seed(103)
equity_imp = mice(equity, m=5)
equity_imp <- complete(equity_imp)

init
sapply(equity_imp, function(x) sum(is.na(x)))

summary(equity_imp)

write.csv(equity_imp,"C:\\Users\\Nam Dang\\DropboX\\2022 Summer\\equityimp.csv", row.names = TRUE)

#-----------------------------------------------------------
######     Decision Tree    ################### 
#-----------------------------------------------------------

library(rpart)
library(rpart.plot)


## Preprocessing & Transformation

## Training and Testing
# Splitting the data into training and 
# testing sets using an 85/15 split rule


# Initialize random seed
set.seed(103) 

# Create list of training indices
sub <- createDataPartition(y = equity_imp$Default, # target variable
                           p = 0.85, # % in training
                           list = FALSE)

# Subset the transformed data
# to create the training (train)
# and testing (test) datasets
train <- equity_imp[sub, ] # create train dataframe
test <- equity_imp[-sub, ] # create test dataframe



## Class Imbalance 

#Plot the target variable Default to check for class imbalance
plot(equity_imp$Default,
     main = "Default")
##Class imbalance exists


### Default, Base Model (with class imbalance)
# We will train a Decision Tree model
ctrl_DT <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 3)

set.seed(103) # initialize random seed

DTFit_base <- train(x = train[,vars],
                    y = train$Default,
                    method = "rpart", # use the rpart package
                    trControl = ctrl_DT, # control object
                    tuneLength = 5) # try 5 cp values

# We can view the results of our tuned
# model with class imbalance
DTFit_base

### Resampling Methods to correct class imbalance

## Oversampling
#(We are oversampling for decision tree as we do not want
# potentially compromise on the number of nodes in our analysis.
#As our data set has a low number of the target variable, we 
#will oversample to treat class imbalance.)

set.seed(103) 
train_os <- upSample(x = train[ ,vars], # predictors
                     y = train$Default, # target
                     yname = "Default")



DTFit_ROS <- train(x = train_os[ ,vars],
                   y = train_os$Default,
                   method = "rpart", # use the rpart package
                   trControl = ctrl_DT, # control object
                   tuneLength = 5) # try 5 cp values

#Viewing the results of our tuned
# model using random oversampling
DTFit_ROS

## Analysis

# 1. Basic Model 
# We used the rpart() function in the rpart 
# package to perform basic DT classification 
# on our training dataset.


MR.rpart <- rpart(formula = Default ~ ., # Y ~ all other variables in dataframe
                  data = train_os[ ,c(vars, "Default")], # include only relevant variables
                  method = "class")

# We can see the basic output of our
# Decision Tree model
MR.rpart

# We can use the variable.importance
# component of the rpart object to 
# obtain variable importance
MR.rpart$variable.importance

## Tree Plots
# We can use either the prp() function 
# or the rpart.plot() function in the 
# rpart.plot package to plot our 
# rpart object (FD.rpart).

prp(x = MR.rpart, # rpart object
    extra = 2) # include proportion of correct predictions

## Training Performance
# We use the predict() function to generate 
# class predictions for our training set
base.trpreds <- predict(object = MR.rpart, # DT model
                        newdata = train_os, # training data
                        type = "class") # class predictions

# We use the confusionMatrix() function
#  to obtain a confusion matrix and obtain performance
# measures for our model applied to the
# training dataset (train).
DT_train_conf <- confusionMatrix(data = base.trpreds, # predictions
                                 reference = train_os$Default, # actual
                                 positive = "1",
                                 mode = "everything")
DT_train_conf


## Testing Performance
# We use the predict() function to generate 
# class predictions for our testing set
base.tepreds <- predict(object = MR.rpart, # DT model
                        newdata = test, # testing data
                        type = "class")

# Confusion matrix for test data set

DT_test_conf <- confusionMatrix(data = base.tepreds, # predictions
                                reference = test$Default, # actual
                                positive = "1",
                                mode = "everything")
DT_test_conf


## Goodness of Fit

# To assess if the model is balanced,
# underfitting or overfitting, we compare
# the performance on the training and
# testing. We can use the cbind() function
# to compare side-by-side.

# Overall
round(cbind(Training = DT_train_conf$overall,
      Testing = DT_test_conf$overall),2)

# Class-Level
cbind(Training = DT_train_conf$byClass,
      Testing = DT_test_conf$byClass)

#------------------------------------------

### 2. Hyperparameter Tuning Model

# We will perform a grid search for the 
# optimal cp value.

# We will search over a grid of values
# from 0 to 0.05. We use the expand.grid()
# function to define the search space
grids <- expand.grid(cp = seq(from = 0,
                              to = 0.05,
                              by = 0.005))
grids

# First, we set up a trainControl object
# (named ctrl) using the trainControl() 
# function in the caret package. We specify 
# that we want to perform 10-fold cross 
# validation, repeated 3 times and specify
# search = "grid" for a grid search. We use 
# this object as input to the trControl 
# argument in the train() function below.
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     search = "grid")
ctrl

# Next, we initialize a random seed for 
# our cross validation
set.seed(103)

# Then, we use the train() function to
# train the DT model using 5-Fold Cross 
# Validation (repeated 3 times). We set
# tuneGrid equal to our grid search
# objects, grids.
DTFit <- train(form = Default ~ ., # use all variables in data to predict delay
               data = train_os[ ,c(vars, "Default")], # include only relevant variables
               method = "rpart", # use the rpart package
               trControl = ctrl, # control object
               tuneGrid = grids) # custom grid object for search

# We can view the results of our
# cross validation across cp values
# for Accuracy and Kappa. The output
# will also identify the optimal cp.
DTFit

DTFit$results[DTFit$results$cp %in% DTFit$bestTune,] #best result


# We can plot the cp value vs. Accuracy
plot(DTFit)

# We can view the confusion matrix showing
# the average performance of the model
# across resamples
confusionMatrix(DTFit)

# Decision Trees give us information 
# about Variable Importance. We can use
# the best fit object from the caret
# package to obtain variable importance
# information 
DTFit$finalModel$variable.importance


## Tuned Model Performance

### Training Performance
# We use the predict() function to generate 
# class predictions for our training data set
tune.trpreds <- predict(object = DTFit,
                        newdata = train_os,
                        type = "raw")

# We use the confusionMatrix() function
# from the caret package
DT_trtune_conf <- confusionMatrix(data = tune.trpreds, # predictions
                                  reference = train_os$Default, # actual
                                  positive = "1",
                                  mode = "everything")

DT_trtune_conf


## Testing Performance
# We use the predict() function to generate class 
# predictions for our testing data set
tune.tepreds <- predict(object = DTFit,
                        newdata = test)

# We use the confusionMatrix() function
# from the caret package
DT_tetune_conf <- confusionMatrix(data = tune.tepreds, # predictions
                                  reference = test$Default, # actual
                                  positive = "1",
                                  mode = "everything")
DT_tetune_conf


## Goodness of Fit
# To assess if the model is balanced,
# underfitting or overfitting, we compare
# the performance on the training and
# testing. We can use the cbind() function
# to compare side-by-side.

# Overall
cbind(Training = DT_trtune_conf$overall,
      Testing = DT_tetune_conf$overall)

# Class-Level
cbind(Training = DT_trtune_conf$byClass,
      Testing = DT_tetune_conf$byClass)


#---------------------- Support Vector Machines ----------------------------------

library(e1071) 

#### Transformation of variables

# Binarize factor variables

equity_imp$Reason_HE <- class2ind(x = equity_imp$Reason_HE,drop2nd = TRUE)

Occupation_dummy <- dummyVars(formula =  ~ Occupation,
                                  data = equity_imp)

Occupation_dums <- predict(object = Occupation_dummy, 
                               newdata = equity_imp)

# Merge new dummy variables into the dataset
equity_imp_dum <- data.frame(equity_imp[ ,!names(equity_imp) %in% c("Occupation")],
                             Occupation_dums)

write.csv(equity_imp_dum,"C:\\Users\\Nam Dang\\DropboX\\2022 Summer\\equityimp.csv", row.names = TRUE)

#### SVM Model

set.seed(82365323) 

# Generate the list of observations for the
# train dataframe
sub <- createDataPartition(y = equity_imp_dum$Default, 
                           p = 0.85, 
                           list = FALSE)

# Create our train and test sets
train <- equity_imp_dum[sub, ] 
test <- equity_imp_dum[-sub, ]

vars <- names(equity_imp_dum[,!names(equity_imp_dum) %in% "Default"])

train_ds <- upSample(x = train[ ,vars], 
                     y = train$Default, 
                     yname = "Attrition") 


ctrl_SVM <- trainControl(method = "repeatedcv",
                         number = 5, 
                         repeats = 3,
                         search = "random", 
                         savePredictions = TRUE)

ctrl_SVM$sampling <- "down"

set.seed(82365323)


SVMFit <- train(form = Default ~ ., 
                data = train, 
                method = "svmRadial", 
                preProcess = c("center", "scale"), 
                trControl = ctrl_SVM,
                tuneLength = 10)
SVMFit

tune.tr.preds <- predict(object = SVMFit,
                         newdata = train)

SVM_trtune_conf <- confusionMatrix(data = tune.tr.preds, 
                                   reference = train$Default, 
                                   positive = "1",
                                   mode = "everything")

tune.te.preds <- predict(object = SVMFit,
                         newdata = test)

SVM_tetune_conf <- confusionMatrix(data = tune.te.preds, 
                                   reference = test$Default, 
                                   positive = "1",
                                   mode = "everything")


cbind(Training = round(SVM_trtune_conf$overall,2),
      Testing = round(SVM_tetune_conf$overall,2))

cbind(Training = round(SVM_trtune_conf$byClass,2),
      Testing = round(SVM_tetune_conf$byClass,2))


#-----------------------------------------------------------
#####        Naive Bayes      ##########
#-----------------------------------------------------------


#install.packages("neuralnet")
library(NeuralNetTools)

# First, we obtain the correlation
cor(x = equity_imp[ ,nums])

#Mortgage balance and home value have a positive correlation
#of 0.874.

summary(lm(equity_imp$Mort_Bal ~ equity_imp$Home_Val))
#very significant relationship. Home value explains 76% of 
#the variation in mortgage balance.Not removed for now.


#Setting up target variable
equity_imp$Default <- factor(equity_imp$Default)

# Standardization for Naive Bayes
# We will use YeoJohnson as our data has a lot of 0s, combined 
# with center scale.

#cen_yjsc <- preProcess(x = mkt,
#method = c("YeoJohnson", "center", "scale"))


eq_yjcs <- preProcess(x = equity_imp,
                      method = c("YeoJohnson", "center", "scale"))

#mkt_bcs <- predict(object = cen_yjsc,
#newdata = mkt)

eq1 <- predict(object = eq_yjcs,
               newdata = equity_imp)

#-----------------------------------------------------------

## Training and Testing

# Splitting the data into training and 
# testing sets using an 85/15 split rule

# Initialize random seed
set.seed(103)

# Create list of training indices
#sub_nb <- createDataPartition(y = mkt_bcs$Response, # target variable
#p = 0.85, # % in training
#list = FALSE)

sub_nb <- createDataPartition(y = eq1$Default,
                              p = 0.85,
                              list = FALSE)

# Subset the transformed data
# to create the training (train)
# and testing (test) datasets
train_nb <- eq1[sub_nb, ] # create train dataframe
test_nb <- eq1[-sub_nb, ] # create test dataframe


#Since there is class imbalance, we will down sample our
#data

#upSample
#train_nbs <- upSample(x = train_nb[ ,vars], # predictors
#y = train_nb$Default, # target
#yname = "Default") # name of y variable

#downSample
vars <- c(facs, nums)


train_nbs <- downSample(x = train_nb[ , vars],
                        y = train_nb$Default,
                        yname = "Default")



par(mfrow = c(1,2))
plot(train_nb$Default, main = "Default: Original")
plot(train_nbs$Default, main = "Default: Down Sampled")
par(mfrow = c(1,1))

#----------------------------------------------------------

## Analysis

# We use the naiveBayes() function from the
# e1071 package to perform NB classification
#install.packages("e1071")
library("e1071")

# If we have NA values that we did not
# handle during preprocessing, we can
# use the na.action argument, which defaults
# to na.pass, which means NAs will not
# be included when calculating probabilities.
# Alternatively, na.action = na.omit can
# be specified and NAs will not be included
# in modeling.

# The default laplace value is 0, meaning
# laplace smoothing is not applied. To 
# determine if we need to use Laplace
# smoothing, we need to look for zero
# probability categories
aggregate(train_nbs[ ,facs],
          by = list(train_nbs$Default),
          FUN = table)

# Since we do not have a zero probability, 
# no smoothing is applied
nb_mod <- naiveBayes(x = train_nbs[ ,vars],
                     y = train_nbs$Default)
# laplace = 1) no zero probability
nb_mod

#-----------------------------------------------------------

### Model Performance & Fit

## Training Performance

# To assess the goodness of fit of the
# model, we compare the training and
# testing performance.

# First, we use the predict() function
# to obtain the class predictions
# (type = "class") for the training
# data based on our NB model. 

nb.train <- predict(object = nb_mod, # NB model
                    newdata = train_nbs[ ,vars], # predictors
                    type = "class")


# This creates a vector of class predictions
# for each of our training observations.
head(nb.train)

# We can use the confusionMatrix() function
# from the caret package to obtain a 
# confusion matrix and obtain performance
# measures for our model applied to the
# training dataset (train). We can set 
# mode = "everything" to obtain all available
# performance measures. We identify
# the "Responseed" class as positive, since
# that is the class we are more interested
# in being able to predict. We will save it 
# so that we can make comparisons
tr_nb_conf <- confusionMatrix(data = nb.train, # predictions
                              reference = train_nbs$Default, # actual
                              positive = "1",
                              mode = "everything")
tr_nb_conf


## Testing Performance

# To assess model performance, we focus on
# the performance of the model applied to 
# the testing set.

# We use the predict() function to obtain
# the class predictions (type = "class") 
# for the testing data based on our NB model. 
nb.test <- predict(object = nb_mod, # NB model
                   newdata = test_nb[ ,vars], # predictors
                   type = "class")

# Again, we use the confusionMatrix() 
# function from the caret package
te_nb_conf <- confusionMatrix(data = nb.test, # test predictions
                              reference = test_nb$Default, # actual
                              positive = "1",
                              mode = "everything")
te_nb_conf

# Overall Performance
# We can describe the overall performance 
# based on our accuracy and kappa values.
te_nb_conf$overall[c("Accuracy", "Kappa")]

# Class-Level Performance
# We can describe class-level performance
# for the class level of interest, Response = "Yes"
te_nb_conf$byClass


## Goodness of Fit
# We assess if the model is balanced,
# underfitting or overfitting

# Overall
cbind(Training = tr_nb_conf$overall,
      Testing = te_nb_conf$overall)

# Class-Level
cbind(Training = tr_nb_conf$byClass,
      Testing = te_nb_conf$byClass)


#-----------------------------KNN-------------------------------

# First, we obtain the correlation matrix
# for our numeric predictor variables
cor_vars <- cor(x = equity_imp[ ,nums])

# We can start by looking at the symbolic 
# correlation matrix to manually identify 
# correlated variables
symnum(x = cor_vars,
       corr = TRUE)

# We can use the findCorrelation() function 
# in the caret package to identify redundant 
# variables for us. Setting names = TRUE will 
# output a list of the variable name that are 
# determined to be redundant. We can then remove 
# them from our nums vector and exclude them 
# from our analysis.
high_corrs <- findCorrelation(x = cor_vars, 
                              cutoff = .75, 
                              names = TRUE)

# By running a code line with the name of
# the output object (high_corrs), we can
# view the names of the redundant variables
high_corrs

# Now, we can remove them from our nums
# vector so that we exclude them from our
# list of input (X) variable names

nums <- nums[!nums %in% high_corrs]
nums

## 3. Rescale Numeric Variables
# kNN has been shown to perform well with 
# min-max (range) normalization, converting
# the numeric variables to range between
# 0 and 1. We can use the preProcess()
# and predict() functions and save the 
# rescaled data as a new dataframe, 
# BH_mm.

cen_mm <- preProcess(x = equity_imp[ ,nums],
                     method = "range")
equity_mm <- predict(object = cen_mm,
                       newdata = equity_imp)
#equity_mm <- equity_mm[,-1]

## 4. Binarization
# If nominal categorical input (X) variables are 
# used in analysis, they must be converted
# to binary variables using the class2ind()
# function from the caret package for 
# categorical variables with 2 class levels and
# the dummyVars() function from the caret 
# package and the predict() function for
# categorical variables with more than 2
# class levels. 

equity_mm$Reason_HE <- class2ind(x = equity_mm$Reason_HE,drop2nd = TRUE)

Occupation_mm_dummy <- dummyVars(formula =  ~ Occupation,
                              data = equity_mm)

Occupation_mm_dums <- predict(object = Occupation_mm_dummy, 
                           newdata = equity_mm)

equity_imp_dum_mm <- data.frame(equity_mm[ ,!names(equity_mm) %in% c("Occupation")],
                                Occupation_mm_dums)

# Create vars vector of the names of the variables
# to use as input to the kNN model


vars2 <- names(equity_imp_dum_mm)[!names(equity_imp_dum_mm) %in% c("Default")] 

vars2

#--------------------------------KNN MODELING-----------------------------------
## Training & Testing
set.seed(917)
sub <- createDataPartition(y = equity_imp_dum_mm$Default, 
                           p = 0.85, 
                           list = FALSE)

# Create the train dataframe
train <- equity_imp_dum_mm[sub, ]

# Create the test dataframe
test <- equity_imp_dum_mm[-sub, ]

## Analysis

### Basic Model (knn() in the class package)

ceiling(sqrt(nrow(train)))
# 72


## Cannot run- Naive Model Building
# To build a basic (Naive) kNN model
# we can use the knn() function from the
# class package
knn.pred <- knn(train = train[ ,vars2],
                test = test[ ,vars2], 
                cl = train$Default, 
                k = 72)

conf_basic <- confusionMatrix(data = knn.pred, # vector of Y predictions
                              reference = test$Default, # actual Y
                              positive = "1", # positive class for class-level performance
                              mode = "everything") # all available measures

# We can view the output by running a code
# line with the name of the object
conf_basic



#------------------------------------------

### Hyperparameter Tuning 

# (using the train() function in the caret 
# package)

# We can perform k-fold cross validation
# for model tuning to help us choose the
# best possible hyperparameter value (k)

# Note: specifying tuneLength = 15 and no 
# particular hyperparameter search method will 
# perform a default grid search

# By default, the train() function will 
# determine the 'best' model based on Accuracy 
# for classification and RMSE for regression. For
# classification models, the Accuracy and Kappa 
# are automatically computed and provided. 

# First, we set up a trainControl object
# (named ctrl) using the trainControl() 
# function in the caret package. We specify 
# that we want to perform 10-fold cross 
# validation, repeated 3 times. We use this
# object as input to the trControl argument
# in the train() function below.
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3)

# Next, we initialize a random seed for 
# our cross validation
set.seed(917)

# Then, we use the train() function to
# train the kNN model using 10-Fold Cross 
# Validation (repeated 3 times). We set
# tuneLength = 15 to try the first 15
# default values of k (odd values from
# k = 5:33). 

# We can apply the pre-processing directly 
# during training to avoid any data leakage
# using the preProcess argument and inputting
# the original BH data.

# We can create a new set of train and test sets
# using the original data
train_o <- equity_imp_dum_mm[sub, ]
test_o <- equity_imp_dum_mm[-sub, ]

knnFit1 <- train(x = train_o[ ,vars2], # input variables, original training (original data)
                 y = train_o[ ,"Default"], # target variable, training (original data)
                 preProcess = "range", # min-max normalization
                 method = "knn", # k-nearest neighbors analysis
                 trControl = ctrl, # trainControl object
                 tuneLength = 15) # try 15 default k values
warnings()
# We can view the results of our
# cross validation across k values
# for Accuracy and Kappa. The output
# will also identify the optimal k.
knnFit1

# We can plot the train() object
# (knnFit1) using the plot() function
# to view a plot of the hyperparameter,
# k (# of neighbors), on the x-axis and 
# the Accuracy on the y-axis.
plot(knnFit1)

# We can view the confusion matrix
# showing the average performance of the model
# across resamples
confusionMatrix(knnFit1)

#------------------------------------------

### Model Performance

# Finally, we can use our tuned model to 
# predict the testing data.

# First, we use the predict() function to 
# predict the value of the median_val variable 
# using the model we created using the train()
# function, knnFit1 model and the true 
# classes of the median_val in the test 
# dataframe.
outpreds <- predict(object = knnFit1, # train object
                    newdata = test_o[ , vars2]) # input variables in test set (original data)

# Again, we can use the confusionMatrix() 
# function from the caret package to obtain a 
# confusion matrix and obtain performance
# measures for our model. We can set mode =
# "everything" to obtain all available
# performance measures
conf_tuned <- confusionMatrix(data = outpreds, 
                              reference = test_o[, "Default"], 
                              positive = "1",
                              mode = "everything")
conf_tuned

# We can describe the overall performance 
# based on our accuracy and kappa values.

conf_tuned$overall[c("Accuracy", "Kappa")]

# We can describe class-level performance
# for the different class levels. Note,
# above, we set positive = "Above", since we
# are more interested in predicting above median
# properties than below median
conf_tuned$byClass

# Note: We could run it again setting 
# positive = "Below" to get class-level
# performance for the Below class

## Comparing Base & Tuned Models

# Since we do not have a training set,
# we cannot assess goodness of fit. Instead,
# we will compare performance across the
# 'Naive' (Best Guess) and tuned models on 
# the testing data set

# Overall Model Performance
round(cbind(Base = conf_basic$overall,
      Tuned = conf_tuned$overall),2)

# Class-Level Model Performance
round(cbind(Base = conf_basic$byClass,
      Tuned = conf_tuned$byClass),2)



