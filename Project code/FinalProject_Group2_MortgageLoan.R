#----------------------------------------------------------------------------- #
# ---------------- MGMT-715-676 Business Consulting Projects ----------------- #
# ------------------------ Credit Risk Analysis ------------------------------ #
# ------------------------------- Group 2 ------------------------------------ #
# ---------- Nam Dang, Aditee Bhattarai, Jingxin Yao, Zihan Huang ------------ #
#----------------------------------------------------------------------------- #

#-------------------------------------------------------------------------------
## Preliminary Code
#-------------------------------------------------------------------------------
#Clear the workspace
rm(list=ls())

# Setting working directory
setwd("D:/Courses/MGMT-715/Project")

# Loading packages
library(caret)
library(rpart)
library(rpart.plot)
library(e1071) 

# Loading the data
mortgage <- read.csv("loan_mortgage.csv")

#-------------------------------------------------------------------------------
## Mortgage Data Aggregation
#-------------------------------------------------------------------------------

# Structure of the data
str(mortgage)

#Aggregte by maximum time
mortgage_agg = aggregate(mortgage,
                by = list(mortgage$MID),
                FUN = max)[,-1]

# Create new variables
mortgage_agg$LTV_time_mean <- aggregate(mortgage$LTV,
                                        list(mortgage$MID), FUN = mean)[,2]
mortgage_agg$LTV_time_std <- aggregate(mortgage$LTV,
                                       list(mortgage$MID), FUN = sd)[,2]
mortgage_agg$balance_mean <- aggregate(mortgage$balance,
                                       list(mortgage$MID), FUN = mean)[,2]
mortgage_agg$balance_std <- aggregate(mortgage$balance,
                                      list(mortgage$MID), FUN = sd)[,2]
mortgage_agg$interest_rate_time_mean <- aggregate(mortgage$interest_rate,
                                                  list(mortgage$MID), FUN = mean)[,2]
mortgage_agg$interest_rate_time_std <- aggregate(mortgage$interest_rate,
                                                 list(mortgage$MID), FUN = sd)[,2]
mortgage_agg$hpi_time_mean <- aggregate(mortgage$hpi,
                                        list(mortgage$MID), FUN = mean)[,2]
mortgage_agg$hpi_time_std <- aggregate(mortgage$hpi,
                                       list(mortgage$MID), FUN = sd)[,2]
mortgage_agg$gdp_time_mean <- aggregate(mortgage$gdp,
                                        list(mortgage$MID), FUN = mean)[,2]
mortgage_agg$gdp_time_std <- aggregate(mortgage$gdp,
                                       list(mortgage$MID), FUN = sd)[,2]
mortgage_agg$uer_time_mean <- aggregate(mortgage$uer,
                                        list(mortgage$MID), FUN = mean)[,2]
mortgage_agg$uer_time_std <- aggregate(mortgage$uer,
                                       list(mortgage$MID), FUN = sd)[,2]

# Omit origin variables
dropcolumns <- c("LTV","balance","interest_rate","hpi","gdp","uer")
mortgage_agg = mortgage_agg[,!(names(mortgage_agg)%in% dropcolumns)]

# Select the uncensored data
mortgage_agg <- mortgage_agg[mortgage_agg$first_ob_time == mortgage_agg$origin_time,]

#-------------------------------------------------------------------------------
## Data Exploration
#-------------------------------------------------------------------------------

# Stucture of sample data 
str(mortgage_agg)

# Summary of data
summary(mortgage_agg)

# Transform target variable
mortgage_agg[mortgage_agg$status_time==2,]$status_time <- 0

# Nominal Variables
facs <- c("investor","RE_Type","status_time")

# There are no ordinal variables

# Irrelavant variables
irres <- c("MID","default","payoff",'origin_time')

# Omit irrelavant variables
mortgage_agg = mortgage_agg[,!(names(mortgage_agg)%in% irres)]

# Numerical Vairables
nums <- names(mortgage_agg)[!names(mortgage_agg) %in% c(facs,irres)] 

# Predictor Variables
vars <- c(facs,nums)
vars <- vars[!vars %in% "status_time"]

# Convert the nominal variables 
# (investor is already in binary form, so we left it be)
mortgage_agg[, facs[-1]] <- lapply(X = mortgage_agg[, facs[-1]],
                               FUN = factor)
# Standard Deviation
lapply(X = mortgage_agg[, nums], FUN = sd)

# Modes
modefun <- function(x){
  if(any(tabulate(match(x, unique(x))) > 1)){
    outp <- unique(x)[which(tabulate(match(x, unique(x))) == max(tabulate(match(x, unique(x)))))]
  } else {
    outp <- "No mode exists!"}
  return(outp)
}

lapply(X = mortgage_agg[,c(facs,"default","payoff")], FUN =  modefun)

#-------------------------------------------------------------------------------
# Data Preprocessing
#-------------------------------------------------------------------------------

## Missing Data Detection and Processing
na_rows <- rownames(mortgage_agg)[!complete.cases(mortgage_agg)]
na_rows

# Impute missing values with 0
mortgage_agg[is.na(mortgage_agg)] <- 0

# Duplicate values
mortgage_agg[duplicated(x = mortgage_agg), ] #no duplicate values

# Outliers detection and handling
outs <- sapply(mortgage_agg[,vars[-c(1,2)]], function(x) which(abs(scale(x)) > 3))
outs
# We will not handle outliers here

# Handle sparse variables (More than 20% of observations are 0)
nrow(mortgage_agg[mortgage_agg$interest_rate_time_std==0,])
mortgage_agg[mortgage_agg$interest_rate_time_std!=0,]$interest_rate_time_std <- 1

nrow(mortgage_agg[mortgage_agg$balance_std==0,])
mortgage_agg[mortgage_agg$balance_std!=0,]$balance_std <- 1

nrow(mortgage_agg[mortgage_agg$Interest_Rate_orig_time==0,])
mortgage_agg[mortgage_agg$Interest_Rate_orig_time!=0,]$Interest_Rate_orig_time <- 1

## Prepare Training and Testing data

# Initial random seed
set.seed(333)

# Create list of training indices
sub <- createDataPartition(y = mortgage_agg$status_time,
                           p = 0.8, # 80% in training
                           list = FALSE)

# Subset the transformed data 
# to create the training (train)
# and testing (test) datasets
train <- mortgage_agg[sub, ] 
test <- mortgage_agg[-sub, ]

#-------------------------------------------------------------------------------
## Prepare data for Deep Learning
#-------------------------------------------------------------------------------
# Redundant Variables
cor_vars <- cor(x = mortgage_agg[ ,nums])

high_corrs <- findCorrelation(x = cor_vars, 
                              cutoff = .75, 
                              names = TRUE)
high_corrs # view variable names for removal

# Remove redundant variables
mortgage_nohigh = mortgage_agg[,!(names(mortgage_agg)%in% high_corrs)]

vars_nohigh <- vars[!vars %in% high_corrs]

# Create dummy variable for RE_Type
cats <- dummyVars(formula =  ~ RE_Type,
                  data = mortgage_nohigh)
cats_dums <- predict(object = cats, 
                     newdata = mortgage_nohigh)

# Combine binarized variables (cats_dum) with data
# (OJ), excluding the StoreID factor variable
mortgage_dum <- data.frame(mortgage_nohigh[ ,!names(mortgage_nohigh) %in% "RE_Type"],
                           cats_dums)

vars_dum <- c(vars_nohigh[-2], colnames(cats_dums))

# Rescale Numerical Variables
mmnorm <- preProcess(x = mortgage_dum[,vars_nohigh[-c(1,2)]],
                     method = "range")
mortgage_dum_mm <- predict(object = mmnorm,
                           newdata = mortgage_dum)

# save data for deep learning
write.csv(mortgage_dum_mm, 'mortgage_preprocessed.csv')

#-------------------------------------------------------------------------------
## Decision Tree Model Training and Performance Analysis
#-------------------------------------------------------------------------------
## Basic Decision Tree Model
mortgage.rpart <- rpart(formula = status_time ~ .,
                     data = train[ ,c(vars, "status_time")], 
                     method = "class")

# Model results
mortgage.rpart

# Visualize decision tree model
rpart.plot(mortgage.rpart,
           extra = 2)

## Basic Model Performance
# Generate class prediction for training set
base.trpreds <- predict(object = mortgage.rpart, 
                        newdata = train, 
                        type = "class")

# Obtain performance information
mortgage_base_train_conf <- confusionMatrix(data = base.trpreds, 
                                         reference = train$status_time,
                                         positive = "1",
                                         mode = "everything")
mortgage_base_train_conf

# Generate class prediction for testing set
base.tepreds <- predict(object = mortgage.rpart, 
                        newdata = test,
                        type = "class")

# Obtain performance information
mortgage_base_test_conf <- confusionMatrix(data = base.tepreds, 
                                        reference = test$status_time,
                                        positive = "1",
                                        mode = "everything")
mortgage_base_test_conf

## Goodness of Fit
# Overall
cbind(Training = mortgage_base_train_conf$overall,
      Testing = mortgage_base_test_conf$overall)

# Class-Level
cbind(Training = mortgage_base_train_conf$byClass,
      Testing = mortgage_base_test_conf$byClass)

## Hyperparameter Model Tuning 
# Identify target variable
target_var <- train$status_time

# Calculate the weights
weights <- c(sum(table(target_var))/(nlevels(target_var)*table(target_var)))
weights

# Set the weight vector
wghts <- weights[match(x = target_var, 
                       table = names(weights))]

# Set up a grid for a grid search
grids <- expand.grid(cp = seq(from = 0,
                              to = 0.2, # Search values from 0 to 0.2
                              by = 0.001))

# Set a 10-fold cross validation grid search 
DT_ctrl <- trainControl(method = "repeatedcv",
                        number = 10, 
                        repeats = 3, 
                        search = "grid")

# Set randomm seed
set.seed(210)

# Tune the model
mortgageDTFit <- train(form = status_time ~ ., 
                       data = train,
                       method = "rpart", 
                       trControl = DT_ctrl, 
                       tuneGrid = grids,
                       weights = wghts)

# Check the best model
mortgageDTFit

#Variable importance
mortgageDTFit$finalModel$variable.importance


## Hyperparameter Tuned Model Performance
# Generate class prediction for training set
DT.trpreds <- predict(object = mortgageDTFit, 
                      newdata = train)

# Obtain performance information
DT_train_conf <- confusionMatrix(data = DT.trpreds, 
                                 reference = train$status_time,
                                 positive = "1",
                                 mode = "everything")
DT_train_conf

# Generate class prediction for testing set
DT.tepreds <- predict(object = mortgageDTFit, 
                      newdata = test)

# Obtain performance information
DT_test_conf <- confusionMatrix(data = DT.tepreds, 
                                reference = test$status_time,
                                positive = "1",
                                mode = "everything")
DT_test_conf

## Goodness of Fit
# Overall
cbind(Training = DT_train_conf$overall,
      Testing = DT_test_conf$overall)

# Class-Level
cbind(Training = DT_train_conf$byClass,
      Testing = DT_test_conf$byClass)

#-------------------------------------------------------------------------------
## Support Vector Machines
#-------------------------------------------------------------------------------
# Prepare data
# Use dummy variables to make nominal variables are in numerical form
mortgage_svm <- data.frame(mortgage_agg[ ,!names(mortgage_agg) %in% "RE_Type"],
                           cats_dums)

vars_svm <- c(vars[-1], colnames(cats_dums))

# Prepare train and test data
train_svm <- mortgage_svm[sub,]
test_svm <- mortgage_svm[-sub,]

# Set up control object
ctrl_SVM <- trainControl(method = "repeatedcv",
                         number = 5, 
                         repeats = 3,
                         search = "random")

# Set random seed
set.seed(210)

# Tune the model
SVMFit <- train(form = status_time ~ ., 
                data = train_svm, 
                method = "svmRadial", 
                preProcess = c("center", "scale"), 
                trControl = ctrl_SVM,
                tuneLength = 10,
                weights = wghts)

# Check the best model
SVMFit

# Generate class prediction for training set
tune.tr.preds <- predict(object = SVMFit,
                         newdata = train_svm)

# Obtain performance information
SVM_trtune_conf <- confusionMatrix(data = tune.tr.preds, 
                                   reference = train_svm$status_time, 
                                   positive = "1",
                                   mode = "everything")
SVM_trtune_conf

# Generate class prediction for testing set
tune.te.preds <- predict(object = SVMFit,
                         newdata = test_svm)

# Obtain performance information
SVM_tetune_conf <- confusionMatrix(data = tune.te.preds, 
                                   reference = test_svm$status_time, 
                                   positive = "1",
                                   mode = "everything")
SVM_tetune_conf

## Goodness of Fit
# Overall
cbind(Training = round(SVM_trtune_conf$overall,4),
      Testing = round(SVM_tetune_conf$overall,4))

# Class-Level
cbind(Training = round(SVM_trtune_conf$byClass,4),
      Testing = round(SVM_tetune_conf$byClass,4))

#-------------------------------------------------------------------------------
## k-Nearest Neighbors
#-------------------------------------------------------------------------------
#Prepare the data

# We will use data prepared for deep learning, in which redundant variables were 
# removed, dummy variables were created. Normalization will be performed in
# model tuning.

# Prepare train and test data
train_knn <- mortgage_dum[sub,]
test_knn <- mortgage_dum[-sub,]

# Hyperparameter tuning

# Set up a control object
knn_ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3)

# Set randon seed
set.seed(917)

# Tune the model
knnFit1 <- train(x = train_knn[ ,vars_dum], # input variables, original training (original data)
                 y = train_knn[ ,"status_time"], # target variable, training (original data)
                 preProcess = "range", # min-max normalization
                 method = "knn", # k-nearest neighbors analysis
                 trControl = knn_ctrl, # trainControl object
                 tuneLength = 15, # try 15 default k values
                 weights = wghts) 

# View the best model
knnFit1 #k = 7

# view a plot of the hyperparameter k
plot(knnFit1)

# View the confusion matrix
confusionMatrix(knnFit1)

# Generate class prediction for training set
knn.trpreds <- predict(object = knnFit1, # train object
                    newdata = train_knn[ , vars_dum]) # input variables in test set (original data)

# Obtain performance information
knn_tr_tuned <- confusionMatrix(data = knn.trpreds, 
                              reference = train_knn[, "status_time"], 
                              positive = "1",
                              mode = "everything")
knn_tr_tuned

# Generate class prediction for testing set
knn.tepreds <- predict(object = knnFit1, # train object
                       newdata = test_knn[ , vars_dum]) # input variables in test set (original data)

# Obtain performance information
knn_te_tuned <- confusionMatrix(data = knn.tepreds, 
                                reference = test_knn[, "status_time"], 
                                positive = "1",
                                mode = "everything")
knn_te_tuned

## Goodness of Fit
# Overall
cbind(Training = round(knn_tr_tuned$overall,4),
      Testing = round(knn_te_tuned$overall,4))

# Class-Level
cbind(Training = round(knn_tr_tuned$byClass,4),
      Testing = round(knn_te_tuned$byClass,4))

#-------------------------------------------------------------------------------

# Save the workspace
save.image('FinalProject_Group2_MortgageLoan.RData')
