rm(list=ls())

library(ggplot2)
library(caret)
library(arules)
library(arulesViz)
library(cluster) 
library(factoextra) 
library(fpc) 

setwd("C:/Users/Nam Dang/Dropbox/2022 Spring/STAT 642 - Data Mining")
data <- read.csv("EmployeeRetention.csv",stringsAsFactors = FALSE)

#### 1. Data Exploration

## Data summary
summary(data)
str(data)

## Remove irrelevant variables
data <- data[ -c(17) ] #Over18 variable and years at company

## Prepare Target (Y) Variable
data$Attrition <- factor(data$Attrition)

## Categorizing variables

facs <- c("BusinessTravel", "Department", "EducationField", "Gender", "JobRole",
          "MaritalStatus","OverTime")

ords <- c("EnvironmentSatisfaction", "JobInvolvement", "JobLevel", "JobSatisfaction",
          "PerformanceRating","RelationshipSatisfaction","StockOptionLevel",
          "WorkLifeBalance")

nums <- names(data)[!names(data) %in% c(facs, ords, "Attrition")] 


vars <- c(facs, ords, nums)


## Coverting data type

data[ ,facs] <- lapply(X = data[ , facs], 
                     FUN = factor)

data[ ,ords] <- lapply(X = data[ ,ords], 
                     FUN = factor,
                     ordered = TRUE)

## Target Variable
plot(data$Attrition, main = "Employee Attrition")

## Check for missing data
any(is.na(data))

## Checking relationship of Target Variable and a few predictors:
## Monthly income
ggplot(data = data, 
       mapping = aes(x = MonthlyIncome, y = Attrition)) +
  geom_boxplot()

## Age
ggplot(data = data, 
       mapping = aes(x = Age, y = Attrition)) +
  geom_boxplot()

## MaritalStatus
ggplot(data = data, mapping = aes(x = MaritalStatus,
                                fill = Attrition)) +
  geom_bar()


## Stock Option
ggplot(data = data, mapping = aes(x = StockOptionLevel,
                                  fill = Attrition)) +
  geom_bar()

## Job Role
ggplot(data = data, mapping = aes(x = JobRole,
                                  fill = Attrition)) +
  geom_bar()

## Job Level
ggplot(data = data, mapping = aes(x = JobLevel,
                                  fill = Attrition)) +
  geom_bar()


## Business Travel
ggplot(data = data, mapping = aes(x = BusinessTravel,
                                  fill = Attrition)) +
  geom_bar()

## work Life Balance
ggplot(data = data, mapping = aes(x = WorkLifeBalance,
                                  fill = Attrition)) +
  geom_bar()

## Overtime
ggplot(data = data, mapping = aes(x = OverTime,
                                  fill = Attrition)) +
  geom_bar()


## Employee Attrition at each Job Satisfaction Level
tab <- table(data$Attrition, data$JobSatisfaction,
             dnn = c("Attrition", "Job Satisfaction"))

barplot(height = tab,
        col = cm.colors(2),
        legend.text = TRUE,
        args.legend = list(x = "topleft"),
        xlab = "Job Satisfaction",
        ylab = "Attrition",
        main = "Employee Attrition at each Job Satisfaction Level")


#---------------------- Cluster Analysis ----------------------------------

## Normalization:
cen_data <- preProcess(x = data,
                       method = c("center", "scale"))
data_sc <- predict(object = cen_data,
                   newdata = data)

## Taking care of outliers:
outs <- sapply(data_sc[,nums], function(x) which(abs(x) > 3))
data_sc[unique(unlist(outs)),]

data_sc <- data_sc[-(unique(unlist(outs))),]
data <- data[-(unique(unlist(outs))),]

## YeoJohnson:

cen_yj <- preProcess(x = data,
                     method = "YeoJohnson")
data_yj <- predict(object = cen_yj,
                   newdata = data)

# Create a distance metric using Gower to handle mixed-type data.

load(file = "Clus_Plot_Fns.RData")

dist <- daisy(x = data_yj, metric = "gower")

sil_plot(dist_mat = dist,
         method = "hc", 
         hc.type = "ward.D2", 
         max.k = 15)
## Possible values are 2, 4 and 6.

### Hierarchical Cluster Analysis

wards <- hclust(d = dist, 
                method = "ward.D2")

plot(wards, 
     xlab = NA, sub = NA, 
     main = "Ward's Method")


## Based on the dendrogram, there could be many
## possible values for k. However, we agree that
## there seems to be 6 separate clusters, so a 
## possible value for k is 6.

rect.hclust(tree = wards, 
            k = 6, 
            border = hcl.colors(6))

wards_clusters <- cutree(tree = wards, 
                         k = 6)


# Obtain cluster centroid information 
# for Ward's HCA cluster solution

## Cluster centroid information for each cluster
## for numerical variables

aggregate(x = data_yj[ ,nums], 
          by = list(wards_clusters),
          FUN = mean)

# Cluster centroid information for each cluster
# for categorical variables

facs <- c("BusinessTravel", "Department", "EducationField", "Gender", "JobRole",
          "MaritalStatus","OverTime","Attrition")

aggregate(x = data_yj[ ,c(ords,facs)], 
          by = list(wards_clusters), 
          FUN = table)


## Based on our result, employees in cluster 3 has the highest chance of quitting
## their jobs. The majority of employees in cluster 3 are single, has no
## stock option package and has a performance rating of 3. Also, they have an average 
## of work environment satisfaction, job involvement and job satisfaction. Those
## employee group rarely travel so they are probably working in departments such as
## Research & Development or Human Resources.


cluster.stats(d = dist,
              clustering = wards_clusters, 
              alt.clustering = as.numeric(data_yj$Attrition))$corrected.rand 

## We got a result of 0.05721918 for the Adjusted Rand Index
## value, which is decent because it's closer to 1 than -1,
## but still relatively low. Therefore, this clustering solution is not good.

### k-Medoids (PAM) Cluster Analysis

set.seed(82365323)
pam <- pam(x = dist,
           diss = TRUE,
           k = 6)


## View the Medoids.
data[pam$medoids,]


#------------------------------------------

# Compare the two clustering solutions 
# using (HCA and PAM) based on the Dunn Index,
# the average distance between clusters and
# the average distance within clusters.

stats_PAM <- cluster.stats(d = dist, 
                           clustering = pam$clustering)
stats_HCA <- cluster.stats(d = dist, 
                           clustering = wards_clusters)

c_stats <- c("average.between", "average.within",
             "dunn")

cbind(HCA = stats_HCA[names(stats_HCA) %in% c_stats], 
      PAM = stats_PAM[names(stats_PAM) %in% c_stats])



#---------------------- Decision Tree ----------------------------------

##Initialize random seed
set.seed(82365323) 

##Splitting the data into training and 
## testing sets using an 80/20 split rule
## Create list of training indices
sub <- createDataPartition(y = data$Attrition,
                           p = 0.80, 
                           list = FALSE)

# Subset the transformed data
# to create the training (train)
# and testing (test) datasets
train <- data[sub, ] 
test <- data[-sub, ]


## Create grids
grids <- expand.grid(cp = seq(from = 0,
                              to = 0.05,
                              by = 0.005))
grids

## set up a trainControl object
ctrl_DT <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 3,
                     search = "grid")


set.seed(82365323)
DTFit <- train(form = Attrition ~ ., 
               data = train[ ,c(vars, "Attrition")], 
               method = "rpart", 
               trControl = ctrl_DT, 
               tuneGrid = grids) 

DTFit # all results


## Class Imbalance
set.seed(82365323)
train_us <- upSample(x = train[ ,vars], 
                     y = train$Attrition, 
                     yname = "Attrition") 
ctrl_DT$sampling <- "up"


set.seed(82365323)

DTFit_resampling <- train(form = Attrition ~ ., 
                          data = train[ ,c(vars, "Attrition")], 
                          method = "rpart", 
                          trControl = ctrl_DT, 
                          tuneGrid = grids) 

DTFit_resampling
DTFit_resampling$results[DTFit_resampling$results$cp %in% DTFit_resampling$bestTune,] #best result


#plot the cp value vs. Accuracy
plot(DTFit_resampling)

## the average performance of the model
# across resamples
confusionMatrix(DTFit_resampling)

## Important variable
DTFit_resampling$finalModel$variable.importance


## Tuned Model Performance

### Training Performance
tune.trpreds <- predict(object = DTFit_resampling,
                        newdata = train)

DT_trtune_conf <- confusionMatrix(data = tune.trpreds, 
                                  reference = train$Attrition, 
                                  positive = "Yes",
                                  mode = "everything")
DT_trtune_conf


## Testing Performance
tune.tepreds <- predict(object = DTFit_resampling,
                        newdata = test)

DT_tetune_conf <- confusionMatrix(data = tune.tepreds, 
                                  reference = test$Attrition, 
                                  positive = "Yes",
                                  mode = "everything")
DT_tetune_conf


## Goodness of Fit
# Overall
cbind(Training = round(DT_trtune_conf$overall,2),
      Testing = round(DT_tetune_conf$overall,2))

# Class-Level
cbind(Training = round(DT_trtune_conf$byClass,2),
      Testing = round(DT_tetune_conf$byClass,2))




#---------------------- Support Vector Machines ----------------------------------

library(e1071) 

#### Transformation of variables

# Binarize factor variables

data$Gender <- class2ind(x = data$Gender,drop2nd = TRUE)
data$OverTime <- class2ind(x = data$OverTime,drop2nd = TRUE)
BusinessTravel_dummy <- dummyVars(formula =  ~ BusinessTravel,
                  data = data)
Department_dummy <- dummyVars(formula =  ~ Department,
                                  data = data)
EducationField_dummy <- dummyVars(formula =  ~ EducationField,
                              data = data)
JobRole_dummy <- dummyVars(formula =  ~ JobRole,
                                  data = data)
MaritalStatus_dummy <- dummyVars(formula =  ~ MaritalStatus,
                           data = data)

# Create dummy variables:

BusinessTravel_dums <- predict(object = BusinessTravel_dummy, 
                     newdata = data)
Department_dums <- predict(object = Department_dummy, 
                               newdata = data)
EducationField_dums <- predict(object = EducationField_dummy, 
                           newdata = data)
JobRole_dums <- predict(object = JobRole_dummy, 
                               newdata = data)
MaritalStatus_dums <- predict(object = MaritalStatus_dummy, 
                        newdata = data)
cat_dums <- cbind(BusinessTravel_dums, Department_dums, EducationField_dums, JobRole_dums, MaritalStatus_dums)
data_dum <- data.frame(data[ ,!names(data) %in% c("BusinessTravel","Department",
                                                "EducationField","JobRole","MaritalStatus")],
                       cat_dums)

# Convert categorical variables into numeric:

data_dum$EnvironmentSatisfaction <- as.numeric(data_dum$EnvironmentSatisfaction)
data_dum$JobInvolvement <- as.numeric(data_dum$JobInvolvement)
data_dum$JobLevel <- as.numeric(data_dum$JobLevel)
data_dum$JobSatisfaction <- as.numeric(data_dum$JobSatisfaction)
data_dum$PerformanceRating <- as.numeric(data_dum$PerformanceRating)
data_dum$RelationshipSatisfaction <- as.numeric(data_dum$RelationshipSatisfaction)
data_dum$StockOptionLevel <- as.numeric(data_dum$StockOptionLevel)
data_dum$WorkLifeBalance <- as.numeric(data_dum$WorkLifeBalance)

vars <- names(data_dum[,!names(data_dum) %in% "Attrition"])
data_dum$Attrition <- factor(data_dum$Attrition, 
                       ordered = FALSE)

#### SVM Model

set.seed(82365323) 

# Generate the list of observations for the
# train dataframe
sub <- createDataPartition(y = data_dum$Attrition, 
                           p = 0.80, 
                           list = FALSE)

# Create our train and test sets
train <- data_dum[sub, ] 
test <- data_dum[-sub, ]

train_ds <- upSample(x = train[ ,vars], 
                       y = train$Attrition, 
                       yname = "Attrition") 


ctrl_SVM <- trainControl(method = "repeatedcv",
                     number = 5, 
                     repeats = 3,
                     search = "random", 
                     savePredictions = TRUE)

ctrl_SVM$sampling <- "down"

set.seed(82365323)


SVMFit <- train(form = Attrition ~ ., 
                data = train, 
                method = "svmRadial", 
                preProcess = c("center", "scale"), 
                trControl = ctrl_SVM,
                tuneLength = 10)
SVMFit

tune.tr.preds <- predict(object = SVMFit,
                         newdata = train)

SVM_trtune_conf <- confusionMatrix(data = tune.tr.preds, 
                                   reference = train$Attrition, 
                                   positive = "Yes",
                                   mode = "everything")

tune.te.preds <- predict(object = SVMFit,
                         newdata = test)

SVM_tetune_conf <- confusionMatrix(data = tune.te.preds, 
                                   reference = test$Attrition, 
                                   positive = "Yes",
                                   mode = "everything")


cbind(Training = round(SVM_trtune_conf$overall,2),
        Testing = round(SVM_tetune_conf$overall,2))

cbind(Training = round(SVM_trtune_conf$byClass,2),
      Testing = round(SVM_tetune_conf$byClass,2))

