#-----------------------------------------------------------
#####     Consulting Project        ###########
#-----------------------------------------------------------
#####        Home Equity             #########
#-----------------------------------------------------------

# Clearing the environment
rm(list=ls())


#Setting Working Directory
setwd("D:/Drexel2021/Course/Summer/Consulting/Data")

#Reading target file, importing blank spaces as NA
equity <- read.csv(file = "loan_home_equity.csv",
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
###Getting NA values coz lots of missing value. Need to run
#code after treating missing values.
sapply(X = equity2_noNA[ , nums],
       FUN = sd)

#Exploring categorical variables through 1-way frequency table
lapply(X = equity[ ,facs], FUN = table)

table(x = equity$Default )

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

# Target variable scale
plot(equity$Default, 
     main = "Equity Loan Status")

SLtab <- table(equity$Default)
barp <- barplot(SLtab,
                main = "Bar Plot of the Target Variable: Equity Loan Status",
                col = c("azure", "cadetblue"),
                names.arg = c( "No Default", "Default"))

text(barp, SLtab/2, labels = round(SLtab, digits = 2))


##Plotting the target variable
ct3 <- table(Default = equity$Default)
Default.No_Default = table(equity$Default)

barplot(height = Default.No_Default,
     main = "Distribution of Target Variable: Default",
     xlab = "Default",
     col = c("azure","cadetblue"),
     legend.text = c("0: No Default", "1: Default"))
    

##Boxplots
#----------------------------------------------------------
#Loan Amount
boxplot(x = equity$Loan_Amount, 
        main="Boxplot for Loan Amount for Home Equity",
        col = "cadetblue2",
        horizontal = TRUE)

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

#Home Value
boxplot(x = equity$Home_Val,
        main = "Boxplot for Home Value",
        horizontal = TRUE,
        col = "cadetblue2")

## Grouped Barplots
#-----------------------------------------------------------
ct1
barplot(height = ct1,
        col = c("cadetblue","cadetblue2"),
        legend.text = c("0: No Default","1: Default"),
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

sum(is.na(equity))
#5277

na_rows <- rownames(equity)[!complete.cases(equity)]
na_rows

##Using mice to impute missing values

init = mice(equity, maxit=0) 
set.seed(103)
equity_imp = mice(equity, m=5)
equity_imp <- complete(equity_imp)

sapply(equity_imp, function(x) sum(is.na(x)))

summary(equity_imp)

#-----------------------------------------------------------
#######   Cluster Analysis: K-Medoids   #########
#-----------------------------------------------------------

## Preprocessing for Cluster analysis

##Setting up vectors to transform numerical variables with
#limited range of values to categorical variables to get 
#better cluster performance.

newfacs <- c("Num_Derog","Num_Delinq",
             "Num_Inq")

# Using lapply() to see what unique values the numerical 
#variables take
lapply(X = equity_imp[ ,newfacs], 
       FUN = unique) 

eq1<-equity_imp
#Working in eq1 as we don't want to accidentally alter
# the main working file


###Due to a large number of 0 values, we are splitting 
#these variables in 0 and non 0 categories.

#Code reference for splitting data into levels:
#https://pubs.wsb.wisc.edu/academics/analytics-using-r-2019/convert-numerical-data-to-categorical.html

##Num Derog
summary(object = eq1$Num_Derog)

#Splitting into levels
eq1 <- within(eq1, {   
  Num_Derog_Disc <- NA # need to initialize variable
  Num_Derog_Disc[Num_Derog <= 0] <- "0"
  Num_Derog_Disc[Num_Derog >= 1] <- "Not 0"
} )

#Converting to factor
eq1$Num_Derog_Disc <- factor(eq1$Num_Derog_Disc)

table(eq1$Num_Derog_Disc)


# We can visualize the Continuous and
# Discretized Variables side-by-side
par(mfrow = c(1,2))
hist(x = eq1$Num_Derog, 
     main = "Number of Derogatory Reports", 
     xlab = "")
plot(x = eq1$Num_Derog_Disc, 
     main = "Derogatory Reports Disc",
     xlab = "")
par(mfrow = c(1,1))



###Num_Delinq

summary(object = eq$Num_Delinq)

#Splitting into levels
eq1 <- within(eq1, {   
  Num_Delinq_Disc <- NA # need to initialize variable
  Num_Delinq_Disc[Num_Delinq <= 0] <- "0"
  Num_Delinq_Disc[Num_Delinq >= 1] <- "Not 0"
} )

#Converting to Factor
eq1$Num_Delinq_Disc <- factor(eq1$Num_Delinq_Disc)


table(eq1$Num_Delinq_Disc)


# We can visualize the Continuous and
# Discretized Variables side-by-side
par(mfrow = c(1,2))
hist(x = eq1$Num_Derog, 
     main = "Delinquent-Numeric", 
     xlab = "")
plot(x = eq1$Num_Delinq_Disc, 
     main = "Delinquent - Factor",
     xlab = "")
par(mfrow = c(1,1))


##Num Inq

summary(object = eq1$Num_Inq)
#We will split this one at the meadian.

#Splitting into levels
eq1 <- within(eq1, {   
  Num_Inq_Disc <- NA # need to initialize variable
  Num_Inq_Disc[Num_Inq <= 1] <- "1 or Less"
  Num_Inq_Disc[Num_Inq >= 2] <- "2 or More"
} )

#Converting to Factor
eq1$Num_Inq_Disc <- factor(eq1$Num_Inq_Disc)


table(eq1$Num_Inq_Disc)


# We can visualize the Continuous and
# Discretized Variables side-by-side
par(mfrow = c(1,2))
hist(x = eq1$Num_Inq, 
     main = "Number of Inquiry - Numerical", 
     xlab = "")
plot(x = eq1$Num_Inq_Disc, 
     main = "Number of Inquiry - Factor",
     xlab = "")
par(mfrow = c(1,1))


# We can combine our dummy variables with
# our original data using the cbind()
# function
eq_dum <- eq1[ ,-c(7,8,10)]

newfacs1 <- c("Num_Derog_Disc","Num_Delinq_Disc","Num_Inq_Disc")

# Normalize variables
cen_cs <- preProcess(x = eq_dum,
                     method = c("center", "scale"))
eq_cs <- predict(object = cen_cs,
                 newdata = eq_dum)

# Standardize numerical variables 
eq_yj <- preProcess(x = eq_cs,
                    method = "YeoJohnson")
eq_csyj <- predict(object = eq_yj,
                   newdata = eq_dum)

nums3<- c("Loan_Amount", "Home_Val", "Mort_Bal", 
          "CL_Age","Debt_Inc","YOJ","Num_CL")
vars3 <- c(facs, newfacs1, nums3)

# Distance metric: Gower distance
# Since we have mixed data, we use gower 
# distance. Gower does better with normally
# distributed numerical variables

hdist <- daisy(x = eq_csyj[, vars3], 
               metric = "gower")
summary(hdist)


# Loading Clus_Plot_Fns.RData filefor sil_plot() function
# to create a silhouette plot and wss_plot()
# function to create a WSS plot.

load("Clus_Plot_Fns.RData")


##Silhouette Plot
sil_plot(dist_mat = hdist, # distance matrix
         method = "pam", # PAM
         max.k = 15, # maximum k value
         seed_no = 103) # Create a plot of average Silhouette

#Possible k values: 15, 2, 3, 8


##WSS Plot
wss_plot(dist_mat = hdist, # distance matrix object
         method = "pam", # PAM
         max.k = 15, # maximum k value
         seed_no = 103) # seed value for set.seed()

# Possible elbows at k = 3, 5, 8

##Since both methods have 8 as a possible value, we will go
#with k = 8.


#Initializing Random Seed
set.seed(103)

# We use the pam() function from the cluster
# package. We use the gower distance matrix
# (hdist) as input, set diss = TRUE since we
# are using a custom distance matrix

pam2 <- pam(x = hdist,
            diss = TRUE,
            k = 8) # 8 cluster solution
pam2


# Viewing the rows representing mediods 
eq_dum[pam2$medoids, ]

# Averages
clus_means_PAM <- aggregate(x = eq_csyj[ ,nums3], 
                            by = list(pam2$clustering), 
                            FUN = mean)
clus_means_PAM

#write.csv(clus_means_PAM, "PAMMeans8.csv")

#Cluster centers
clus_cens_PAM <- eq_csyj[pam2$medoids,nums3]
clus_cens_PAM

#write.csv(clus_cens_PAM, "PAMCens8.csv")

#Categorical Variables 
obs8<- aggregate(x = eq_dum[ ,c(newfacs1,facs)], 
                 by = list(pam2$clustering), 
                 FUN = table)
#write.csv(obs8,"Obs8.csv")

# We can use the matplot() function to
# visualize the (scaled) cluster centers
# to observe differences
matplot(t(clus_cens_PAM), 
        type = "l", # line plot ("l")
        ylab = "", # no y-axis label
        xlim = c(0, 9), # add space for legend
        xaxt = "n", # no x-axis
        col = 1:5, # 6 colors, k = 5
        lty = 1:5, # 6 line types, k = 5
        main = "PAM Cluster Centers") # main title
# Add custom x-axis labels
axis(side = 1, # x-axis
     at = 1:7, # x values 1-9 (9 numeric variables)
     labels = nums3, # variable names as labels
     las = 2) # flip text to vertical
# Add legend
legend("left", # left position
       legend = 1:8, # 6 lines, k = 6
       col = 1:8, # 6 colors, k = 6
       lty = 1:8, # 6 line types, k = 6
       cex = 0.6) # reduce text size

# k-Medoids Clustering (PAM, k = 8)
cluster.stats(d = hdist, # distance matrix
              clustering = pam2$clustering, # cluster assignments
              alt.clustering = as.numeric(eq_dum$Default))$corrected.rand

##Corrected RAND: 0.012

# Obtain Cluster Statistics for PAM
stats_PAM <- cluster.stats(d = hdist, 
                           clustering = pam2$clustering)
c_stats <- c("average.between", "average.within", "dunn")

PAM = stats_PAM[names(stats_PAM) %in% c_stats]

PAM
#write.csv(PAM, "PAMStats8.csv")

# k-Medoids Clustering (PAM, k = 2)
t2<- table(Default = eq_dum$Default,
           Clusters = pam2$clustering)
t2

#write.csv(t2,"ExternalPAM8.csv")

#-----------------------------------------------------------
######     Decision Tree    ################### 
#-----------------------------------------------------------

library(rpart)
library(rpart.plot)


## Preprocessing & Transformation

## Training and Testing
# Splitting the data into training and 
# testing sets using an 80/10 split rule


# Initialize random seed
set.seed(103) 

# Create list of training indices (85-15 partition)
sub <- createDataPartition(y = equity_imp$Default, # target variable
                           p = 0.85, # % in training
                           list = FALSE)

# Subset the transformed data
# to create the training (train)
# and testing (test) datasets
train <- equity_imp[sub, ] # create train dataframe
test <- equity_imp[-sub, ] # create test dataframe

## Class Imbalance 

#Plotting the target variable Default to check for class imbalance
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


#Oversampling takes the minority class from 1000 to 4000, and
#undersampling takes the majority class from 4000 to 1000. 
#As this might give us a biased observation, we will go
# with class weighting.

#Class Weighting
target_var <- train$Default

weights <- c(sum(table(target_var))/(nlevels(target_var)*table(target_var)))
weights

#Giving 0.625 weightage to 0 and 2.505 weightage to 1.

wghts <- weights[match(x = target_var,
                       table = names(weights))]


set.seed(103) # initialize random seed

DTFit_cw <- train(x = train[ ,vars],
                  y = train$Default,
                  method = "rpart", # use the rpart package
                  trControl = ctrl_DT, # control object
                  tuneLength = 5, # try 5 cp values
                  weights = wghts) # identify vector of case weights


## Analysis

# 1. Basic Model 
# We used the rpart() function in the rpart 
# package to perform basic DT classification 
# on our training dataset.

MR.rpart <- rpart(formula = Default ~ .,# Y ~ all other variables in dataframe
                  data = train[ ,c(vars, "Default")], # include only relevant variables
                  method = "class")

# We can see the basic output of our
# Decision Tree model
MR.rpart

#Variable Importance
MR.rpart$variable.importance

## Tree Plots
prp(x = MR.rpart, # rpart object
    extra = 2) # include proportion of correct predictions

## Training Performance
base.trpreds <- predict(object = MR.rpart, # DT model
                        newdata = train, # training data
                        type = "class") # class predictions

#confusionMatrix() function
DT_train_conf <- confusionMatrix(data = base.trpreds, # predictions
                                 reference = train$Default, # actual
                                 positive = "1",
                                 mode = "everything")
DT_train_conf


## Testing Performance
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

# Overall
cbind(Training = DT_train_conf$overall,
      Testing = DT_test_conf$overall)

# Class-Level
cbind(Training = DT_train_conf$byClass,
      Testing = DT_test_conf$byClass)

##Model is somewhat balanced but not the best accuracy and kappa.

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


ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     search = "grid")
ctrl

#Initialize a random seed 
set.seed(103)

# Then, we use the train() function to
# train the DT model using 5-Fold Cross 
# Validation (repeated 3 times). 

DTFit <- train(form = Default ~ ., # use all variables in data to predict delay
               data = train[ ,c(vars, "Default")], # include only relevant variables
               method = "rpart", # use the rpart package
               trControl = ctrl, # control object
               tuneGrid = grids,
               weights = wghts) # custom grid object for search


DTFit


DTFit$results[DTFit$results$cp %in% DTFit$bestTune,] #best result


# We can plot the cp value vs. Accuracy
plot(DTFit)

# confusion matrix 
confusionMatrix(DTFit)

#  Variable Importance
DTFit$finalModel$variable.importance


## Tuned Model Performance

### Training Performance
tune.trpreds <- predict(object = DTFit,
                        newdata = train,
                        type = "raw")

#  confusionMatrix() function
DT_trtune_conf <- confusionMatrix(data = tune.trpreds, # predictions
                                  reference = train$Default, # actual
                                  positive = "1",
                                  mode = "everything")

DT_trtune_conf


## Testing Performance
tune.tepreds <- predict(object = DTFit,
                        newdata = test)


DT_tetune_conf <- confusionMatrix(data = tune.tepreds, # predictions
                                  reference = test$Default, # actual
                                  positive = "1",
                                  mode = "everything")
DT_tetune_conf


## Goodness of Fit
# Overall
cbind(Training = DT_trtune_conf$overall,
      Testing = DT_tetune_conf$overall)

# Class-Level
cbind(Training = DT_trtune_conf$byClass,
      Testing = DT_tetune_conf$byClass)

##DT is overfitting. Important variables noted.

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
#the variation in mortgage balance.leaving it be for now.

equity2 <- equity_imp

#Setting up target variable
equity2$Default <- factor(equity2$Default)

# Standardization for Naive Bayes
# We will use YeoJohnson as our data has a lot of 0s, combined 
# with center scale.

eqnb_yjcs <- preProcess(x = equity2,
                      method = c("YeoJohnson", "center", "scale"))

eq2 <- predict(object = eqnb_yjcs,
               newdata = equity2)

#-----------------------------------------------------------

## Training and Testing

# Splitting the data into training and 
# testing sets using an 80/20 split rule

# Initialize random seed
set.seed(103)

# Create list of training indices
sub_nb <- createDataPartition(y = eq2$Default,
                              p = 0.85,
                              list = FALSE)

# Subset the transformed data
# to create the training (train)
# and testing (test) datasets
train_nb <- eq2[sub_nb, ] # create train dataframe
test_nb <- eq2[-sub_nb, ] # create test dataframe


#Since there is class imbalance, we will down sample our
#data

#downSample
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
nb.train <- predict(object = nb_mod, # NB model
                    newdata = train_nbs[ ,vars], # predictors
                    type = "class")


head(nb.train)

#  confusionMatrix() function

tr_nb_conf <- confusionMatrix(data = nb.train, # predictions
                              reference = train_nbs$Default, # actual
                              positive = "1",
                              mode = "everything")
tr_nb_conf


## Testing Performance
 
nb.test <- predict(object = nb_mod, # NB model
                   newdata = test_nb[ ,vars], # predictors
                   type = "class")

# Again, we use the confusionMatrix() 
te_nb_conf <- confusionMatrix(data = nb.test, # test predictions
                              reference = test_nb$Default, # actual
                              positive = "1",
                              mode = "everything")
te_nb_conf

# Overall Performance
te_nb_conf$overall[c("Accuracy", "Kappa")]

# Class-Level Performance
te_nb_conf$byClass


## Goodness of Fit

# Overall
cbind(Training = tr_nb_conf$overall,
      Testing = te_nb_conf$overall)


# Class-Level
cbind(Training = tr_nb_conf$byClass,
      Testing = te_nb_conf$byClass)

#Underfitting. Need better model.

#-----------------------------------------------------------
####       kNN         ############
#-----------------------------------------------------------

## Install new packages
library(class) # knn base (untuned) model


# Redundant Variables

# We can use the findCorrelation() function 
# in the caret package to identify redundant 
# variables for us.
high_corrs <- findCorrelation(x = cor_vars, 
                              cutoff = .75, 
                              names = TRUE)

high_corrs

# Now, we can remove them from our nums
# vector so that we exclude them from our
# list of input (X) variable names

numsk <- nums[!nums %in% high_corrs]
numsk

eq3 <- equity_imp #creating a working dataset

# Using min-max (range) normalization, converting
# the numeric variables to range between
# 0 and 1. 

cen_mm <- preProcess(x = eq3[ ,numsk],
                     method = "range")
eq_mm <- predict(object = cen_mm,
                 newdata = eq3)

## Binarization of categorical variable

#Reason_HE
eq3$Reason_HE_bin <- class2ind(x = eq3$Reason_HE, 
                            drop2nd = TRUE)

#Occupation
eq31 <- dummyVars(formula = ~ Occupation,
                 data = eq3)
eqdum1 <- predict(object = eq31, 
                  newdata = eq3)

##Combining  dummy variable with our original dataset
eq_dum1 <- cbind(eq3, eqdum1)

#Removing redundant
eq_dum1 <- eq_dum1[ , -c(4,5)]

# Create vars vector of the names of the variables
# to use as input to the kNN model
vars4 <- nums
vars4

#------------------------------------------

## Training & Testing

set.seed(103)
# Generate the list of observations for the
# train dataframe
subknn <- createDataPartition(y = eq_mm$Default, 
                           p = 0.85, 
                           list = FALSE)

# Subset 
traink <- eq_mm[subknn, ] 

# Use all observations not in the sub object
# to create the test dataframe
testk <- eq_mm[-subknn, ]

#------------------------------------------

## Analysis

### Basic Model (knn() in the class package)

## First, we can try using a 'best guess' 
# value of k (square root of the number 
# of training observations)
ceiling(sqrt(nrow(traink)))
# 72

## Naive Model Building
# To build a basic (Naive) kNN model
# we can use the knn() function from the
# class package
knn.pred <- knn(train = traink[ ,vars4],
                test = testk[ ,vars4], 
                cl = traink$Default, 
                k = 72)


# confusionMatrix() function
conf_basic <- confusionMatrix(data = knn.pred, # vector of Y predictions
                              reference = testk$Default, # actual Y
                              positive = "1", # positive class for class-level performance
                              mode = "everything") # all available measures
conf_basic

#-----------------------------------------------------------

### Hyperparameter Tuning 

# We can perform k-fold cross validation
# for model tuning to help us choose the
# best possible hyperparameter value (k)


# traincontrol

ctrlk <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3)

# Next, we initialize a random seed for 
# our cross validation
set.seed(103)


# We can create a new set of train and test sets
# using the original data
train_o <- eq3[sub, ]
test_o <- eq3[-sub, ]

knnFit1 <- train(x = train_o[ ,vars4], # input variables, original training (original data)
                 y = train_o[ ,"Default"], # target variable, training (original data)
                 preProcess = "range", # min-max normalization
                 method = "knn", # k-nearest neighbors analysis
                 trControl = ctrlk, # trainControl object
                 tuneLength = 15) # try 15 default k values

#Cross Validation
knnFit1


plot(knnFit1)

# We can view the confusion matrix
# showing the average performance of the model

confusionMatrix(knnFit1)

#------------------------------------------

### Model Performance

# Finally, we can use our tuned model to 
# predict the testing data.

outpreds <- predict(object = knnFit1, # train object
                    newdata = test_o[ , vars4]) # input variables in test set (original data)


conf_tuned <- confusionMatrix(data = outpreds, 
                              reference = test_o[, "Default"], 
                              positive = "1",
                              mode = "everything")
conf_tuned

# We can describe the overall performance 
# based on our accuracy and kappa values.

conf_tuned$overall[c("Accuracy", "Kappa")]

conf_tuned$byClass

## Comparing Base & Tuned Models


# Overall Model Performance
cbind(Base = conf_basic$overall,
      Tuned = conf_tuned$overall)

# Class-Level Model Performance
cbind(Base = conf_basic$byClass,
      Tuned = conf_tuned$byClass)

#-----------------------------------------------------------
#######  Support Vector Machines  ########
#-----------------------------------------------------------

library(e1071) 

#Alternate working File 
eq4 <- equity_imp

#### Transformation of variables

# Binarize factor variables

eq4$Reason_HE <- class2ind(x = eq4$Reason_HE,drop2nd = TRUE)

Occupation_dummy <- dummyVars(formula =  ~ Occupation,
                              data = eq4)

Occupation_dums <- predict(object = Occupation_dummy, 
                           newdata = eq4)

# Merge new dummy variables into the dataset
eq4_dum <- data.frame(eq4[ ,!names(eq4) %in% c("Occupation")],
                             Occupation_dums)

#### SVM Model

set.seed(82365323) 

# Generate the list of observations for the
# train dataframe
subvm <- createDataPartition(y = eq4_dum$Default, 
                           p = 0.85, 
                           list = FALSE)

# Create our train and test sets
trainvm <- eq4_dum[subvm, ] 
testvm <- eq4_dum[-subvm, ]

vars5 <- names(eq4_dum[,!names(eq4_dum) %in% "Default"])

trainvm_ds <- upSample(x = trainvm[ ,vars5], 
                     y = trainvm$Default, 
                     yname = "Default") 


ctrl_SVM <- trainControl(method = "repeatedcv",
                         number = 5, 
                         repeats = 3,
                         search = "random", 
                         savePredictions = TRUE)

ctrl_SVM$sampling <- "down"

set.seed(82365323)


SVMFit <- train(form = Default ~ ., 
                data = trainvm, 
                method = "svmRadial", 
                preProcess = c("center", "scale"), 
                trControl = ctrl_SVM,
                tuneLength = 10)
SVMFit

tune.tr.preds <- predict(object = SVMFit,
                         newdata = trainvm)

SVM_trtune_conf <- confusionMatrix(data = tune.tr.preds, 
                                   reference = trainvm$Default, 
                                   positive = "1",
                                   mode = "everything")

tune.te.preds <- predict(object = SVMFit,
                         newdata = testvm)

SVM_tetune_conf <- confusionMatrix(data = tune.te.preds, 
                                   reference = testvm$Default, 
                                   positive = "1",
                                   mode = "everything")


cbind(Training = round(SVM_trtune_conf$overall,2),
      Testing = round(SVM_tetune_conf$overall,2))

cbind(Training = round(SVM_trtune_conf$byClass,2),
      Testing = round(SVM_tetune_conf$byClass,2))

#----------------------------------------------------

save.image(file = "FinalProject_Group2.RData")





