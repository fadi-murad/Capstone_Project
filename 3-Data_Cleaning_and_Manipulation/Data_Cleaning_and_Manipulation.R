library(caret)
library(mlr)


load(file="3-Data_Cleaning_and_Manipulation/tr.RData")
load(file="3-Data_Cleaning_and_Manipulation/test.RData")

#first let us split the data sets to numeric and categorical, it's easier for this step
tr_num <- tr[numcol]
tr_cat <- tr[catcol]

test_num <- test[numcol]
test_cat <- test[catcol]

#1. Missing values

#Let’s check for missing values in numeric variables.

#check missing values in numerical data
trNumMissing <- sapply(tr_num, function(x){sum(is.na(x))/length(x)})
testNumMissing <- sapply(test_num, function(x){sum(is.na(x)/length(x))})


#there is no missing values in the numerical variables which means we save some time :)
#Anyhow, let us turn our focus now to the missing values in the categorical variables 


#Now,let’s check for missing values in categorical data. 
#We’ll use base sapply() to find out percentage of missing values per column.

#check missing values in each variable
trCatMissing <- sapply(tr_cat, function(x){sum(is.na(x))/length(x)})
testCatMissing <- sapply(test_cat, function(x){sum(is.na(x)/length(x))})

trCatMissing[trCatMissing > 0] 

#There are 9 variables have missing values 3 of them have almost 50% missing values.
#We will dorp off variables with more than 10% missing values from our set

tr_cat <- subset(tr_cat, select = trCatMissing < 0.1 )
test_cat <- subset(test_cat, select = trCatMissing < 0.1)



#2. Near Zero Variance Variables
#during the exploration setp we saw alot of variables have one level with very high frequency
#(around 90% of the data belong to this level) while the other levels have very low 
#frequenct ( between 1% to 4%), usually this kind of variables doesn't have much information
#to offer to the predictive models, we will use nearZeroVar function to locate those variables
#nearZeroVar diagnoses predictors that have one unique value or predictors that are have 
#both of the following characteristics: they have very few unique values relative to the 
#number of samples and the ratio of the frequency of the most common value to the frequency of the second most common value is large.

#2.1 Numeric Variable
nzvNum <- nearZeroVar(tr_num, saveMetrics= TRUE)
subset(nzvNum, nzvNum$nzv == TRUE)

# As we saw on the exploration step those variables have a huge amount of Zero values, we will reclassify 
#those variable to categorical with only 2 levels "zero" & "more than zero" 

tr_num$wage_per_hour <- as.factor(ifelse(tr_num$wage_per_hour == 0,"Zero","MoreThanZero"))
tr_num$capital_gains <- as.factor(ifelse(tr_num$capital_gains == 0,"Zero","MoreThanZero"))
tr_num$capital_losses <- as.factor(ifelse(tr_num$capital_losses == 0,"Zero","MoreThanZero"))
tr_num$dividend_from_Stocks <- as.factor(ifelse(tr_num$dividend_from_Stocks == 0,"Zero","MoreThanZero"))

test_num$wage_per_hour <- as.factor(ifelse(test_num$wage_per_hour == 0,"Zero","MoreThanZero"))
test_num$capital_gains <- as.factor(ifelse(test_num$capital_gains == 0,"Zero","MoreThanZero"))
test_num$capital_losses <- as.factor(ifelse(test_num$capital_losses == 0,"Zero","MoreThanZero"))
test_num$dividend_from_Stocks <- as.factor(ifelse(test_num$dividend_from_Stocks == 0,"Zero","MoreThanZero"))


#2.2 Categorical Variables
nzvCat <- nearZeroVar(tr_cat,freqCut = 90/10, saveMetrics= TRUE)
subset(nzvCat, nzvCat$nzv == TRUE)

tr_cat[nzvCat$nzv == TRUE] <- NULL
test_cat[nzvCat$nzv == TRUE] <- NULL

#3. Checking levels for each variable in the train and test sets
# when you new variable level in test set that doesn't exist in the train set, sometimes that could rise a problem 
#some of the classification model tends to treat this level with null values and other models could give an error such
#as logistics regression

#therefore we will check level count for each categorical variable in train and test sets

summarizeColumns(tr_cat)[,"nlevs"]
summarizeColumns(test_cat)[,"nlevs"]

#Only d_household_family_stat variable have one additonal level on the train set
#However this variable has levels with very low frequency 6, 9, 37 ...., this will affect 
#cross validation process for specific models later on, therefore and only for this variable we will combain 
#all the levels with less than 1% frequency to new level "others"
which(prop.table(table(tr_cat$d_household_family_stat)) < 0.01)
tr_d_hous <- names(which(prop.table(table(tr_cat$d_household_family_stat)) < 0.01))
test_d_hous <-names(which(prop.table(table(test_cat$d_household_family_stat)) < 0.01))

levels(tr_cat$d_household_family_stat) [levels(tr_cat$d_household_family_stat) %in% tr_d_hous] <- "others"
levels(test_cat$d_household_family_stat) [levels(test_cat$d_household_family_stat) %in% test_d_hous] <- "others"


#now let combine the numeric and categorical subsets to train and test sets
f_tr <- cbind(tr_num,tr_cat)
f_test <- cbind(test_num,test_cat)



#remove the subsets
remove(tr_cat,test_cat,tr_num,test_num,tr,test)

#income level as factor for our classification models
f_tr$income_level <- as.factor(f_tr$income_level)
f_test$income_level <- as.factor(f_test$income_level)

#check our result
str(f_tr)
str(f_test)

#now we have 23 variables (including the target variable) ready to be used in the machine learning step


#Finally, let us save training and test files to be used on the next step markdown file 
save(f_tr,file="4-Machine_Learning/f_tr.RData")
save(f_test,file="4-Machine_Learning/f_test.RData")


