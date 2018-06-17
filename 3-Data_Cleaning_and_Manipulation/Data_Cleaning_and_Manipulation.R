library(readr)
library(dplyr)
library(ggplot2)

## 1- Data Cleaning

#first let us split the data sets to numeric and categorical, it's easier for this step
tr_num <- tr[numcol]
tr_cat <- tr[catcol]

test_num <- test[numcol]
test_cat <- test[catcol]


#Let’s check for missing values in numeric variables.

#check missing values in numerical data
table(is.na(tr_num))
table(is.na(test_num))
#there is no missing values in the numerical variables which means we save some time :)
#Anyhow, let us turn our focus now to the missing values in the categorical variables 


#Now,let’s check for missing values in categorical data. 
#We’ll use base sapply() to find out percentage of missing values per column.

#check missing values in each variable
trCatMissing <- sapply(tr_cat, function(x){sum(is.na(x))/length(x)})
testCatMissing <- sapply(test_cat, function(x){sum(is.na(x)/length(x))})
 
#There are a few variables have almost 50% missing values.
#We will dorp off variables with more than 10% missing values from our set

tr_cat <- subset(cat_train, select = trCatMissing < 0.1 )
test_cat <- subset(cat_test, select = testCatMissing < 0.1)
#Julian should i remove the column in the test set depend on the train set?


#for the variables with less than 10% missing value, we will replace 'NA' with 'Not Available':
#convert to matrix, replace 'null' with 'Not Available', and convert back to data.frame
tr_cat <- as.matrix(tr_cat)
tr_cat[is.na(tr_cat)]<-"Not Available"
tr_cat <- as.data.frame(tr_cat)

test_cat <- as.matrix(test_cat)
test_cat[is.na(test_cat)]<-"Not Available"
test_cat <- as.data.frame(test_cat)



## 2- Data Manipulation

#The last step before start building our model is the data manipulation, 
#In this stage we will try to shape the data in way that could serve our model better than
#the original shape (to get more information)
#as we did in the exploration step when we regroup some of the categrical variable level or when
#we grouped the numeric variables as categrical with only 2 or 3 levels

#Nomeric Variables:
#As we saw in the correlation matrix, there is no significant correlation between the target
#variable and the other variables, therefore, it's better to reshape the numeric variables according 
#to the target variable distribution:

#age: 3 categories (<25 25-65 >65):

#other numeric variables: 2 categories (zero, and more than zero)



#Nomeric Variables:

#what do you think?
#1- any category has frequency lest than 5% or 10% to be considered as "other" category  
#2- stick to the levels we created in the data exploration step 
#3- use both method, use the new level we created then apply the 10% rule ("other" category)



