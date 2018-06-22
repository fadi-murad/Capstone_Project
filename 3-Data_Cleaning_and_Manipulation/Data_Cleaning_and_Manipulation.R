## 1- Data Cleaning

#first let us split the data sets to numeric and categorical, it's easier for this step
tr_num <- tr[numcol]
tr_cat <- tr[catcol]

test_num <- test[numcol]
test_cat <- test[catcol]

#a. missing values

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

tr_cat <- subset(tr_cat, select = trCatMissing < 0.1 )
test_cat <- subset(test_cat, select = trCatMissing < 0.1)


#for the variables with less than 10% missing value, we will replace 'NA' with 'Not Available':
#convert to matrix, replace 'null' with 'Not Available', and convert back to data.frame
tr_cat <- as.matrix(tr_cat)
tr_cat[is.na(tr_cat)]<-"Not Available"
tr_cat <- as.data.frame(tr_cat)

test_cat <- as.matrix(test_cat)
test_cat[is.na(test_cat)]<-"Not Available"
test_cat <- as.data.frame(test_cat)


#b. Fields with very low information to offer

#during the exploration setp we saw alot of variables have one level with very high frequency (around 90% of the data belong to this level) while the other levels have very low frequenct ( between 1% to 4%)
#due to this reason we better not consider this variables in our model
#below is a list of those variables, you can check yourself using levelPer function which 
#calculate level frequency percentage in each variable

#levelPer function
levelPer <- function(levelFreqPercentage){
  prop.table(table(levelFreqPercentage))
}
levelPer(tr$hispanic_origin)


#categorical variable to drop 
lowInfo <- c("race","hispanic_origin","member_of_labor_union","reason_for_unemployment","region_of_previous_residence",
             "state_of_previous_residence","live_1_year_ago","country_father","country_mother","country_self","citizenship",
             "business_or_self_employed","fill_questionnaire_veteran_admin","enrolled_in_edu_inst_lastwk","year")

#drop off variables from the train and test sets 
tr_cat[lowInfo] <- NULL
test_cat[lowInfo] <- NULL

#c. Multicollinearity
#We notice earlier in the exploration step (correlation matrix) that there is a strong correlation
# between num_person_Worked_employer and weeks_worked_in_year, it's ~ 0.75+
# we need to drop off one one these variable and I prefer to drop weeks_worked_in_year as most of the data fall under
# to values 0 and 52 week while num_person_Worked_employer looks more informative to our model



tr_num$weeks_worked_in_year <- NULL
test_num$weeks_worked_in_year <- NULL

## 2- Data Manipulation

#The last step before start building our model is the data manipulation, 
#In this stage we will try to shape the data in way that could serve our model better than
#the original shape (to get more information)
#as we did in the exploration step when we regroup some of the categrical variable level or when
#we grouped the numeric variables as categrical with only 2 or 3 levels

#Nomeric Variables:
# As we saw in the correlation matrix, there is no significant correlation between the target
#variable and the other numeric variables, therefore, it's better to reshape the numeric variables


#Variables with larg amount of zero value to be reclassifed to categorical variable with only 2 levels to zero and more than zero 

tr_num$wage_per_hour <- as.factor(ifelse(tr_num$wage_per_hour == 0,"Zero","MoreThanZero"))
tr_num$capital_gains <- as.factor(ifelse(tr_num$capital_gains == 0,"Zero","MoreThanZero"))
tr_num$capital_losses <- as.factor(ifelse(tr_num$capital_losses == 0,"Zero","MoreThanZero"))
tr_num$dividend_from_Stocks <- as.factor(ifelse(tr_num$dividend_from_Stocks == 0,"Zero","MoreThanZero"))

test_num$wage_per_hour <- as.factor(ifelse(test_num$wage_per_hour == 0,"Zero","MoreThanZero"))
test_num$capital_gains <- as.factor(ifelse(test_num$capital_gains == 0,"Zero","MoreThanZero"))
test_num$capital_losses <- as.factor(ifelse(test_num$capital_losses == 0,"Zero","MoreThanZero"))
test_num$dividend_from_Stocks <- as.factor(ifelse(test_num$dividend_from_Stocks == 0,"Zero","MoreThanZero"))

#now let combine the numeric and categorical subsets to train and test sets

f_tr <- cbind(tr_num,tr_cat)
f_test <- cbind(test_num,test_cat)

#check our result
str(f_tr)
str(f_test)

#now we have 23 variables (including the target variable) ready to be used in the machine learning step


##for discussion:
#numeric:
#age: 3 categories (<25 25-65 >65):

#Categorical Variables:
#what do you think?
#1- any category has frequency lest than 5% or 10% to be considered as "other" category  
#2- stick to the levels we created in the data exploration step 
#3- use both method, use the new level we created then apply the 10% rule ("other" category)