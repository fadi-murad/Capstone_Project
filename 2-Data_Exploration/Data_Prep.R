#install.packages("tinytex")
#install.packages("gridExtra")
##Train Set Preparation

#load needed package for this step:
library(readr)
library(dplyr)
library(ggplot2)

#import train and test data sets
tr <- read_csv('1-Data_Set/train.csv')
test <- read_csv('1-Data_Set/test.csv')

#I will save this data sets as RData files in the directory due to the huge sizes of the original csv files
save(tr, file="2-Data_Exploration/tr.RData")
save(test, file="2-Data_Exploration/test.RData")


#View train set
View(tr)
str(tr)
dim(tr)
str(test)

#Creating vectors contains the columns index for categorical and numeric columns 
catcol <- c(2:5,7,8:16,20:29,31:38,40)
numcol <- c(setdiff(1:40,catcol),41)

##Checking the target variables
unique(tr$income_level)
prop.table(table(tr$income_level)) #value distribution
summarise(group_by(tr,income_level),n=n()) #values count

unique(test$income_level)
prop.table(table(test$income_level)) #value distribution
summarise(group_by(test,income_level),n=n()) #values count


#set the class for each column
tr[numcol]<-  lapply(tr[numcol], as.numeric)
tr[catcol] <- lapply(tr[catcol], as.factor)

test[numcol]<-  lapply(test[numcol], as.numeric)
test[catcol] <- lapply(test[catcol], as.factor)


#Changing income level values to 0 and 1
tr$income_level[tr$income_level==-50000] <- 0
tr$income_level[tr$income_level==+50000] <- 1

test$income_level[test$income_level=="-50000"] <- 0
test$income_level[test$income_level=="50000+."] <- 1


#create 2nd income level colum will logic class to be used later on
tr<-mutate(tr,income_level_1=as.logical(tr$income_level))

#creating class_of_worker_Level column (regrouping levels) 
tr<- mutate(tr, class_of_worker_Level = ifelse(grepl('gov', tr$class_of_worker), "Government",
       ifelse(grepl('Self', tr$class_of_worker), "Self-employed",as.character(tr$class_of_worker))))

#creating Education_Level column (regrouping levels) 
tr<- mutate(tr, Education_Level = ifelse(grepl('th grade', tr$education), "the first through twelfth grades",
        ifelse(grepl('Associates degree', tr$education), "Associates degree",as.character(tr$education))))

#creating d_household_summary_Level column (regrouping levels) 
tr<- mutate(tr, d_household_summary_Level = ifelse(grepl('Child under 18', tr$d_household_summary), "Child under 18",
                                         ifelse(grepl('householder', tr$d_household_summary), "householder/relative/non-relative",
                                                as.character(tr$d_household_summary))))

#set the class for the new columns
tr$class_of_worker_Level <- as.factor(tr$class_of_worker_Level)
tr$Education_Level <- as.factor(tr$Education_Level)
tr$d_household_summary_Level <- as.factor(tr$d_household_summary_Level)

#creating continent_self,continent_father, continent_mother columns using mapping data.fram "countries" 
countries[1:6] <- lapply(countries, as.factor)
tr <- dplyr::left_join(tr,countries[c(1,4)], by= "country_self")
tr <- dplyr::left_join(tr,countries[c(2,5)], by= "country_father")
tr <- dplyr::left_join(tr,countries[c(3,6)], by= "country_mother")

#Save data.frames as RData files to be used later (for markdown files ....)
save(countries,file="2-Data_Exploration/countries.RData")


