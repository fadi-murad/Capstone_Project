#install.packages("tinytex")
#install.packages("gridExtra")
##Train Set Preparation

#load needed package for this step:
library(readr)
library(dplyr)
library(ggplot2)
library(grid)
library(Hmisc)
library(gridExtra)
library(corrplot) 


#import train and test data sets
#tr <- read_csv('1-Data_Set/train.csv')
#test <- read_csv('1-Data_Set/test.csv')

#I will save this data sets as RData files in the directory due to the huge sizes of the original csv files
#save(tr, file="2-Data_Exploration/tr.RData")
#save(test, file="2-Data_Exploration/test.RData")

#View train set
str(tr)
Hmisc::describe(tr)

str(test)
Hmisc::describe(test)

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


#Changing income level values to 0 and 1
tr$income_level[tr$income_level==-50000] <- 0
tr$income_level[tr$income_level==+50000] <- 1

test$income_level[test$income_level=="-50000"] <- 0
test$income_level[test$income_level== "50000+."] <- 1


#set the class for each column
tr[numcol]<-  lapply(tr[numcol], as.numeric)
tr[catcol] <- lapply(tr[catcol], as.factor)

test[numcol]<-  lapply(test[numcol], as.numeric)
test[catcol] <- lapply(test[catcol], as.factor)


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


load(file="2-Data_Exploration/countries.RData")

#creating continent_self,continent_father, continent_mother columns using mapping data.fram "countries" 
countries[1:6] <- lapply(countries, as.factor)
tr <- dplyr::left_join(tr,countries[c(1,4)], by= "country_self")
tr <- dplyr::left_join(tr,countries[c(2,5)], by= "country_father")
tr <- dplyr::left_join(tr,countries[c(3,6)], by= "country_mother")

#Save data.frames as RData files to be used later (for markdown files ....)
save(countries,file="2-Data_Exploration/countries.RData")




##Creating Function for Zero Values in each variable
Zero_value <- function(a){
  summarise(filter(tr,a == 0),Zero_Count=n()) / summarise(tr,Zero_Count=n())
}

##Numeric Variables

# 1- age 
ggplot(tr,aes(x=age,y=..density..)) +
  geom_histogram(color="white",binwidth = 1) + geom_density(col="red",size = 1)+
  theme(panel.background = element_blank()) + ggtitle("Age Varibale Density")

ggplot(tr,aes(x=age,y=..count.., fill = as.factor(income_level))) +
  geom_bar() + theme(panel.background = element_blank()) + ggtitle("Income level Distribution in the Age Variable")+labs(fill = "income level")

help("element_text")

# 2- wage_per_hour

ggplot(tr,aes(x=age,y= wage_per_hour)) +
  geom_jitter(aes(col=as.factor(income_level),alpha = 0.5, shape = as.factor(income_level))) + 
  theme(panel.background = element_blank())+ labs(fill = "income level")+
  scale_y_continuous("wage per hour", breaks = seq(0,10000,1000)) +
  ggtitle("Age vs. Wage per hour per Income Level")


Zero_value(tr$wage_per_hour) #94% of the records have Zero value in this variable 


# 3- capital_gains
Zero_value(tr$capital_gains) #96% of the records have Zero value in this variable 

#capital gain log2
ggplot(tr,aes(x=capital_gains,y=age,col=as.factor(income_level),alpha=0.5)) +
  geom_point(position="jitter")+scale_x_continuous(trans='log2') + labs(fill = "income level")+
  ggtitle("Capital Gain (log2) vs. Age per Income Level")



#re-grouping
ggplot(tr,aes(x=ifelse(capital_gains == 0,"Zero","MoreThanZero"),fill=as.factor(income_level))) +
  geom_bar(position="dodge")+
  theme(panel.background = element_blank())+labs(fill = "income level")+
  labs(x="capital gain")+ggtitle("Capital Gain per Income Level") 



#sapply(tr, function(x){sum(is.na(x))/length(x)})*100

#num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == #0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]


# 4- capital_losses
Zero_value(tr$capital_losses) #98% of the records have Zero value in this variable 

ggplot(tr,aes(x=capital_losses,y=age,col=as.factor(income_level),alpha=0.5)) +
  geom_point(position="jitter")+scale_x_continuous(trans='log2') +
  theme(panel.background = element_blank())+labs(fill = "income level")+
  ggtitle("capital losses (log2) vs. age per income level")

ggplot(tr,aes(x=ifelse(capital_losses == 0,"Zero","MoreThanZero"),fill=as.factor(income_level))) +
  geom_bar(position="dodge")+
  theme(panel.background = element_blank())+labs(fill = "income level")+
  labs(x="capital losses")+ggtitle("capital losses per income level") 


# 5- dividend_from_Stocks
Zero_value(tr$dividend_from_Stocks) #89% of the records have Zero value in this variable 

ggplot(tr,aes(x=dividend_from_Stocks,y=age,col=as.factor(income_level),alpha=0.5)) +
  geom_point(position="jitter")+scale_x_continuous(trans='log2') +
  theme(panel.background = element_blank())+labs(fill = "income level")+
  ggtitle("dividend from Stocks (log2) vs. age per income level")

ggplot(tr,aes(x=ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero"),fill=as.factor(income_level))) +
  geom_bar(position="dodge")+
  theme(panel.background = element_blank())+labs(fill = "income level")+
  labs(x="dividend from stocks")+ggtitle("dividend from Stocks per income level") 

# 6- num_person_Worked_employer

ggplot(tr,aes(x=num_person_Worked_employer,y=..count.., fill = as.factor(income_level))) + 
  geom_bar(position="dodge")+ 
  scale_x_continuous("num_person_Worked_employer", breaks = seq(0,6,1)) + 
  theme(panel.background = element_blank())+labs(fill = "income level")

Zero_value(tr$num_person_Worked_employer) #48% zero values


# 7- weeks_worked_in_year
ggplot(tr,aes(x=ifelse(weeks_worked_in_year == 0,"0",ifelse(weeks_worked_in_year==52,52,"1 to 51 week(s)")),y=..count.., fill = as.factor(income_level))) + 
  geom_bar(position="dodge")+theme(panel.background = element_blank())+
  labs(fill = "income level")+labs(x="dividend from stocks")

Zero_value(tr$weeks_worked_in_year) #48% zero values

# Correlation 
numCor<-cor(tr[numcol])
corrplot(numCor, method = "number" , type = "upper", order = "hclust", 
         tl.col = "black")

##Categorical Variables

# 1- class_of_worker

ggplot(tr,aes(x = tr$class_of_worker_Level, fill = as.factor(income_level))) + 
  geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 2- major_occupation_code & occupation_code

ggplot(tr,aes(x = tr$major_occupation_code, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 70,hjust = 1,size=9))

ggplot(tr,aes(x = occupation_code, fill = as.factor(income_level))) + geom_bar(position="dodge")+
  theme(panel.background = element_blank(),axis.text.x =element_text(angle  = 90,hjust = 1,size=9))+ 
  facet_wrap(~major_occupation_code,scales = "free",ncol =  3)
help("facet_wrap")

unique(tr$major_occupation_code)
# 3- education
ggplot(tr,aes(x =tr$Education_Level ,fill= as.factor(income_level))) + geom_bar(position="dodge")+
  theme(panel.background = element_blank(),axis.text.x =element_text(angle  = 70,hjust = 1,size=9)) +
  ggtitle("education vs. income level")

# 4- enrolled_in_edu_inst_lastwk
ggplot(tr,aes(x = tr$enrolled_in_edu_inst_lastwk, fill = as.factor(income_level))) + 
  geom_bar(position="dodge")+
  theme(panel.background = element_blank(),axis.text.x =element_text(angle  = 90,hjust = 1,size=9))+
  ggtitle("enrolled in edu inst lastwk vs. income level")

# 5- marital_status
ggplot(tr,aes(x = tr$marital_status, fill = as.factor(income_level))) + 
  geom_bar(position="dodge")+
  theme(panel.background = element_blank(),axis.text.x =element_text(angle  = 90,hjust = 1,size=9))+
  ggtitle("marital status vs. income level")

# 6- major_industry_code $ industry_code
ggplot(tr,aes(x = tr$major_industry_code, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 70,hjust = 1,size=9))

ggplot(tr,aes(x = industry_code, fill = as.factor(income_level))) + geom_bar(position="dodge")+
  theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))+ 
  facet_wrap(~major_industry_code,scales = "free",ncol = 3)


ggplot(tr,aes(x = tr$major_industry_code, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 70,hjust = 1,size=9))
#without Not in universe or children category
ggplot(filter(tr,major_industry_code!="Not in universe or children"),aes(x = major_industry_code, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 70,hjust = 1,size=9))

# 7- race
ggplot(tr,aes(x = tr$race, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 8- hispanic_origin
ggplot(tr,aes(x = tr$hispanic_origin, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 9- sex
ggplot(tr,aes(x = tr$sex, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))


# 10- member_of_labor_union
ggplot(tr,aes(x = tr$member_of_labor_union, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 11- reason_for_unemployment
ggplot(tr,aes(x = tr$reason_for_unemployment, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 12- full_parttime_employment_stat
ggplot(tr,aes(x = tr$full_parttime_employment_stat, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 13- tax_filer_status
ggplot(tr,aes(x = tr$tax_filer_status, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))


# 14- region_of_previous_residence
ggplot(tr,aes(x = tr$region_of_previous_residence, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 15- state_of_previous_residence
# 183,750 (92%) out of 200,000 records doesn't have a previous residence

# 16- d_household_summary & d_household_family_stat
ggplot(tr,aes(x = tr$d_household_summary_Level, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))
ggplot(tr,aes(x = tr$d_household_family_stat, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 27- migration_msa
ggplot(tr,aes(x = tr$migration_msa, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 28- migration_reg
ggplot(tr,aes(x = tr$migration_reg, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 29- migration_within_reg
ggplot(tr,aes(x = tr$migration_within_reg, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 30- live_1_year_ago
ggplot(tr,aes(x = tr$live_1_year_ago, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 31- migration_sunbelt
ggplot(tr,aes(x = tr$migration_sunbelt, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 32- num_person_Worked_employer
ggplot(tr,aes(x = tr$num_person_Worked_employer, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 33- family_members_under_18
ggplot(tr,aes(x = tr$family_members_under_18, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 34- country_father
ggplot(tr,aes(x = tr$continent_father, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 35- country_mother
ggplot(tr,aes(x = tr$continent_mother, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 36- country_self
ggplot(tr,aes(x = tr$continent_self, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 37- citizenship
ggplot(tr,aes(x = tr$citizenship, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 38- usiness_or_self_employed
ggplot(tr,aes(x = tr$business_or_self_employed, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 39- fill_questionnaire_veteran_admin
ggplot(tr,aes(x = tr$fill_questionnaire_veteran_admin, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 40- veterans_benefits
ggplot(tr,aes(x = tr$veterans_benefits, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))

# 42- year
ggplot(tr,aes(x = tr$year, fill = as.factor(income_level))) + geom_bar(position="dodge")+theme(axis.text.x =element_text(angle  = 90,hjust = 1,size=9))


#Finally, let us save the change we made on the tr file to be used on the next step markdown file 
save(tr,file="3-Data_Cleaning_and_Manipulation/tr.RData")
save(test,file="3-Data_Cleaning_and_Manipulation/test.RData")

