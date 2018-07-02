library(caret)
library(pROC)
#Checking all level for the factors
feature_names=names(f_tr)

feature_names
for (f in feature_names) {
  if (class(f_tr[[f]])=="factor") {
    levels <- unique(c(f_tr[[f]]))
    f_tr[[f]] <- factor(f_tr[[f]],
                         labels=make.names(levels))
  }
}


for (f in feature_names) {
  if (class(f_test[[f]])=="factor") {
    levels <- unique(c(f_test[[f]]))
    f_test[[f]] <- factor(f_test[[f]],
                        labels=make.names(levels))
  }
}

#Logistic Reg Model
glm <- glm(income_level~., data = f_tr,family = binomial("logit"))
summary(glm)
anova(glm,test = "Chisq")

remove(glm)

#from the deviance column we can notice how each variable affect the reduction of the deviance
#num_person_Worked_employer, education, dividend_from_Stocks and weeks_worked_in_year has the 
#biggest deviance reduction compare to other variable
#also we can notice that there is  variable doesn't have any affect on the deviance (and doesn't have a significant t value)

#let us drop those variable and run the model again:
 
glm2 <- glm(income_level~ age+wage_per_hour+capital_gains+capital_losses+dividend_from_Stocks
           +num_person_Worked_employer+weeks_worked_in_year+class_of_worker+industry_code
           +education+marital_status+major_occupation_code+race+sex
           +full_parttime_employment_stat+tax_filer_status+d_household_family_stat
           +veterans_benefits
           , data = f_tr,family = binomial("logit"))

summary(glm2)

#the deviance reduced from 71970 to 50856  and AIC from 72306 to 51168 which means the model improved

remove(glm2)



#train Control
ModControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, classProbs = TRUE)

#train
glm3 <-   caret::train(income_level~ age+wage_per_hour+capital_gains+capital_losses+dividend_from_Stocks
                        +num_person_Worked_employer+weeks_worked_in_year+class_of_worker+industry_code
                        +education+marital_status+major_occupation_code+race+sex
                        +full_parttime_employment_stat+tax_filer_status+d_household_family_stat
                        +veterans_benefits,
                        data = f_tr,method = "glm", 
                        metric = "ROC", trControl = ModControl)

summary(glm3)
help(train)
glm3P <- predict(glm3,newdata = f_test)

confusionMatrix(glm3P,f_test$income_level)

remove(glm3)

#Up-Sampling
ModControl$sampling <- "up"
glmUp <-   caret::train(income_level~ age+wage_per_hour+capital_gains+capital_losses+dividend_from_Stocks
                       +num_person_Worked_employer+weeks_worked_in_year+class_of_worker+industry_code
                       +education+marital_status+major_occupation_code+race+sex
                       +full_parttime_employment_stat+tax_filer_status+d_household_family_stat
                       +veterans_benefits,
                       data = f_tr,method = "glm", 
                       metric = "ROC", trControl = ModControl)



glmUp <- predict(glmUp,newdata = f_test)

confusionMatrix(glmUp,f_test$income_level)

remove(glm3)

#Down-Sampling

imp <- varImp(LogMod1,scale = FALSE)
print(imp)

