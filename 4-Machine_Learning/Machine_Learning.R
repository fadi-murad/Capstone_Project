#Before we start with machine learning let us take a minute to clarify a few things about 
#imbalanced dataset

#Is accurecy measure the best measure for imbalanced dataset classification model, well it's not

#it's very common for classification model (especially with simple rule based algorithm) to give 
#90% accurecy when we have 90% of 
#the instances in one class, simply because our models look at the data and cleverly decide 
#that the best thing to do is to always predict this majority class and achieve high accuracy

#Solution?
#We will use the below performance measures that can give more insight into the accuracy of 
#our model

#Sensitivity = True Positive Rate (TP/TP+FN) 
#Specificity = True Negative Rate (TN/TN +FP)
#Precision = (TP/TP+FP)
#Recall = Sensitivity
#F score (A weighted average of precision and recall) = () 2 * (Precision * Recall)/ (Precision + Recall) – It is the harmonic mean of precision and recall. It is used to compare several models side-by-side. Higher the better.
#ROC Curves

#Do we need to resampling our dataset?
#It's recommended to change the dataset that we use to build your predictive model 
#to have more balanced data.
#This change is called sampling dataset and there are 3 main methods: over-sampling, under-sampling, and SMOTE (Synthetic Minority Over-sampling Technique)
  
#we will create 3 addional sets using the mentioned methods then we will apply the classification models
#on the original and the sampling  sets and evaluate the results


#What is the best algorithm for imbalanced dataset:
#Decision trees (like random forest) often perform well on imbalanced datasets also naive Bayes
#is good algorithm
#We are gonna apply those algorithm and compare the results to choose the best fit model

#Now after we have a clear vision let us hit the road!







  