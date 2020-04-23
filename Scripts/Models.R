# Predicting fatality in an accident

library(ISLR)

accidents_df <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Road_Accidents\\v1_Data_Processing\\Vehicle_Accidents_in_Iowa_2015_2016_categorical.csv", header = TRUE)
names(accidents_df)

head(accidents_df)
summary(accidents_df)


par(mfrow=c(1,1))

hist(accidents_df$Property.Damage)

boxplot(accidents_df$Property.Damage)

library(Amelia)

missmap(accidents_df)

sapply(accidents_df,function(x) sum(is.na(x)))
sapply(accidents_df, function(x) length(unique(x)))
sapply(accidents_df, function(x) is.factor(x))
#is.factor(accidents_df$DOT.Case.Number)
contrasts(accidents_df$Light.Conditions)


str(accidents_df)

# Remove unnecessary columns

accidents_df$DOT.Case.Number <- NULL
accidents_df$Law.Enforcement.Case.Number <- NULL
accidents_df$Crash.Date...Time <- NULL
accidents_df$City <- NULL
accidents_df$Crash.Month <- NULL
accidents_df$Work.Zone <- NULL
accidents_df$Literal.Description <- NULL
accidents_df$Report.Type <- NULL
accidents_df$Rest.Update <- NULL
accidents_df$District <- NULL
accidents_df$Environment <- NULL
accidents_df$Roadway <- NULL

install.packages('caTools')


# Encode target variable as factor

accidents_df$Fatality <- as.factor(accidents_df$Fatality)


require(caTools)  # loading caTools library
## Loading required package: caTools
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(accidents_df,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 =subset(accidents_df,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test1=subset(accidents_df, sample==FALSE)

logistic_model <- glm(Fatality ~Major.Cause+Crash.Manner+Surface.Conditions+Drug.Alcohol.Related+Light.Conditions+Light.Conditions+Weather.Conditions+Vehicles+Occupants+Property.Damage,family=binomial(link='logit'),data=train1, weights = weight)


summary(logistic_model)



fitted.results.probability <- predict(logistic_model,newdata=test1,type='response')
fitted.results <- ifelse(fitted.results.probability > 0.4,1,0)
misClasificError <- mean(fitted.results != test1$Fatality)
print(paste('Accuracy',1-misClasificError))

fitted.results

cm = table(test1$Fatality, fitted.results)

cm

weight <- train1$Fatality
weight <- weight *29
weight <- weight + 1
table(weight)

# plotting logistic regression 

predicted.data <- data.frame(probability.of.fatality=fitted.results.probability, fatality=test1$Fatality)
predicted.data <- predicted.data[order(predicted.data$probability.of.fatality, decreasing = FALSE), ]
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
library(cowplot)

ggplot(data = predicted.data, aes(x=rank, y=probability.of.fatality)) +
  geom_point(aes(color=fatality), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of fatality")

ggsave()

#table(train1$Fatality, fitted.results > 0.5)
#names(fitted.results)

install.packages('ROCR')
library(ROCR)
p <- predict(model, newdata=test1, type="response")
pr <- prediction(p, test1$Fatality)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#auc <- performance(pr, measure = "auc")
#auc <- auc@y.values[[1]]
#auc





# naive bayes

#install.packages('e1071')
library('e1071')

classifier = naiveBayes(x = train1[-26], y = train1$Fatality)

naive_pred = predict(classifier, newdata = test1[-26])
naive_cm = table(test1$Fatality, naive_pred)
naive_cm


# visualise
plot(classifier)





#svm

library(e1071)
svm_classifier_radial = svm(formula = Fatality ~Major.Cause+Crash.Manner+Surface.Conditions+Drug.Alcohol.Related+Light.Conditions+Light.Conditions+Weather.Conditions+Vehicles+Occupants+Property.Damage, 
                     data = train1, 
                     type = 'C-classification',
                     kernel = 'radial')

svm_classifier_linear = svm(formula = Fatality ~Major.Cause+Crash.Manner+Surface.Conditions+Drug.Alcohol.Related+Light.Conditions+Light.Conditions+Weather.Conditions+Vehicles+Occupants+Property.Damage, 
                            data = train1, 
                            type = 'C-classification',
                            kernel = 'linear')


plot(svm_classifier_linear, train1['Light.Conditions', 'Weather.Conditions', 'Fatality'])

y_pred_svm_radial = predict(svm_classifier_radial, newdata = test1[-26])

y_pred_svm_linear = predict(svm_classifier_linear, newdata = test1[-26])

cm_svm_radial = table(test1$Fatality, y_pred_svm_radial)

cm_svm_linear = table(test1$Fatality, y_pred_svm_linear)

cm_svm_radial
cm_svm_linear




# Metrics for evaluating the fit of the models (Recall)
recall <- diag(naive_cm) / rowSums(naive_cm)
recall











alcohol_df <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\final_merged_alcohol_weather_with_0_prcp_temp_cleaned.csv", header = TRUE)



# Check for NA values
sapply(alcohol_df, function(x) sum(is.na(x))) 

alcohol_df_backup <- alcohol_df

alcohol_df$AWND[is.na(alcohol_df$AWND)] <- 0

alcohol_df$TAVG <- NULL
x <- na.omit(alcohol_df)

# Splitting the dataset into the Training set and Test set


library(caTools)
set.seed(123)
split = sample.split(x$Sale..Dollars., SplitRatio = 0.75)
training_set = subset(x, split == TRUE)
test_set = subset(x, split == FALSE)



### Check correlation in the training data
library(corrplot)
head(training_set)
cor_mat <- cor(training_set[, c(3,4,8,9,10,11,12,13)], method = c('pearson'))
corrplot(cor_mat, method='circle', tl.srt=45)



### Create multiple linear model

regressor = lm(formula = Sale..Dollars. ~ PRCP + AWND + SNOW + SNWD + TMAX + TMIN,
               data = training_set)

summary(regressor)

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressor)

y_pred = predict(regressor, newdata = test_set)

y_pred
#library(tidyverse)
library(caret)

# Model performance
# (a) Prediction error, RMSE
RMSE(y_pred, test_set$Sale..Dollars.)
# (b) R-square
R2(y_pred, test_set$Sale..Dollars.)





alcohol_df_without_NA <- x

# Applying k-Fold Cross Validation
folds = createFolds(alcohol_df_without_NA$Sale..Dollars., k = 10)
cv = lapply(folds, function(x) {
  training_fold = alcohol_df_without_NA[-x, ]
  test_fold = alcohol_df_without_NA
  regressor = lm(formula = Sale..Dollars. ~ PRCP + AWND + SNOW + SNWD + TMAX + TMIN,
                 data = training_fold)
  #classifier = svm(formula = Purchased ~ .,
  #                 data = training_fold,
  #                 type = 'C-classification',
  #                 kernel = 'radial')
  y_pred = predict(regressor, newdata = test_fold[-3])
  #cm = table(test_fold[, 3], y_pred)
  rsquare = R2(y_pred, test_fold$Sale..Dollars.)
  return(rsquare)
})
rsquare_with_k_fold = mean(as.numeric(cv))
rsquare_with_k_fold




# SVR

library(e1071)
library(caret)
svr_model <- svm(Sale..Dollars. ~ PRCP + AWND + SNOW + SNWD + TMAX + TMIN, training_set)

svr_y_pred <- predict(svr_model, test_set)

# Model performance
# (a) Prediction error, RMSE
RMSE(svr_y_pred, test_set$Sale..Dollars.)
# (b) R-square
R2(svr_y_pred, test_set$Sale..Dollars.)
