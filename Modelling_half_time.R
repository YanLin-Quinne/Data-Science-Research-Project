##### Half-time Modelling#####
library(skimr)
library(pROC)
library(leaps)
library(MASS)
library(e1071)
library(tree)
library(caret)
library(randomForest)
library(gbm)
library(dplyr)
library(ggplot2)
library(viridis)  

##### Modelling on 100% of the dataset #####
#rather than splitting the dataset into training and test sets
#train 100% dataset, and use 10-fold cv to test, this way better

# Import Data
df_bat <- read.csv("~/Desktop/all_code/dissertation_dataset/halftime_encode_R.csv")
skim(df_bat)
df_bat$Result <- as.factor(df_bat$Result)
table(df_bat$Result)
str(df_bat)

### Reference material: Introduction to Statistics for Data Science ###
n=nrow(df_bat) #115
p=ncol(df_bat)-1 #16

#k-fold: Set the seed (say, at 5) to make the analysis reproducible 
set.seed(5)
## Sample the fold-assignment index
nfolds = 10
fold_index = sample(nfolds, n, replace=TRUE)
#A random sample of 683 numbers from 0-10 with release 
## Print the first few fold-assignments
head(fold_index)

##10-fold test error function 
logistic_reg_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y=y)
  if(ncol(Xy)>1) tmp_fit = glm(y ~ ., data=Xy[!test_data,], family="binomial") 
  else tmp_fit = glm(y ~ 1, data=Xy[!test_data,,drop=FALSE], family="binomial") 
  phat = predict(tmp_fit, Xy[test_data,,drop=FALSE], type="response")
  yhat = ifelse(phat > 0.5, 1, 0)
  yobs = y[test_data]
  test_error = 1 - mean(yobs == yhat)
  return(test_error)
}

general_cv = function(X, y, fold_ind, fold_error_function) {
  p = ncol(X)
  Xy = cbind(X, y=y)
  nfolds = max(fold_ind)
  if(!all.equal(sort(unique(fold_ind)), 1:nfolds)) stop("Invalid fold partition.") 
  fold_errors = numeric(nfolds)
  for(fold in 1:nfolds) {
    fold_errors[fold] = fold_error_function(X, y, fold_ind==fold) 
  }
  fold_sizes = numeric(nfolds)
  for(fold in 1:nfolds) fold_sizes[fold] = length(which(fold_ind==fold)) 
  test_error = weighted.mean(fold_errors, w=fold_sizes) 
  return(test_error)
}


##### Binomial Regression #####

#model-1:full model
binomial_model <- glm(Result ~ ., data = df_bat, family = "binomial")
#summary and coefficients
summary(binomial_model) 
summary(binomial_model)$coef
summary(binomial_model)$coef[,4] #p-value
#important variables: team_score, gender, avg_wickets_out
confint(binomial_model)

#Calculating predictive probabilities
prob_win_full_binomial <- predict(binomial_model, type = "response")
prob_win_full_binomial
hist(prob_win_full_binomial, 
     main='Histogram of Predicted Probabilities in Full Binomial Regression Model')

#Calculate the training error
phat = predict(binomial_model, df_bat, type="response")
yhat = as.numeric(ifelse(phat > 0.5, 1, 0)) 
1-mean(yhat==df_bat$Result) 
#Train error(full model):0.226087 < 50%, 
#Train Accuracy of the full model:0.773913

#If we simply flipped a coin to decide whether to predict a win or a loss, the training error would be 50%. 
#It appears that we did a little better than that. 
#However, the training error uses the same data to train and test the model and therefore gives an optimistic assessment of the performance of the classifier.

#10-fold cross validation test error
test_error_full_binomial = general_cv(df_bat[,2:p+1], df_bat[,1], fold_index, logistic_reg_fold_error)
test_error_full_binomial
#test error(full model):0.3565217, 
#test accuracy(full model):0.6434783


#Model2: Best Subset Selection with 10 fold cross validation (8 variables)
best = regsubsets(Result~., data=df_bat, nvmax = 16)
results = summary(best)
names(results)
RSS = results$rss
r2 = results$rsq
Cp = results$cp
BIC = results$bic
Adj_r2 = results$adjr2

cbind(RSS, r2, Cp, BIC, Adj_r2)
#RSS should steadily decrease and that R2 increase as we add predictors
par(mfrow = c(1, 2))
plot(RSS, xlab = "Number of Predictors", ylab = "RSS", 
     type = "l", lwd = 2)
plot(r2, xlab = "Number of Predictors", ylab = "R-square", 
     type = "l", lwd = 2)

which.min(Cp) #3
which.min(BIC) #3
which.max(Adj_r2) #8

par(mfrow = c(1, 3))
plot(Cp, xlab = "Number of Predictors", ylab = "Cp", 
     type = 'l', lwd = 2)
points(3, Cp[3], col = "red", cex = 2, pch = 8, lwd = 2)
plot(BIC, xlab = "Number of Predictors", ylab = "BIC", 
     type = 'l', lwd = 2)
points(3, BIC[3], col = "red", cex = 2, pch = 8, lwd = 2)
plot(Adj_r2, xlab = "Number of Predictors", ylab = "Adjusted RSq", 
     type = "l", lwd = 2)
points(8, Adj_r2[8],  col = "red", cex = 2, pch = 8, lwd = 2)
dev.off()

plot(best, scale = "bic")
#The top row corresponds to the best model, 
#while the bottom row to the worst model according to the chosen criterion. 
#White squares correspond to variables that are excluded under each model.

coef(best,3) # Cp BIC
coef(best,8)  # adj-Rsq

### Best Subset Selection with 10-fold cross-validation (CV) approach
glm3 = glm(Result ~ gender + team_score + avg_wickets_out, data = df_bat, family = "binomial")
glm8 = glm(Result ~ opponent_code+gender+team_score+team_wickets+venue_city_code+ 
             avg_score+avg_win_rate+avg_wickets_out, data = df_bat, family = "binomial")


k = 10
folds = cut(1:115, breaks=10, labels=FALSE)
folds
table(folds)
set.seed(5)
folds = sample(folds)
folds
cv.errors = matrix(NA, nrow = k, ncol = 2, 
                   dimnames = list(NULL, c("glm3", "glm8")))
cv.errors 
for(i in 1:k){
  glm3_fit = glm(Result ~ gender + team_score + avg_wickets_out, data = df_bat[folds!=i, ], family = "binomial")
  glm8_fit = glm(Result ~ opponent_code+gender+team_score+team_wickets+venue_city_code+ 
                   avg_score+avg_win_rate+avg_wickets_out, data = df_bat[folds!=i, ], family = "binomial")
  
  pred3 <- predict( glm3_fit, newdata = df_bat[folds==i, ],type="response" )
  pred8 <- predict( glm8_fit, newdata = df_bat[folds==i, ], type="response")
  
  pred3 <- ifelse(pred3 >= 0.5, 1, 0)
  pred8 <- ifelse(pred8 >= 0.5, 1, 0)
  
  cv.errors[i,] = c( mean( (df_bat$Result[folds==i] != pred3)), 
                     mean( (df_bat$Result[folds==i] != pred8)) )
}
cv.errors

cv.mean.errors <- colMeans(cv.errors)
cv.mean.errors
# glm3      glm8 
#0.3045455 0.2772727

#I would choose it glm8 because it has a slightly smaller average cross-validation error
#Cross-validation was used to assess model performance, which greatly reduced the risk of overfitting
df_best <- df_bat[ , c('Result', 'team_score', 'gender', 'avg_wickets_out', 'opponent_code', 'team_wickets',
                       'venue_city_code', 'avg_score', 'avg_win_rate')]

glm8_model <- glm(Result ~ opponent_code + gender + team_score + team_wickets + venue_city_code + 
                    avg_score + avg_win_rate + avg_wickets_out, data = df_best, family = "binomial")
summary(glm8_model)
confint(glm8_model)

prob_win_glm8 <- predict(glm8_model, type = "response")
prob_win_glm8
hist(prob_win_glm8, 
     main='Histogram of Predicted Probabilities in Best Subset Seletion Model')

yhat_glm8 = as.numeric(ifelse(prob_win_glm8 > 0.5, 1, 0)) 
train_error_glm8 = 1 - mean(yhat_glm8 == df_best$Result)
train_error_glm8
#Train error (best subset with 8 variables): 0.226087, 
#Train Accuracy of the best subset model:  0.773913

test_error_glm8 = general_cv(df_best[,2:9], df_best[,1], fold_index, logistic_reg_fold_error)
test_error_glm8
#Test error (best subset with 8 variables): 0.2956522
#test accuracy of the best subset model: 0.7043478

#model3----forward stepwise selection with AIC criterion (3 variables) 

#If the prediction accuracy and the explanatory power of the model are considered: AIC
#If simplicity of the model and avoidance of overfitting are considered: BIC
#If AIC,BIC,Cp all suggest 3 variables, this may be strong evidence that these variables contain most of the useful information for explaining the response variable. 
#Adding more variables may not add much information and may even introduce noise.

null_logit_model = glm(Result~1, data=df_bat, family = "binomial")
forward_step_model <- step(null_logit_model, scope=formula(binomial_model), direction="forward") 
#Result ~ team_score, gender, avg_wickets_out
df_subset <- df_bat[,c('Result','team_score','gender','avg_wickets_out')]

forward_step_logit_model <- glm(formula = Result ~ team_score + gender + avg_wickets_out,
                                family = "binomial", data = df_subset)

summary(forward_step_logit_model) #AIC: 129.74
confint(forward_step_logit_model)

prob_win_forward_binomial <- predict(forward_step_logit_model, type = "response")
prob_win_forward_binomial
hist(prob_win_forward_binomial, 
     main='Histogram of Predicted Probabilities in Forward Stepwise Model')

phat = predict(forward_step_logit_model, df_subset, type="response")
yhat = as.numeric(ifelse(phat > 0.5, 1, 0)) 
1-mean(yhat==df_subset$Result) 
#Train error(forward binomial stepwise): 0.2608696, 
#Train Accuracy:0.7391304

test_error_forward = general_cv(df_subset[,2:4], df_subset[,1], fold_index, logistic_reg_fold_error)
test_error_forward
#test error(forward stepwise):0.2956522
#test accuracy (forward stepwise): 0.7043478

#LDA assumes that the covariance structure is the same for each category, whereas QDA does not make this assumption. 
#As a result, QDA can capture more complex patterns, but may also be more prone to overfitting.


#Linear Discriminant Analysis (LDA)
#model4----LDA with best subset selection model (8 vars)
lda_model = lda(Result ~ opponent_code + gender + team_score + team_wickets + venue_city_code + 
                          avg_score + avg_win_rate + avg_wickets_out, data = df_best)
summary(lda_model)

lda_model$prior
lda_model$means
lda_model$scaling

lda_predict = predict(lda_model, df_best)
lda_win_pro = lda_predict$class
lda_win_pro

confusion_lda = table(Observed=df_best$Result, Predicted=lda_win_pro) 
confusion_lda

1 - mean(df_best$Result == lda_win_pro) 
#Training error (LDA with 8 vars): 0.226087
#train accuracy(LDA with 8 vars): 0.773913

lda_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y=y)
  if(ncol(Xy)>1) tmp_fit = lda(y ~ ., data=Xy[!test_data,]) 
  else tmp_fit = lda(y ~ 1, data=Xy[!test_data,,drop=FALSE]) 
  yhat = predict(tmp_fit, Xy[test_data,,drop=FALSE])$class
  yobs = y[test_data]
  test_error = 1 - mean(yobs == yhat)
  return(test_error)
}

lda_test_error = general_cv(df_best[,2:9], df_best[,1], fold_index, lda_fold_error)
lda_test_error
#LDA test error: 0.3043478
#LDA test accuracy: 0.6956522


#Quadratic Discriminant Analysis (QDA)
#model5----QDA with best subset selection model (8 vars)
qda_model = qda(Result ~opponent_code + gender + team_score + team_wickets + venue_city_code + 
                avg_score + avg_win_rate + avg_wickets_out, data = df_best) 
summary(qda_model)

qda_predict = predict(qda_model, df_best)
qda_win_pro = qda_predict$class
qda_win_pro

confusion_qda = table(Observed=df_best$Result, Predicted=qda_win_pro) 
confusion_qda

1-mean(df_best$Result==qda_win_pro) 
#QDA training error with forward subset: 0.2086957
#QDA train accuracy: 0.7913043

#test error for QDA model
qda_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y=y)
  if(ncol(Xy)>1) tmp_fit = qda(y ~ ., data=Xy[!test_data,]) 
  else tmp_fit = qda(y ~ 1, data=Xy[!test_data,,drop=FALSE]) 
  yhat = predict(tmp_fit, Xy[test_data,,drop=FALSE])$class
  yobs = y[test_data]
  test_error = 1 - mean(yobs == yhat)
  return(test_error)
}

qda_test_error = general_cv(df_best[,2:9], df_best[,1], fold_index, qda_fold_error)
qda_test_error
#QDA test error: 0.3478261
#QDA test accuracy:0.6521739

library(caret)
ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)
result <- rfe(df_best[,2:9], df_best[,1], sizes=c(1:8), rfeControl=ctrl)
print(result)


##### Naive Bayesian #####
#Naive Bayesian model usually does not require feature selection 
#because it automatically handles irrelevant or redundant features

#model6:polynomial naive Bayesian model
nb_model <- naiveBayes(Result ~ ., data = df_bat)
summary(nb_model)
print(nb_model)

nb_model$tables 
#important variables: team_score, team_wickets, match_number

prob_predict_nb <- predict(nb_model, df_bat, type="raw")
print(prob_predict_nb)

predicted_nb <- factor(ifelse(prob_predict_nb[,2] > 0.5, 1, 0), levels = c(0,1))
confusion_nb = table(Observed=df_bat$Result, Predicted=predicted_nb) 
confusion_nb

1-mean(df_bat$Result==predicted_nb )
#naive Bayesian train error:0.2956522
#train Accuracy of the model: 0.7043478

naive_bayes_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y=y)
  if(ncol(Xy) > 1) tmp_fit = naiveBayes(y ~ ., data=Xy[!test_data,]) 
  else tmp_fit = naiveBayes(y ~ 1, data=Xy[!test_data,,drop=FALSE]) 
  phat = predict(tmp_fit, Xy[test_data,,drop=FALSE], type="raw")
  yhat = ifelse(phat[,2] > 0.5, 1, 0)
  yobs = y[test_data]
  test_error = 1 - mean(yobs == yhat)
  return(test_error)
}

test_error_naive_bayes = general_cv(df_bat[, 2:p+1], df_bat[, 1], fold_index, naive_bayes_fold_error)
test_error_naive_bayes
#Test error for naive bayes:  0.3826087
#test accuracy for naive bayes: 0.6173913


##### Classification Tree ##### model7-11
#10-fold cross-validation is a more robust method of model evaluation that reduces fluctuations in results due to random division. 
#However, it is more computationally expensive.
#Simple training/testing division is less computationally expensive, 
#but may be affected by the way the data is divided, which can lead to unstable performance evaluation.

set.seed(472)

### Standard Cart:model7
unpruned_tree_model = tree(Result ~., data = df_bat)
plot(unpruned_tree_model);text(unpruned_tree_model, pretty = 0)
title("Unpruned Classification Tree", line = +3)

summary(unpruned_tree_model) #Classification tree
#training classification error rate is 13.04%
#"team_score"       "avg_wickets_out"  "winner_code"      "team_wickets"     "avg_win_rate"    
# "avg_score"        "toss_winner_code" "gender"           "opponent_code" 

unpruned_prob <- predict(unpruned_tree_model, df_bat, type = "vector")
unpruned_prob

train_predictions_unpruned <- predict(unpruned_tree_model, df_bat, type = "class")
table(df_bat$Result,train_predictions_unpruned)

train_error_unpruned <- mean(train_predictions_unpruned != df_bat$Result)
train_error_unpruned
#Unpruned train error: 0.1304348, unpruned train accuracy: 0.8695652

# 10-fold CV Test Error for Unpruned
folds <- createFolds(df_bat$Result, k = 10)
cv_errors_unpruned <- vector("numeric", 10)
for (i in 1:10) {
  train_data <- df_bat[-folds[[i]], ] 
  test_data <- df_bat[folds[[i]], ]   
  unpruned_model <- tree(Result ~ ., data = train_data) 
  unpruned_predictions <- predict(unpruned_model, test_data, type = "class") 
  cv_errors_unpruned[i] <- mean(unpruned_predictions != test_data$Result) 
}

mean_test_error_unpruned <- mean(cv_errors_unpruned)
mean_test_error_unpruned 
#unpruned test error: 0.3832751, 
#unpruned test accuracy: 0.6167249


### Pruned Tree-model8
cv_tree <- cv.tree(unpruned_tree_model, FUN = prune.misclass)
cv_tree

plot(cv_tree)
title("Deviance as Size of Pruned Tree Increases", line = +3)

pruned_tree_model = prune.misclass(unpruned_tree_model, best = 5)
pruned_tree_model
summary(pruned_tree_model)
#Misclassification error rate: 0.2
#"team_score"  "avg_wickets_out"

plot(pruned_tree_model);text(pruned_tree_model, pretty = 0)
title("Pruned Classification Tree", line = +3)

pruned_prob <- predict(pruned_tree_model, df_bat, type = "vector")
pruned_prob

train_predictions_pruned <- predict(pruned_tree_model, df_bat, type = "class")
table(df_bat$Result,train_predictions_pruned)
train_error_pruned <- mean(train_predictions_pruned != df_bat$Result)
train_error_pruned
# train error pruned:0.2, train accuracy:0.8

# 10-fold CV Test Error for Pruned
folds <- createFolds(df_bat$Result, k = 10)
cv_errors_pruned <- vector("numeric", 10)
for(i in 1:10) {
  train_data <- df_bat[-folds[[i]], ]
  test_data <- df_bat[folds[[i]], ]
  pruned_model <- prune.tree(tree(Result ~., data = train_data, method = "class"), best = 5)
  pruned_predictions <- predict(pruned_model, test_data, type = "class")
  cv_errors_pruned[i] <- mean(pruned_predictions != test_data$Result)
}
mean_test_error_pruned <- mean(cv_errors_pruned)
mean_test_error_pruned
#pruned tree test error: 0.3068765, 
#test accuracy:0.6931235


### Bagging----model9 (mtry=16)
set.seed(472)
bag.10=randomForest(Result~.,data=df_bat,mtry=16,ntree=10,importance=TRUE)
bag.10
summary(bag.10)
#test error and test accuracy for bag.10
folds <- createFolds(df_bat$Result, k = 10)
cv_errors_bagging.10 <- vector("numeric", 10)
for (i in 1:10) {
  train_data <- df_bat[-folds[[i]], ]
  test_data <- df_bat[folds[[i]], ]
  bag_model <- randomForest(Result ~ ., train_data, ntree = 10)
  bag_test_predictions <- predict(bag_model, test_data, type='class')
  cv_errors_bagging.10[i] <- mean(bag_test_predictions != test_data$Result)
}
mean_test_error_bagging.10 <- mean(cv_errors_bagging.10)
mean_test_error_bagging.10  #0.3462121
#test accuracy: 0.6537879

bag.100=randomForest(Result~.,data=df_bat,mtry=16,ntree=100,importance=TRUE)
bag.100
#test error and test accuracy for bag.100
folds <- createFolds(df_bat$Result, k = 10)
cv_errors_bagging.100 <- vector("numeric", 10)
for (i in 1:10) {
  train_data <- df_bat[-folds[[i]], ]
  test_data <- df_bat[folds[[i]], ]
  bag_model <- randomForest(Result ~ ., train_data, ntree = 100)
  bag_test_predictions <- predict(bag_model, test_data, type='class')
  cv_errors_bagging.100[i] <- mean(bag_test_predictions != test_data$Result)
}
mean_test_error_bagging.100 <- mean(cv_errors_bagging.100)
mean_test_error_bagging.100  #0.3088578
#test accuracy: 0.6911422

bag.1000=randomForest(Result~.,data=df_bat,mtry=16,ntree=1000,importance=TRUE)
bag.1000
summary(bag.1000)
#test error and test accuracy for bag.1000
folds <- createFolds(df_bat$Result, k = 10)
cv_errors_bagging.1000 <- vector("numeric", 10)
for (i in 1:10) {
  train_data <- df_bat[-folds[[i]], ]
  test_data <- df_bat[folds[[i]], ]
  bag_model <- randomForest(Result ~ ., train_data, ntree = 1000)
  bag_test_predictions <- predict(bag_model, test_data, type='class')
  cv_errors_bagging.1000[i] <- mean(bag_test_predictions != test_data$Result)
}
mean_test_error_bagging.1000 <- mean(cv_errors_bagging.1000)
mean_test_error_bagging.1000  #0.2652681


predicted_probabilities_bagging <- predict(bag.1000, df_bat, type = "prob")
predicted_probabilities_bagging

train_predictions_bagging <- predict(bag.1000, df_bat,type='class')
train_error_bagging <- mean(train_predictions_bagging != df_bat$Result)
train_error_bagging #bagging train error:0, train accuracy:1, which may be an indication of overfitting

#check feature importance in bagging model
summary(bag.1000)
importance(bag.1000)
importance(bag.1000)[,4]>6.25
#important vars: team_score, avg_score, avg_wickets_out 

varImpPlot(bag.1000, main = "Variable Importance in Bagging Model")
#plot feature importance in bagging model
feature_importance <- as.data.frame(importance(bag.1000))
feature_importance$Feature <- rownames(feature_importance)
ggplot(feature_importance, aes(x=reorder(Feature, MeanDecreaseGini), y=MeanDecreaseGini)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Feature Importance (Bagging)") +
  xlab("Feature") +
  ylab("Mean Decrease Gini")

# check the Gini coefficient
var.gini <- function(x) {
  1 - sum((x/sum(x))^2)
}
bag.gini <- apply(bag.1000$confusion, 1, var.gini)
names(bag.gini) <- rownames(bag.1000$confusion)
bag.gini
#0         1 
#0.4134747 0.4711660 

randomForest::getTree(bag.1000, k = 1, labelVar = TRUE)
plot(bag.1000, main = "Bagging Model")



### Random Forest- model10 (p=16, mtry= 4)
#Regression tree mtry = p/3, classification tree mtry = sqrt(p)
rf.1000 = randomForest(Result~ ., data = df_bat, 
                       mtry=4, ntree=1000, importance=TRUE)
rf.1000 #OOB estimate of  error rate:27.83%

train_predictions_rf <- predict(rf.1000, df_bat, type='class')
train_error_rf <- mean(train_predictions_rf != df_bat$Result)
train_error_rf #random forest train error:0, train accuracy:1, maybe overfitting

predicted_probabilities_rf <- predict(rf.1000, df_bat, type = "prob")
predicted_probabilities_rf

# 10-fold cv test error for Random Forest
folds <- createFolds(df_bat$Result, k = 10)
cv_errors_rf <- vector("numeric", 10)
for (i in 1:10) {
  train_data <- df_bat[-folds[[i]],]
  test_data <- df_bat[folds[[i]],]
  
  rf_model_cv <- randomForest(Result ~ ., data = train_data, mtry = 4, ntree = 1000)
  rf_test_predictions <- predict(rf_model_cv, test_data, type='class')
  cv_errors_rf[i] <- mean(rf_test_predictions != test_data$Result)
}
mean_test_error_rf <- mean(cv_errors_rf)
mean_test_error_rf #0.2840909
# 0.7159091

#chekc feature importance in random forest model
importance(rf.1000 )
importance(rf.1000 )[,4]>6.25
#importance variables: team_score, avg_wickets_out
varImpPlot(rf.1000, main = "Variable Importance in Random Forest Model")

#plot feature importance in random forest model
feature_importance_rf <- as.data.frame(importance(rf.1000))
feature_importance_rf$Feature <- rownames(feature_importance_rf)
ggplot(feature_importance_rf, aes(x=reorder(Feature, MeanDecreaseGini), y=MeanDecreaseGini)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Feature Importance (Random Forest)") +
  xlab("Feature") +
  ylab("Mean Decrease Gini")

randomForest::getTree(rf.1000, k = 1, labelVar = TRUE)
plot(rf.1000, main = "Random Forest Model")



## Boosting-model 11
set.seed(517)
df_bat$Result <- unclass(df_bat$Result)-1
table(df_bat$Result)

boosting = gbm(Result~., data= df_bat, distribution = 'bernoulli',
                    n.trees = 1000, interaction.depth = 2)
boosting
# Perform 10-fold CV for mean test error
folds <- createFolds(df_bat$Result, k = 10)
cv_errors <- vector("numeric", 10)
for (j in 1:10) {
  train_data <- df_bat[-folds[[j]],]
  test_data <- df_bat[folds[[j]],]
  cv_boost_model <- gbm(Result~., data = train_data, 
                        distribution = "bernoulli",
                        n.trees = 1000, interaction.depth = 2)
  predicted_probabilities <- predict(cv_boost_model, test_data, 
                                     n.trees = 1000, type = "response")
  predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)
  cm_boost <- table(factor(predicted_labels, levels=c(0, 1)), 
                    factor(test_data$Result, levels=c(0, 1)))
  accuracy <- sum(diag(cm_boost)) / sum(cm_boost)
  cv_errors[j] <- 1 - accuracy
}
mean_test_error <- mean(cv_errors)
mean_test_error #0.3295455
#test accuracy for boosting: 0.6704545

boosting.1 = gbm(Result~., data= df_bat, distribution = 'bernoulli', shrinkage = 0.1,
               n.trees = 1000, interaction.depth = 1)
# Perform 10-fold CV for mean test error
folds <- createFolds(df_bat$Result, k = 10)
cv_errors <- vector("numeric", 10)
for (j in 1:10) {
  train_data <- df_bat[-folds[[j]],]
  test_data <- df_bat[folds[[j]],]
  cv_boost_model <- gbm(Result~., data = train_data, 
                        distribution = "bernoulli",shrinkage = 0.1,
                        n.trees = 1000, interaction.depth = 1)
  predicted_probabilities <- predict(cv_boost_model, test_data, 
                                     n.trees = 1000, type = "response")
  predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)
  cm_boost <- table(factor(predicted_labels, levels=c(0, 1)), 
                    factor(test_data$Result, levels=c(0, 1)))
  accuracy <- sum(diag(cm_boost)) / sum(cm_boost)
  cv_errors[j] <- 1 - accuracy
}
mean_test_error <- mean(cv_errors)
mean_test_error #0.3492424
#test accuracy for boosting.1: 0.6507576

boosting.4 = gbm(Result~., data= df_bat, distribution = 'bernoulli', shrinkage = 0.1,
                n.trees = 1000, interaction.depth = 4)

# Perform 10-fold CV for mean test error
folds <- createFolds(df_bat$Result, k = 10)
cv_errors <- vector("numeric", 10)
for (j in 1:10) {
  train_data <- df_bat[-folds[[j]],]
  test_data <- df_bat[folds[[j]],]
  cv_boost_model <- gbm(Result~., data = train_data, 
                        distribution = "bernoulli",shrinkage = 0.1,
                        n.trees = 1000, interaction.depth = 4)
  predicted_probabilities <- predict(cv_boost_model, test_data, 
                                     n.trees = 1000, type = "response")
  predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)
  cm_boost <- table(factor(predicted_labels, levels=c(0, 1)), 
                    factor(test_data$Result, levels=c(0, 1)))
  accuracy <- sum(diag(cm_boost)) / sum(cm_boost)
  cv_errors[j] <- 1 - accuracy
}
mean_test_error <- mean(cv_errors)
mean_test_error #0.2863636
#test accuracy for boosting.4: 0.7136364
#boosting.4 is best boosting model

predicted_probabilities <- predict(boosting.4, df_bat, n.trees = 1000, 
                                   shrinkage = 0.1, interaction.depth = 4,type = "response")
predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)
hist(predicted_probabilities, 
     main="Histogram of Predicted Probabilities in Boosting Model")

# Calculate training error
cm_boost <- table(predicted_labels, df_bat$Result)
train_accuracy <- sum(diag(cm_boost)) / sum(cm_boost)
train_error <- 1 - train_accuracy
train_error #train error for boosting:  0, train accuracy for boosting:1, overfitting

#plot feature importance in boosting model
feature_importance <- as.data.frame(summary(boosting.4)[,2])
colnames(feature_importance) <- c("rel.inf")
feature_importance$Feature <- rownames(summary(boosting.4))
ggplot(feature_importance, aes(x=reorder(Feature, rel.inf), y=rel.inf)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Feature Importance (Boosting)") +
  xlab("Feature") +
  ylab("Relative Influence")

gbm::plot.gbm(boosting.4, i.trees = 1, main = "Boosting Model")


### Roc analysis and Log Loss for all model (model1-11)
probs_model1 <- predict(binomial_model, type = "response")
probs_model2 <- predict(glm8_model, type = "response")
probs_model3 <- predict(forward_step_logit_model, type = "response")
lda_predict <- predict(lda_model, df_best)
probs_model4 <- lda_predict$posterior[, 2]
qda_predict <- predict(qda_model, df_best)
probs_model5 <- qda_predict$posterior[, 2]
probs_model6 <- prob_predict_nb[,2] 

probs_model7 <- predict(unpruned_tree_model, df_bat, type = "vector")[,2] 
probs_model8 <- predict(pruned_tree_model, df_bat, type = "vector")[,2] 

probs_model9 <- as.numeric(predict(bag.1000, df_bat, mtry=16, type = "class")) 
probs_model10 <- as.numeric(predict(rf.1000, df_bat, mtry=4, type = "class")) 
probs_model11 <- as.numeric(predict(boosting.4, df_bat,n.trees = 1000, 
                                    shrinkage = 0.1, interaction.depth = 4,type = "response")) 


actual_results <- df_bat$Result

roc_model1 <- roc(actual_results, probs_model1)
roc_model2 <- roc(actual_results, probs_model2)
roc_model3 <- roc(actual_results, probs_model3)
roc_model4 <- roc(actual_results, probs_model4)
roc_model5 <- roc(actual_results, probs_model5)
roc_model6 <- roc(actual_results, probs_model6)
roc_model7 <- roc(actual_results, probs_model7)
roc_model8 <- roc(actual_results, probs_model8)
roc_model9 <- roc(actual_results, probs_model9)
roc_model10 <- roc(actual_results, probs_model10)
roc_model11 <- roc(actual_results, probs_model11)

plot(roc_model1, main="ROC Curves", col="red")
lines(roc_model2, col="green")
lines(roc_model3, col="blue")
lines(roc_model4, col="purple")
lines(roc_model5, col="orange")
lines(roc_model6, col="cyan")
lines(roc_model7, col="magenta")
lines(roc_model8, col="yellow")
lines(roc_model9, col="brown")
lines(roc_model10, col="pink")
lines(roc_model11, col="grey")

legend("bottomright", 
       legend=c("Full Logistic (16 vars)", 
                "Best Subset (8 vars)", 
                "Forward Stepwise (3 vars)", 
                "LDA with Best Subset (8 vars)", 
                "QDA with Best Subset (8 vars)",
                "Naive Bayes",
                "Unpruned Tree",
                "Pruned Tree",
                "Bagging",
                "Random Forest",
                "Boosting"),
       col=c("red", "green", "blue", "purple", "orange", "cyan", "magenta", "yellow", "brown", "pink", "grey"), 
       lty=1, cex=0.6)
#AUC (Area Under the Curve): the closer it is to 1, the better it is at classification.

auc_full <- auc(roc_model1) #0.8446
auc_best <- auc(roc_model2)  #0.8373
auc_forward <- auc(roc_model3) #0.7985
auc_lda <- auc(roc_model4)  #0.8336
auc_qda <- auc(roc_model5)  #0.8877
auc_naive <- auc(roc_model6) #0.8132
auc_unpruned <- auc(roc_model7) #0.9542
auc_pruned <- auc(roc_model8) #0.8339
auc_bagging <- auc(roc_model9) #1
auc_rf <- auc(roc_model10) #1
auc_boosting <- auc(roc_model11) #1
 
#Low Log Loss: Indicates that the model's predictions are very close to the actual labels, which is good.
#High Log Loss: Indicates that the model's predictions are far from the actual labels.
log_loss <- function(predictions, labels) {
  epsilon <- 1e-15
  predictions <- pmin(pmax(predictions, epsilon), 1 - epsilon)
  return(-mean(labels * log(predictions) + (1 - labels) * log(1 - predictions)))
}
labels<- df_bat$Result
table(labels)

log_loss_full_model <- log_loss(probs_model1, labels)
log_loss_full_model #0.4848952

log_loss_best_model <- log_loss(probs_model2, labels)
log_loss_best_model # 0.4998339

log_loss_forward_model <- log_loss(probs_model3, labels)
log_loss_forward_model #0.5293101

log_loss_lda_model <- log_loss(probs_model4, labels)
log_loss_lda_model #0.5012988

log_loss_qda_model <- log_loss(probs_model5, labels)
log_loss_qda_model #0.4233304

log_loss_nb_model <- log_loss(probs_model6, labels)
log_loss_nb_model #0.5393809

log_loss_unpruned_tree_model <- log_loss(probs_model7, labels)
log_loss_unpruned_tree_model #0.2518447

log_loss_pruned_tree_model <- log_loss(probs_model8, labels)
log_loss_pruned_tree_model #0.4524271

log_loss_bagging_model <- log_loss(probs_model9, labels)
log_loss_bagging_model #18.92168
 
log_loss_rf_model <- log_loss(probs_model10, labels)
log_loss_rf_model #18.92168

log_loss_boosting_model <- log_loss(probs_model11, labels)
log_loss_boosting_model #2.504902e-05


##### prediction set #####
df_2023 <- read.csv("~/Desktop/all_code/dissertation_dataset/prediction_set_encoding_halftime_model.csv")
skim(df_2023)
df_2023$Result  <- as.factor(df_2023$Result)
table(df_2023$Result)
str(df_2023)

# Binomial Regression
prob_predict_full <- predict(binomial_model, df_2023, type="response")
pred_full <- ifelse(prob_predict_full > 0.5, 1, 0)
confusion_full <- table(Observed = df_2023$Result, Predicted = pred_full)
print(confusion_full)
accuracy_full <- sum(diag(confusion_full)) / sum(confusion_full)
cat("Full Model Accuracy:", accuracy_full, "\n")

prob_predict_best_sel <- predict(glm8_model, df_2023, type="response")
pred_best_sel <- ifelse(prob_predict_best_sel > 0.5, 1, 0)
confusion_best_sel = table(Observed=df_2023$Result, Predicted=pred_best_sel) 
print(confusion_best_sel)
accuracy_best_sel = sum(diag(confusion_best_sel)) / sum(confusion_best_sel)
cat("Best Selection Model Accuracy:", accuracy_best_sel, "\n")

qda_predict <- predict(qda_model, df_2023)
predicted_qda <- qda_predict$class
confusion_qda <- table(Observed = df_2023$Result, Predicted = predicted_qda)
accuracy_qda <- sum(diag(confusion_qda)) / sum(confusion_qda)
cat("QDA Model Accuracy:", accuracy_qda, "\n")

# nb_model
prob_predict_nb <- predict(nb_model, df_2023, type="raw")[,2]
pred_nb <- ifelse(prob_predict_nb > 0.5, 1, 0)
confusion_nb = table(Observed=df_2023$Result, Predicted=pred_nb) 
print(confusion_nb)
accuracy_nb = sum(diag(confusion_nb)) / sum(confusion_nb)
cat("Naive Bayesian Model Accuracy:", accuracy_nb, "\n")

# classification tree
predictions_unpruned_2023 <- predict(unpruned_tree_model, df_2023, type = "class")
confusion_unpruned <- table(Observed = df_2023$Result, Predicted = predictions_unpruned_2023)
print(confusion_unpruned)
accuracy_unpruned <- sum(diag(confusion_unpruned)) / sum(confusion_unpruned)
cat("Unpruned Tree Model Accuracy:", accuracy_unpruned, "\n")
#0.6551724

predictions_pruned_2023 <- predict(pruned_tree_model, df_2023, type = "class")
confusion_pruned <- table(Observed = df_2023$Result, Predicted = predictions_pruned_2023)
print(confusion_pruned)
accuracy_pruned <- sum(diag(confusion_pruned)) / sum(confusion_pruned)
cat("Pruned Tree Model Accuracy:", accuracy_pruned, "\n")
#0.7241379

prob_predict_bag <- predict(bag.1000, df_2023, type="prob")
pred_bag <- ifelse(prob_predict_bag[,2] > 0.5, 1, 0)
confusion_bag = table(Observed=df_2023$Result, Predicted=pred_bag) 
print(confusion_bag)
accuracy_bag = sum(diag(confusion_bag)) / sum(confusion_bag)
cat("Bagging Model Accuracy:", accuracy_bag, "\n")
#0.7586207 

prob_predict_rf_2023 <- predict(rf.1000, df_2023, type = "prob")
pred_rf_2023 <- ifelse(prob_predict_rf_2023[,2] > 0.5, 1, 0)
confusion_rf_2023 <- table(Observed = df_2023$Result, Predicted = pred_rf_2023)
print(confusion_rf_2023)
accuracy_rf_2023 <- sum(diag(confusion_rf_2023)) / sum(confusion_rf_2023)
cat("Random Forest Model Accuracy:", accuracy_rf_2023, "\n")
#0.7758621 

prob_predict_boost_2023 <- predict(boosting.4, df_2023, n.trees = 1000, type = "response")
pred_boost_2023 <- ifelse(prob_predict_boost_2023 > 0.5, 1, 0)
confusion_boost_2023 <- table(Observed = df_2023$Result, Predicted = pred_boost_2023)
print(confusion_boost_2023)
accuracy_boost_2023 <- sum(diag(confusion_boost_2023)) / sum(confusion_boost_2023)
cat("Boosting Model Accuracy:", accuracy_boost_2023, "\n")
# 0.7413793 

