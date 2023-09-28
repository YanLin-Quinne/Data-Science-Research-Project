##### Full-time Modelling#####
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
library(bestglm)
library(car)
##### Modelling on 100% of the dataset #####
df_bat <- read.csv("~/Desktop/all_code/dissertation_dataset/halftime_encode_R.csv")
skim(df_bat)
dim(df_bat)
#drop team score, team wickets, winner, toss winner, choose to bat

columns_to_keep <- c('Result', "match_number", "team_code", "opponent_code", 
                     "gender",'season','home_advantage','venue_city_code',
                     'avg_score','avg_win_rate','avg_wickets_out','last_match_result')

df <- df_bat[, columns_to_keep]
str(df)
skim(df)
cor(df[,2:12])
dim(df)

df$Result <- as.factor(df$Result)
table(df$Result)

n=nrow(df)  #115
p=ncol(df)-1 #11

set.seed(5)
nfolds = 10
fold_index = sample(nfolds, n, replace=TRUE)
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

#model-1:full model
binomial_model <- glm(Result ~ ., data = df, family = "binomial")
#summary and coefficients
summary(binomial_model) 
summary(binomial_model)$coef
summary(binomial_model)$coef[,4] #p-value
#important variables: avg_wickets_out

#Calculating predictive probabilities
prob_win_full_binomial <- predict(binomial_model, df, type = "response")
prob_win_full_binomial
hist(prob_win_full_binomial, 
     main='Histogram of Predicted Probabilities in Full Binomial Regression Model')

#Calculate the training error
phat = predict(binomial_model, df, type="response")
yhat = as.numeric(ifelse(phat > 0.5, 1, 0)) 
1-mean(yhat==df$Result) 
#Train error(full model):0.3391304 < 50%, 
#Train Accuracy of the full model:0.6608696

test_error_full_binomial = general_cv(df[,2:p+1], df[,1], 
                                      fold_index, logistic_reg_fold_error)
test_error_full_binomial
#test error: 0.4608696
#test accuracy: 0.5391304

#best selection model (model2)
best = regsubsets(Result~., data=df, nvmax = 11)
results = summary(best)
names(results)
RSS = results$rss
r2 = results$rsq
Cp = results$cp
BIC = results$bic
Adj_r2 = results$adjr2

cbind(RSS, r2, Cp, BIC, Adj_r2)
par(mfrow = c(1, 2))
plot(RSS, xlab = "Number of Predictors", ylab = "RSS", 
     type = "l", lwd = 2)
plot(r2, xlab = "Number of Predictors", ylab = "R-square", 
     type = "l", lwd = 2)

which.min(Cp) #3
which.min(BIC) #1
which.max(Adj_r2) #4

par(mfrow = c(1, 3))
plot(Cp, xlab = "Number of Predictors", ylab = "Cp", 
     type = 'l', lwd = 2)
points(3, Cp[3], col = "red", cex = 2, pch = 8, lwd = 2)
plot(BIC, xlab = "Number of Predictors", ylab = "BIC", 
     type = 'l', lwd = 2)
points(1, BIC[1], col = "red", cex = 2, pch = 8, lwd = 2)
plot(Adj_r2, xlab = "Number of Predictors", ylab = "Adjusted RSq", 
     type = "l", lwd = 2)
points(4, Adj_r2[4],  col = "red", cex = 2, pch = 8, lwd = 2)
dev.off()

plot(best, scale = "bic")

coef(best,3) # Cp 
coef(best,1) #BIC
coef(best,4)  # adj-Rsq

glm3 = glm(Result ~  team_code+avg_win_rate + avg_wickets_out, 
           data = df, family = "binomial")
glm1 = glm(Result~ avg_wickets_out, data= df,  family = "binomial")
glm4 = glm(Result ~ team_code+venue_city_code+avg_win_rate+avg_wickets_out , 
           data = df, family = "binomial")

k = 10
folds = cut(1:115, breaks=10, labels=FALSE)
folds
table(folds)
set.seed(5)
folds = sample(folds)
folds
cv.errors = matrix(NA, nrow = k, ncol = 3, 
                   dimnames = list(NULL, c("glm3",'glm1',"glm8")))
cv.errors 
for(i in 1:k){
  glm3_fit = glm(Result ~  team_code+avg_win_rate + avg_wickets_out, 
                 data = df[folds!=i, ], family = "binomial")
  glm1_fit = glm(Result~ avg_wickets_out, data= df[folds!=i, ],  family = "binomial")
  glm8_fit = glm(Result ~ team_code+venue_city_code+avg_win_rate+avg_wickets_out , 
                 data = df[folds!=i, ], family = "binomial")
  
  pred3 <- predict( glm3_fit, newdata = df[folds==i, ],type="response" )
  pred1 <- predict( glm1_fit, newdata = df[folds==i, ],type="response" )
  pred8 <- predict( glm8_fit, newdata = df[folds==i, ], type="response")
  
  pred3 <- ifelse(pred3 >= 0.5, 1, 0)
  pred1 <- ifelse(pred1 >= 0.5, 1, 0)
  pred8 <- ifelse(pred8 >= 0.5, 1, 0)
  
  cv.errors[i,] = c( mean( (df$Result[folds==i] != pred3)), 
                     mean((df$Result[folds==i] != pred1)),
                     mean( (df$Result[folds==i] != pred8)) )
}
cv.errors

cv.mean.errors <- colMeans(cv.errors)
cv.mean.errors
# glm3      glm1      glm8 
#0.3643939 0.3909091 0.3825758 

#we choose glm3
df_best <- df_bat[ , c('Result', 'team_code', 'avg_wickets_out','avg_win_rate')]
glm3_model <- glm(Result ~ team_code+avg_win_rate + avg_wickets_out, 
                  data = df_best, family = "binomial")
summary(glm3_model)
confint(glm3_model)

prob_win_glm3 <- predict(glm3_model, df, type = "response")
prob_win_glm3
hist(prob_win_glm3, 
     main='Histogram of Predicted Probabilities in Best Subset Seletion Model')

yhat_glm3 = as.numeric(ifelse(prob_win_glm3 > 0.5, 1, 0)) 
train_error_glm3 = 1 - mean(yhat_glm3 == df_best$Result)
train_error_glm3
#Train error (best subset with 8 variables): 0.3391304
#Train Accuracy of the best subset model:  0.6608696

test_error_glm3 = general_cv(df_best[,2:4], df_best[,1], 
                             fold_index, logistic_reg_fold_error)
test_error_glm3
#Test error (best subset with 8 variables): 0.4
#test accuracy of the best subset model: 0.6

#model3----forward stepwise selection with AIC criterion (same with best selection)
#model3==model2
null_logit_model = glm(Result~1, data=df, family = "binomial")
forward_step_model <- step(null_logit_model, scope=formula(binomial_model), 
                           direction="forward") 
#Result ~  avg_wickets_out + team_code + avg_win_rate

#model4 LDA with 3vars
lda_model = lda(Result ~ team_code+avg_win_rate + avg_wickets_out, data = df_best)
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
#Training error (LDA with 3 vars): 0.3565217
#train accuracy(LDA with 3 vars): 0.6434783

lda_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y=y)
  if(ncol(Xy)>1) tmp_fit = lda(y ~ ., data=Xy[!test_data,]) 
  else tmp_fit = lda(y ~ 1, data=Xy[!test_data,,drop=FALSE]) 
  yhat = predict(tmp_fit, Xy[test_data,,drop=FALSE])$class
  yobs = y[test_data]
  test_error = 1 - mean(yobs == yhat)
  return(test_error)
}

lda_test_error = general_cv(df_best[,2:4], df_best[,1], fold_index, lda_fold_error)
lda_test_error
#LDA test error:0.3913043
#LDA test accuracy: 0.6086957

#Quadratic Discriminant Analysis (QDA)
#model5----QDA with best subset selection model (8 vars)
qda_model = qda(Result ~team_code+avg_win_rate + avg_wickets_out, data = df_best) 
summary(qda_model)

qda_predict = predict(qda_model, df_best)
qda_win_pro = qda_predict$class
qda_win_pro

confusion_qda = table(Observed=df_best$Result, Predicted=qda_win_pro) 
confusion_qda

1-mean(df_best$Result==qda_win_pro) 
#QDA training error with forward subset: 0.3652174
#QDA train accuracy: 0.6347826

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

qda_test_error = general_cv(df_best[,2:4], df_best[,1], fold_index, qda_fold_error)
qda_test_error
#QDA test error: 0.4
#QDA test accuracy:0.6

variables <- names(df_best)[2:4]  
errors <- numeric(length(variables))
for (i in seq_along(variables)) {
  formula <- as.formula(paste("Result ~", paste(variables[variables != variables[i]], collapse = " + "), sep = " "))
  model <- qda(formula, data = df_best)
  pred <- predict(model, df_best)$class
  errors[i] <- mean(pred != df_best$Result)
}

importance <- data.frame(Variable = variables, Error = errors)
print(importance)



#model6:polynomial naive Bayesian model
nb_model <- naiveBayes(Result ~ ., data = df)
summary(nb_model)
print(nb_model)

nb_model$tables 
#important variables:  match_number, avg_wickets_out, venue_city_code,  team_code

prob_predict_nb <- predict(nb_model, df, type="raw")
print(prob_predict_nb)

predicted_nb <- factor(ifelse(prob_predict_nb[,2] > 0.5, 1, 0), levels = c(0,1))
confusion_nb = table(Observed=df$Result, Predicted=predicted_nb) 
confusion_nb

1-mean(df$Result==predicted_nb )
#naive Bayesian train error:0.3652174
#train Accuracy of the model: 0.6347826

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

test_error_naive_bayes = general_cv(df[, 2:p+1], df[, 1], 
                                    fold_index, naive_bayes_fold_error)
test_error_naive_bayes
#Test error for naive bayes: 0.4782609
#test accuracy for naive bayes: 0.5217391


set.seed(472)
### Standard Cart:model7
unpruned_tree_model = tree(Result ~., data = df)
plot(unpruned_tree_model);text(unpruned_tree_model, pretty = 0)
title("Unpruned Classification Tree", line = +3)

summary(unpruned_tree_model) #Classification tree
#training classification error rate is 17.39%
#"avg_wickets_out" "opponent_code"   "team_code"       "avg_score"      
# "home_advantage"  "match_number"    "avg_win_rate"    "season"   

unpruned_prob <- predict(unpruned_tree_model, df, type = "vector")
unpruned_prob

train_predictions_unpruned <- predict(unpruned_tree_model, df, type = "class")
table(df$Result,train_predictions_unpruned)

train_error_unpruned <- mean(train_predictions_unpruned != df$Result)
train_error_unpruned
#Unpruned train error: 0.173913, unpruned train accuracy: 0.826087

# 10-fold CV Test Error for Unpruned
folds <- createFolds(df$Result, k = 10)
cv_errors_unpruned <- vector("numeric", 10)
for (i in 1:10) {
  train_data <- df[-folds[[i]], ] 
  test_data <- df[folds[[i]], ]   
  unpruned_model <- tree(Result ~ ., data = train_data) 
  unpruned_predictions <- predict(unpruned_model, test_data, type = "class") 
  cv_errors_unpruned[i] <- mean(unpruned_predictions != test_data$Result) 
}

mean_test_error_unpruned <- mean(cv_errors_unpruned)
mean_test_error_unpruned 
#unpruned test error: 0.4421329
#unpruned test accuracy: 0.5578671

### Pruned Tree-model8
cv_tree <- cv.tree(unpruned_tree_model, FUN = prune.misclass)
cv_tree

plot(cv_tree)
title("Deviance as Size of Pruned Tree Increases", line = +3)

pruned_tree_model = prune.misclass(unpruned_tree_model, best = 5)
pruned_tree_model
summary(pruned_tree_model)
#Misclassification error rate: 0.2696
#"avg_wickets_out" "avg_win_rate"    "team_code"       "opponent_code"  

plot(pruned_tree_model);text(pruned_tree_model, pretty = 0)
title("Pruned Classification Tree", line = +3)

pruned_prob <- predict(pruned_tree_model, df, type = "vector")
pruned_prob

train_predictions_pruned <- predict(pruned_tree_model, df, type = "class")
table(df$Result,train_predictions_pruned)
train_error_pruned <- mean(train_predictions_pruned != df$Result)
train_error_pruned
# train error pruned:0.2695652, train accuracy:0.7304348

# 10-fold CV Test Error for Pruned
folds <- createFolds(df$Result, k = 10)
cv_errors_pruned <- vector("numeric", 10)
for(i in 1:10) {
  train_data <- df[-folds[[i]], ]
  test_data <- df[folds[[i]], ]
  pruned_model <- prune.tree(tree(Result ~., data = train_data, method = "class"), best = 5)
  pruned_predictions <- predict(pruned_model, test_data, type = "class")
  cv_errors_pruned[i] <- mean(pruned_predictions != test_data$Result)
}
mean_test_error_pruned <- mean(cv_errors_pruned)
mean_test_error_pruned
#pruned tree test error: 0.4545455
#test accuracy:0.5454545


### Bagging----model9 (mtry=11)
set.seed(472)
bag.10=randomForest(Result~.,data=df,mtry=11,ntree=10,importance=TRUE)
bag.10
summary(bag.10)
#test error and test accuracy for bag.10
folds <- createFolds(df$Result, k = 10)
cv_errors_bagging.10 <- vector("numeric", 10)
for (i in 1:10) {
  train_data <- df[-folds[[i]], ]
  test_data <- df[folds[[i]], ]
  bag_model <- randomForest(Result ~ ., train_data, ntree = 10)
  bag_test_predictions <- predict(bag_model, test_data, type='class')
  cv_errors_bagging.10[i] <- mean(bag_test_predictions != test_data$Result)
}
mean_test_error_bagging.10 <- mean(cv_errors_bagging.10)
mean_test_error_bagging.10  

bag.100=randomForest(Result~.,data=df,mtry=11,ntree=100,importance=TRUE)
bag.100
#test error and test accuracy for bag.100
folds <- createFolds(df$Result, k = 10)
cv_errors_bagging.100 <- vector("numeric", 10)
for (i in 1:10) {
  train_data <- df[-folds[[i]], ]
  test_data <- df[folds[[i]], ]
  bag_model <- randomForest(Result ~ ., train_data, ntree = 100)
  bag_test_predictions <- predict(bag_model, test_data, type='class')
  cv_errors_bagging.100[i] <- mean(bag_test_predictions != test_data$Result)
}
mean_test_error_bagging.100 <- mean(cv_errors_bagging.100)
mean_test_error_bagging.100  

bag.1000=randomForest(Result~.,data=df,mtry=11,ntree=1000,importance=TRUE)
bag.1000
summary(bag.1000)
#test error and test accuracy for bag.1000
folds <- createFolds(df$Result, k = 10)
cv_errors_bagging.1000 <- vector("numeric", 10)
for (i in 1:10) {
  train_data <- df[-folds[[i]], ]
  test_data <- df[folds[[i]], ]
  bag_model <- randomForest(Result ~ ., train_data, ntree = 1000)
  bag_test_predictions <- predict(bag_model, test_data, type='class')
  cv_errors_bagging.1000[i] <- mean(bag_test_predictions != test_data$Result)
}
mean_test_error_bagging.1000 <- mean(cv_errors_bagging.1000)
mean_test_error_bagging.1000  

predicted_probabilities_bagging <- predict(bag.1000, df, type = "prob")
predicted_probabilities_bagging

train_predictions_bagging <- predict(bag.1000, df,type='class')
train_error_bagging <- mean(train_predictions_bagging != df$Result)
train_error_bagging #bagging train error:0, train accuracy:1, which may be an indication of overfitting

#check feature importance in bagging model
summary(bag.1000)
importance(bag.1000)
importance(bag.1000)[,4]>9.090909
#important vars:  avg_score, avg_wickets_out 

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

randomForest::getTree(bag.1000, k = 1, labelVar = TRUE)
plot(bag.1000, main = "Bagging Model")


### Random Forest- model10 (p=11, mtry= 3/4)
#Regression tree mtry = p/3, classification tree mtry = sqrt(p)
rf.1000 = randomForest(Result~ ., data = df, 
                       mtry=3, ntree=1000, importance=TRUE)
rf.1000 #OOB estimate of  error rate: 40%

train_predictions_rf <- predict(rf.1000, df, type='class')
train_error_rf <- mean(train_predictions_rf != df$Result)
train_error_rf #random forest train error:0, train accuracy:1, maybe overfitting

predicted_probabilities_rf <- predict(rf.1000, df, type = "prob")
predicted_probabilities_rf

# 10-fold cv test error for Random Forest
folds <- createFolds(df$Result, k = 10)
cv_errors_rf <- vector("numeric", 10)
for (i in 1:10) {
  train_data <- df[-folds[[i]],]
  test_data <- df[folds[[i]],]
  rf_model_cv <- randomForest(Result ~ ., data = train_data, mtry = 3, ntree = 1000)
  rf_test_predictions <- predict(rf_model_cv, test_data, type='class')
  cv_errors_rf[i] <- mean(rf_test_predictions != test_data$Result)
}
mean_test_error_rf <- mean(cv_errors_rf)
mean_test_error_rf 

#chekc feature importance in random forest model
importance(rf.1000 )
importance(rf.1000 )[,4]>9.090909
#importance variables: avg_wickets_out 
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
df$Result <- unclass(df$Result)-1
table(df$Result)

boosting = gbm(Result~., data= df, distribution = 'bernoulli',
               n.trees = 1000, interaction.depth = 2)
# Perform 10-fold CV for mean test error
folds <- createFolds(df$Result, k = 10)
cv_errors <- vector("numeric", 10)
for (j in 1:10) {
  train_data <- df[-folds[[j]],]
  test_data <- df[folds[[j]],]
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
mean_test_error #0.4530303
#test accuracy for boosting: 0.5469697

boosting.1 = gbm(Result~., data= df, distribution = 'bernoulli', shrinkage = 0.1,
                 n.trees = 1000, interaction.depth = 1)
# Perform 10-fold CV for mean test error
folds <- createFolds(df$Result, k = 10)
cv_errors <- vector("numeric", 10)
for (j in 1:10) {
  train_data <- df[-folds[[j]],]
  test_data <- df[folds[[j]],]
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
mean_test_error #0.4348485
#test accuracy for boosting.1: 0.5651515

boosting.4 = gbm(Result~., data= df, distribution = 'bernoulli', shrinkage = 0.1,
                 n.trees = 1000, interaction.depth = 4)
# Perform 10-fold CV for mean test error
folds <- createFolds(df$Result, k = 10)
cv_errors <- vector("numeric", 10)
for (j in 1:10) {
  train_data <- df[-folds[[j]],]
  test_data <- df[folds[[j]],]
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
mean_test_error #0.4340909
#test accuracy for boosting.4: 0.5659091
#boosting.4 is best boosting model

predicted_probabilities <- predict(boosting.4, df, n.trees = 1000, 
                                   shrinkage = 0.1, interaction.depth = 4,type = "response")
predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)
hist(predicted_probabilities, 
     main="Histogram of Predicted Probabilities in Boosting Model")

# Calculate training error
cm_boost <- table(predicted_labels, df$Result)
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

#avg_wicket_out, avg_score, avg_win_rate, match_number
gbm::plot.gbm(boosting.4, i.trees = 1, main = "Boosting Model")


# Compare
df$Result <- as.factor(df$Result)
df_best$Result <- as.factor(df_best$Result)

accuracy_full <- vector("numeric", nfolds)
accuracy_best <- vector("numeric", nfolds)

accuracy_lda <- vector("numeric", nfolds)
accuracy_qda <- vector("numeric", nfolds)
accuracy_nb <- vector("numeric", nfolds)
accuracy_unpruned <- vector("numeric", nfolds)
accuracy_pruned <- vector("numeric", nfolds)
accuracy_bagging <- vector("numeric", nfolds)
accuracy_rf <- vector("numeric", nfolds)
accuracy_boosting <- vector("numeric", nfolds)

precision_full <- vector("numeric", nfolds)
precision_best <- vector("numeric", nfolds)

precision_lda <- vector("numeric", nfolds)
precision_qda <- vector("numeric", nfolds)
precision_nb <- vector("numeric", nfolds)
precision_unpruned <- vector("numeric", nfolds)
precision_pruned <- vector("numeric", nfolds)
precision_bagging <- vector("numeric", nfolds)
precision_rf <- vector("numeric", nfolds)
precision_boosting <- vector("numeric", nfolds)

recall_full <- vector("numeric", nfolds)
recall_best <- vector("numeric", nfolds)

recall_lda <- vector("numeric", nfolds)
recall_qda <- vector("numeric", nfolds)
recall_nb <- vector("numeric", nfolds)
recall_unpruned <- vector("numeric", nfolds)
recall_pruned <- vector("numeric", nfolds)
recall_bagging <- vector("numeric", nfolds)
recall_rf <- vector("numeric", nfolds)
recall_boosting <- vector("numeric", nfolds)

f1_full <- vector("numeric", nfolds)
f1_best <- vector("numeric", nfolds)

f1_lda <- vector("numeric", nfolds)
f1_qda <- vector("numeric", nfolds)
f1_nb <- vector("numeric", nfolds)
f1_unpruned <- vector("numeric", nfolds)
f1_pruned <- vector("numeric", nfolds)
f1_bagging <- vector("numeric", nfolds)
f1_rf <- vector("numeric", nfolds)
f1_boosting <- vector("numeric", nfolds)

for(fold in 1:nfolds) {
  test_data_index <- fold_index == fold
  train_data_index <- !test_data_index
  
  # Train Models
  full_fit <- glm(Result ~ ., data = df[train_data_index,], family="binomial")
  best_fit <- glm(Result ~team_code+avg_win_rate + avg_wickets_out, data = df[train_data_index,], family = "binomial")
  
  lda_fit <- lda(Result ~ team_code+avg_win_rate + avg_wickets_out, data=df_best[train_data_index,])
  qda_fit <- qda(Result ~ team_code+avg_win_rate + avg_wickets_out,  data=df_best[train_data_index,]) 
  
  nb_fit <- naiveBayes(Result ~ ., data = df[train_data_index,])
  
  unpruned_fit <- tree(Result ~ ., data = df[train_data_index,])
  pruned_fit <- prune.misclass(unpruned_fit, best=5)
  bag_fit <- randomForest(Result ~ ., data = df[train_data_index,], 
                          ntree=1000, mtry=11, importance=TRUE)
  rf_fit <- randomForest(Result ~ ., data = df[train_data_index,], 
                         ntree=1000, mtry=3, importance=TRUE)
  boost_fit <- gbm(unclass(Result)-1 ~ ., data = df[train_data_index,], distribution = "bernoulli",
                   n.trees = 1000, shrinkage = 0.1, interaction.depth = 4)
  
  # Predict with models on test set
  phat_full <- predict(full_fit, df[test_data_index,,drop=FALSE], type="response")
  yhat_full <- ifelse(phat_full > 0.5, 1, 0)
  yobs_full <- df$Result[test_data_index]
  
  phat_best <- predict(best_fit, df[test_data_index,,drop=FALSE], type="response")
  yhat_best <- ifelse(phat_best > 0.5, 1, 0)
  yobs_best <- df$Result[test_data_index]
  
  phat_lda <- predict(lda_fit, df_best[test_data_index,,drop=FALSE])
  yhat_lda <- phat_lda$class
  yobs_lda <- df_best$Result[test_data_index]
  
  phat_qda <- predict(qda_fit, df_best[test_data_index,,drop=FALSE])
  yhat_qda <- phat_qda$class
  yobs_qda <- df_best$Result[test_data_index]
  
  phat_nb <- predict(nb_fit, df[test_data_index,,drop=FALSE], type="raw")[,2]
  yhat_nb <- ifelse(phat_nb > 0.5, 1, 0)
  yobs_nb <- df$Result[test_data_index]
  
  phat_unpruned <- predict(unpruned_fit, df[test_data_index,,drop=FALSE], type="vector")[,2]
  yhat_unpruned <- ifelse(phat_unpruned > 0.5, 1, 0)
  yobs_unpruned <- df$Result[test_data_index]
  
  phat_pruned <- predict(pruned_fit, df[test_data_index,,drop=FALSE], type="vector")[,2]
  yhat_pruned <- ifelse(phat_pruned > 0.5, 1, 0)
  yobs_pruned <- df$Result[test_data_index]
  
  phat_bagging <- as.numeric(predict(bag_fit, df[test_data_index,,drop=FALSE], type="class"))
  yhat_bagging <- ifelse(phat_bagging > 0.5, 1, 0)
  yobs_bagging <- df$Result[test_data_index]
  
  phat_rf <- as.numeric(predict(rf_fit, df[test_data_index,,drop=FALSE], type="class"))
  yhat_rf <- ifelse(phat_rf > 0.5, 1, 0)
  yobs_rf <- df$Result[test_data_index]
  
  phat_boosting <- as.numeric(predict(boost_fit, df[test_data_index,,drop=FALSE], type="response"))
  yhat_boosting <- ifelse(phat_boosting > 0.5, 1, 0)
  yobs_boosting <- df$Result[test_data_index]
  
  CM_full <- table(Observed = yobs_full, Predicted = yhat_full)
  CM_best <- table(Observed = yobs_best, Predicted = yhat_best)
  
  CM_lda <- table(Observed = yobs_lda, Predicted = yhat_lda)
  CM_qda <- table(Observed = yobs_qda, Predicted = yhat_qda)
  CM_nb <- table(Observed = yobs_nb, Predicted = yhat_nb)
  CM_unpruned <- table(Observed = factor(yobs_unpruned, levels=c(0,1)),
                       Predicted = factor(yhat_unpruned, levels=c(0,1)))
  CM_pruned <- table(Observed = factor(yobs_pruned, levels=c(0,1)),
                     Predicted = factor(yhat_pruned, levels=c(0,1)))
  CM_bagging <- table(Observed = factor(yobs_bagging, levels=c(0,1)),
                      Predicted = factor(yhat_bagging, levels=c(0,1)))
  CM_rf <- table(Observed = factor(yobs_rf, levels=c(0,1)),
                 Predicted = factor(yhat_rf, levels=c(0,1)))
  CM_boosting <- table(Observed = factor(yobs_boosting, levels=c(0,1)),
                       Predicted = factor(yhat_boosting, levels=c(0,1)))
  
  # Compute and Store Accuracy Metrics
  accuracy_full[fold] <- sum(diag(CM_full)) / sum(CM_full)
  accuracy_best[fold] <- sum(diag(CM_best)) / sum(CM_best)

  accuracy_lda[fold] <- sum(diag(CM_lda)) / sum(CM_lda)
  accuracy_qda[fold] <- sum(diag(CM_qda)) / sum(CM_qda)
  accuracy_nb[fold] <- sum(diag(CM_nb)) / sum(CM_nb)
  accuracy_unpruned[fold] <- sum(diag(CM_unpruned)) / sum(CM_unpruned)
  accuracy_pruned[fold] <- sum(diag(CM_pruned)) / sum(CM_pruned)
  accuracy_bagging[fold] <- sum(diag(CM_bagging)) / sum(CM_bagging)
  accuracy_rf[fold] <- sum(diag(CM_rf)) / sum(CM_rf)
  accuracy_boosting[fold] <- sum(diag(CM_boosting)) / sum(CM_boosting)
  
  recall_full[fold] <- CM_full[2,2] / (CM_full[2,2] + CM_full[2,1])
  recall_best[fold] <- CM_best[2,2] / (CM_best[2,2] + CM_best[2,1])

  recall_lda[fold] <- CM_lda[2,2] / (CM_lda[2,2] + CM_lda[2,1])
  recall_qda[fold] <- CM_qda[2,2] / (CM_qda[2,2] + CM_qda[2,1])
  recall_nb[fold] <- CM_nb[2,2] / (CM_nb[2,2] + CM_nb[2,1])
  recall_unpruned[fold] <- CM_unpruned[2,2] / (CM_unpruned[2,2] + CM_unpruned[2,1])
  recall_pruned[fold] <- CM_pruned[2,2] / (CM_pruned[2,2] + CM_pruned[2,1])
  recall_bagging[fold] <- CM_bagging[2,2] / (CM_bagging[2,2] + CM_bagging[2,1])
  recall_rf[fold] <- CM_rf[2,2] / (CM_rf[2,2] + CM_rf[2,1])
  recall_boosting[fold] <- CM_boosting[2,2] / (CM_boosting[2,2] + CM_boosting[2,1])
  
  precision_full[fold] <- CM_full[2,2] / (CM_full[2,2] + CM_full[1,2])
  precision_best[fold] <- CM_best[2,2] / (CM_best[2,2] + CM_best[1,2])
 
  precision_lda[fold] <- CM_lda[2,2] / (CM_lda[2,2] + CM_lda[1,2])
  precision_qda[fold] <- CM_qda[2,2] / (CM_qda[2,2] + CM_qda[1,2])
  precision_nb[fold] <- CM_nb[2,2] / (CM_nb[2,2] + CM_nb[1,2])
  precision_unpruned[fold] <- CM_unpruned[2,2] / (CM_unpruned[2,2] + CM_unpruned[1,2])
  precision_pruned[fold] <- CM_pruned[2,2] / (CM_pruned[2,2] + CM_pruned[1,2])
  precision_bagging[fold] <- CM_bagging[2,2] / (CM_bagging[2,2] + CM_bagging[1,2])
  precision_rf[fold] <- CM_rf[2,2] / (CM_rf[2,2] + CM_rf[1,2])
  precision_boosting[fold] <- CM_boosting[2,2] / (CM_boosting[2,2] + CM_boosting[1,2])
  
  f1_full[fold] <- 2 * (precision_full[fold] * recall_full[fold]) / (precision_full[fold] + recall_full[fold])
  f1_best[fold] <- 2 * (precision_best[fold] * recall_best[fold]) / (precision_best[fold] + recall_best[fold])
 
  f1_lda[fold] <- 2 * (precision_lda[fold] * recall_lda[fold]) / (precision_lda[fold] + recall_lda[fold])
  f1_qda[fold] <- 2 * (precision_qda[fold] * recall_qda[fold]) / (precision_qda[fold] + recall_qda[fold])
  f1_nb[fold] <- 2 * (precision_nb[fold] * recall_nb[fold]) / (precision_nb[fold] + recall_nb[fold])
  f1_unpruned[fold] <- 2 * (precision_unpruned[fold] * recall_unpruned[fold]) / (precision_unpruned[fold] + recall_unpruned[fold])
  f1_pruned[fold] <- 2 * (precision_pruned[fold] * recall_pruned[fold]) / (precision_pruned[fold] + recall_pruned[fold])
  f1_bagging[fold] <- 2 * (precision_bagging[fold] * recall_bagging[fold]) / (precision_bagging[fold] + recall_bagging[fold])
  f1_rf[fold] <- 2 * (precision_rf[fold] * recall_rf[fold]) / (precision_rf[fold] + recall_rf[fold])
  f1_boosting[fold] <- 2 * (precision_boosting[fold] * recall_boosting[fold]) / (precision_boosting[fold] + recall_boosting[fold])
}


boxplot(accuracy_full, accuracy_best, accuracy_lda,accuracy_qda, 
        accuracy_nb,
        accuracy_unpruned,accuracy_pruned,accuracy_bagging,
        accuracy_rf, accuracy_boosting,
        names = c('Full Model',
                  'Best Subset',
               
                  'LDA', 'QDA', 'Naive Bayes',
                  'Unpruned',
                  'Pruned',
                  'Bagging', 
                  'Random Forest',
                  "Boosting"),
        main = "Test Accuracy Distribution for Full-time All Models",
        xlab = "Model Type", ylab = 'Test Accuracy',
        col = viridis(10))


##### prediction set #####
df_2023 <- read.csv("~/Desktop/all_code/dissertation_dataset/prediction_set_encoding_fulltime_model.csv")
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

prob_predict_best_sel <- predict(glm3_model, df_2023, type="response")
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
