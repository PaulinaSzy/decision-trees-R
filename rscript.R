library(caTools)
library(RWeka)
library(caret)
library(e1071)

diabetes <- read.csv(file="diabetes.csv",head=TRUE,sep=",")

spl = sample.split(diabetes[,1], SplitRatio = 0.7)

dataTrain = subset(diabetes, spl==TRUE)
dataTest = subset(diabetes, spl==FALSE)
true_labels = dataTest$Outcome


train <- function(data_train,class_name, folds=10, strat=FALSE, ...) {
  if(isTRUE(strat)){
    cvIndex <- createFolds(factor(data_train$class_name), folds, returnTrain = T)
    tc <- trainControl(index = cvIndex, method = 'cv', number = folds)
  }
  else{
    tc <- trainControl(method = 'cv', number = folds)
  }
  C45Fit <- train(as.factor(class_name)~., method="J48", data=data_train, tuneLength = 5, trControl = tc)
}

predictt <- function(model, data_test) {
  data_test.pred <- predict(model, newdata = data_test)
}


accuracy <- function(true_label,pred_label) {
  sum(pred_label == true_label) / length(pred_label)
}	


f1 <- function(conf_matr) {
  tn = conf_matr[1,1]
  tp = conf_matr[2,2]
  fn = conf_matr[2,1]
  fp = conf_matr[1,2]
  prec = (tp + 1) / (tp + fp + 1*2)
  recall = (tp + 1) / (tp + fn + 1*2)
  f1_score = 2 * (prec * recall / (prec + recall))
}


model <- train(train_data, Outcome, 10, strat=FALSE)
pred <- predictt(model, test_data)
res <- accuracy(true_labels, pred)
vector <- c(vector, res)

fold_num <- function(folds, train_data, test_data) {
  vector <- c()
  for (val in folds) {
    print(val)
    model <- train(train_data, Outcome, val, strat=FALSE)
    pred <- predictt(model, test_data)
    res <- accuracy(true_labels, pred)
    vector <- c(vector, res)
  }
  vector
}

print(fold_num(c(2,3),dataTrain,dataTest))

