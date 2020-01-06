library(caTools)
library(RWeka)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(dismo)
library(partykit)
library(float)

diabetes <- read.csv(file="diabetes.csv",head=TRUE,sep=",")
glass <- read.csv(file="glass.csv",head=TRUE,sep=",")
wine <- read.csv(file="wine.csv",head=TRUE,sep=",")

colnames(wine) = c('Wine','Alcohol','Malic.acid','Ash','Acl','Mg','Phenols','Flavanoids','Nonflavanoid.phenols','Proanth','Color.int','Hue','OD','Proline')

spl = sample.split(diabetes[,1], SplitRatio = 0.7)

dataTrain = subset(diabetes, spl==TRUE)
dataTest = subset(diabetes, spl==FALSE)
true_labels = dataTest$Outcome
folds = 10

kfold(diabetes, k=10)
print(length(f[1]))

get_model <- function(train, con=0.25, unpruned = FALSE, m = 25, colap = FALSE) {
  #J48(as.factor(Outcome)~., train, control = Weka_control(M = m, U = unpruned, O = colap)) 
  #J48(as.factor(Outcome)~., train, control = Weka_control(U = unpruned, C=con))
  #J48(as.factor(Type)~., train, control = Weka_control(M = m, U = unpruned, O = colap)) 
  J48(as.factor(Wine)~., train, control = Weka_control(M = m, U = unpruned, O = colap)) 
  
  }

cross <- function(d, f) {
  #tc <- trainControl(method = 'cv', number = f)
  train_data <- kfold(diabetes, k=10)
  # train_data <- createFolds(diabetes$Outcome, k=f)
  model <- train(Outcome~., method="J48", data=diabetes, trControl = trainControl(
    method="cv", indexOut=diabetes))
  
  # train_data <- createFolds(d$Type, k=f)
  # model <- train(Type~., method="J48", data=d, trControl = trainControl(
  #   method="cv", indexOut=train_data))
}
cross(diabetes, 10)

accuracy <- function(true_label,pred_label) {
  sum(pred_label == true_label) / length(pred_label)
}	

cv <- function(data,fnum,strat,conf) {
  
  if (strat == TRUE){
    folds <- kfold(data, k=fnum, by=data$Type)
  }
  else{
    print('ddnje')
    folds <- kfold(data, k=fnum)
  }
  res = c()
  for(i in 1:fnum){
   
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    
    model = J48(as.factor(Wine)~., trainData, control = Weka_control(C=conf)) 
    
    pred <- strtoi(predict(model, newdata = testData))

    f = f1multi(testData$Wine, pred)
    res <- c(res, f)
  }
  return (sum(res)/length(res))
}

#print(cv(wine,10))

cross_val <- function(model, f, comp = FALSE, s = 1, clas = TRUE) {
  evaluate_Weka_classifier(model, numFolds = f, complexity = comp, seed = s, class = clas)
  #evaluate_Weka_classifier(model, numFolds = f)
  
  }

f1 <- function(conf_matr) {
  print(conf_matr)
  tn = conf_matr[1,1]
  tp = conf_matr[2,2]
  fn = conf_matr[2,1]
  fp = conf_matr[1,2]
  prec = (tp + 1) / (tp + fp + 1*2)
  recall = (tp + 1) / (tp + fn + 1*2)
  2 * (prec * recall / (prec + recall))
}

f1multi <- function(true_labels, pred_labels){
  res = 0
  uni_labels = unique(c(true_labels, pred_labels))
  print(uni_labels)
  size = length(uni_labels)
  for (u in uni_labels){
    print(u)
    f = f1(table(u == true_labels, u == pred_labels))
    res = res + f
  }
  res/size
}

extract_f1 <- function(model){
  print(model$detailsClass)
  print(length(model$detailsClass[,5]))
  sum(model$detailsClass[,5])/length(model$detailsClass[,5])
  
}

get_model_info <- function(model) {
  print(summary(model))
  #plot(model)
}

folds = c(3,5,10,15)
min_leaf_num = c(2,5,10,15,20,25,35,50)
conns = c(0.05,0.10,0.15,0.25,0.35)

get_results <- function(folds, data, clas) {
  res = c()

  for (m in min_leaf_num) {
   
    model <- get_model(data, con = cc, FALSE, m)
    cv_model = cross_val(model,10, cl=clas)
    #cv_model = cross(data,10)
    
    if (m == 10){
    #   #print(cv_model$confusionMatrix)
       plot(model)
    #   #print(cv_model$detailsClass)
    #   #print(extract_f1(cv_model))
    }
   
    #f_sc = cv(data,f, TRUE)
    
    f_sc = extract_f1(cv_model)
    
    res <- c(res, f_sc)
  }
  return(res)
}

#get_results(folds, diabetes, TRUE)

#get_results(folds, glass, TRUE)

get_results(folds, wine, TRUE)
