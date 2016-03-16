#Sonali Guleria
#11-13-2015

##import
library(e1071)
library(plyr)

#replace *** with your file name
my.data <- import.csv("****.csv", sep = ",", header = TRUE)
my.data <- transform( my.data, type=factor( mapvalues( type, c("high","low"),c(1,0))))
set.seed(1)
# function for logistic
get_pred_logreg <- function(train,test)
{
  options(scipen=999)
  last.index <- ncol(train)
  colnames(train)[last.index] <- "train.predictor"
  train.predictor <- train[,-last.index]
  train.log<-glm(train.predictor~.,train,family=binomial) 
  true.test <- as.data.frame(test[,last.index])
  predicted.test<-predict(train.log,test,type="response") 
  predicted.test <- 1-predicted.test
  test.results <- as.data.frame(cbind(predicted.test,true.test))
  return(test.results)
  
}


#function for svm

get_pred_svm<-function(train,test)
{
  
  last.index <- ncol(train)
  names(train)[last.index] <- "last"
  svm.model <- svm(as.factor(last)~.,data=train,probability=TRUE)
  predicted.svm <- predict(svm.model,test,probability=TRUE)
  test.true <- test[,last.index]
  predicted.svm.prob <- as.data.frame(attr(predicted.svm,"probabilities"))
  predicted.svm.prob.high <-  as.data.frame(predicted.svm.prob[,1])
  colnames(predicted.svm.prob.high) <- "predicted.svm.prob.high"
  svm.result<- as.data.frame(cbind(as.data.frame(predicted.svm.prob.high),as.data.frame(test.true)))
  return(svm.result)
  
}


# bayesNaive

get_pred_nb <- function(train,test)
  
{
  options(scipen=999)
  last.index <- ncol(train)
  test.true <- test[, last.index] 
  names(train)[last.index] <- "last"
  naiveBayes.model <- naiveBayes(as.factor(last)~ ., data = train,type="raw")
  predicted.naiveBayes <- predict(naiveBayes.model,test,type="raw")
  predicted.naiveBayes.high <- predicted.naiveBayes[,1]
  naiveBayes.result<- as.data.frame(cbind(as.data.frame(predicted.naiveBayes.high),as.data.frame(test.true)))
  return(naiveBayes.result)
  
}


library(class)

## k nearest

get_pred_knn <- function(train,test,k)
{
  nf <- ncol(train)
  input <- as.data.frame(train[,-nf])
  query <- as.data.frame(test[,-nf])
  test.true <- test[ ,nf]
  cl <- train[,nf]
  knn.model<- knn(input,query,cl=cl,k=k,prob=TRUE,use.all = FALSE)
  knn.predict<- as.data.frame(knn.model)
  knn.prob<-attr(knn.model,"prob")
  knn.compare <- cbind(knn.predict,knn.prob)
  
  calcProb <- function(x)
  {
    out <- data.frame()
    for(i in 1: nrow(x))
      if(x[i,1]==1)
      {
        out[i,1] <- x[i,2]
      }
    else
    {
      out[i,1]<- 1-x[i,2]
    }
    return(out)
  }
  knn.prob.high <- calcProb(knn.compare)
  names(knn.prob.high)<-"knn.prob.high"
  knn.result<- cbind(knn.prob.high,test.true)
  return(knn.result)
  
}

# do cv for the above three models

do_cv_class <- function(df,num_folds,model_name) {
  total.rows <- nrow(df)
  shuffle.indexes <- sample(total.rows,total.rows)
  shuffled.df <- as.data.frame(df[c(shuffle.indexes),])
  row.names(shuffled.df)<-1:total.rows
  partition.size <- as.integer(total.rows /num_folds)
  out <- rep(1:num_folds,each=partition.size)
  out<- c(out,rep(num_folds,nrow(shuffled.df)-num_folds* partition.size))

  fitted.result.prob<- data.frame()

  for(i in 1:num_folds)
  {
   

    test <- shuffled.df[which(out==i),]
    

    train<- shuffled.df[which(out!=i),]
    
    if(model_name == "logreg")
     
    {fitted.result <- get_pred_logreg(train,test)}
     if(model_name == "svm")
        
      {fitted.result <- get_pred_svm(train,test)}
   
      if(model_name =="nb")
        
      { fitted.result <- get_pred_nb(train,test) }

      if((substr(model_name,nchar(model_name)-1,nchar(model_name)) == "NN"))
      {
        
        l <- regexpr("N",model_name)[1] 
        l <- l-1
        k <- as.numeric(substr(model_name,1,l))
        fitted.result <- get_pred_knn(train,test,k)
        
      }
    
    fitted.result.prob <- rbind.data.frame(as.data.frame(fitted.result.prob), fitted.result)
  }

  colnames(fitted.result.prob)<- c("Predict.Prob.High" , "True.Value")
  return(fitted.result.prob)
}

# get metrics for the models with default cutoff .5
get_metrics <- function(pdf,cutoff = 0.5){
  
  for(i in 1:nrow(pdf))
  {
    if(pdf[i,1] < cutoff )
    {
      pdf[i,1] <- 0
      
    }
    else
      pdf[i,1] <- 1
  }
  tp <- 0
  tn<-0
  fp <- 0
  fn <- 0
  for(i in 1:nrow(pdf))
  {
    if(( (pdf[i,1] == 1) & (pdf[i,2]== 1) ))
    {
      tp <- tp +1 
    }
    else
      if(( (pdf[i,1])== 0 & (pdf[i,2]== 0)))
      {
        tn <- tn +1
      }
    else
      if(((pdf[i,1])== 1 & (pdf[i,2]==0)))
      {
        fp <-fp+1
      }
    else
      if(((pdf[i,1])== 0 & (pdf[i,2]==1)))
      {
        fn <- fn +1
      }
    
  }
  pos <-tp +fn
  neg <- tn + fp
  Ppos<- tp+fp
  Pneg <- tn + fn
  N = pos +neg
  tpr <- tp/pos
  fpr <-fp/neg
  acc <- (tp+tn)/N
  precision <- tp/Ppos
  recall<-tp/pos
  result <- data.frame(tpr,fpr,acc,precision,recall)
  colnames(result) <- c("True.Positive.Rate","False.Positive.Rate","Accuracy","Precision","Recall")
  return(result)
}  




log <- do_cv_class(my.data,10,"logreg")
metric.log <- get_metrics(log)

svm <- do_cv_class(my.data,10,"svm")
metric.svm <- get_metrics(svm)

nb <- do_cv_class(my.data,10,"nb")
metric.nb<- get_metrics(nb)

neigh <- do_cv_class(my.data,10,"5NN")
metric.neigh <- get_metrics(neigh)


#default classfier cross validation
do_cv_class_def <- function(df,num_folds,model_name) {
  
  total.rows <- nrow(df)
  shuffle.indexes <- sample(total.rows,total.rows)
  shuffled.df <- as.data.frame(df[c(shuffle.indexes),])
  row.names(shuffled.df)<-1:total.rows
  partition.size <- as.integer(total.rows /num_folds)
  out <- rep(1:num_folds,each=partition.size)
  out<- c(out,rep(num_folds,nrow(shuffled.df)-(num_folds* partition.size)))
  fitted.result.prob<- data.frame()
  
  for(i in 1:num_folds)
  {
    
    test <- shuffled.df[which(out==i),]
    train<- shuffled.df[which(out!=i),]
    
    if(model_name=="default")
    {
      train.value <- as.data.frame(train[,ncol(train)])
      train.value <-  table(train.value)
      train.value <- as.data.frame(train.value)
      if((train.value[1,2])>(train.value[2,2]))
      { 
        def.value <- as.numeric(train.value[1,1])
      }
      
      fitted.result.prob1 <- as.data.frame(rep(def.value,nrow(test)))
      fitted.result.prob1[,2]<- test[,ncol(test)]
      fitted.result.prob <- rbind(fitted.result.prob, fitted.result.prob1)
      
    }
    fitted.result.prob<- as.data.frame(fitted.result.prob)  
  }
  colnames(fitted.result.prob)<- c("Predict.Prob.High" , "True.Value")
  return(fitted.result.prob)
  
}


def <- do_cv_class_def(my.data,10,"default")
metric.def <- get_metrics(def)


cat("\n*************Summary Result*************\n\n")
summary.metric<-as.data.frame(rbind(metric.log,metric.svm,metric.nb,metric.neigh,metric.def))
row.names(summary.metric) <- c("metric.log","metric.svm","metric.nb","metric.neigh","metric.def")
print(summary.metric)
cat("\n\n")
########################
result.general <- data.frame()
out1 <- as.numeric()
for(i in 1:30)
{

  result.gen <- get_metrics(do_cv_class(my.data,10,paste(i,"NN",sep="") ))
  
  result.general <- rbind(result.general,result.gen)
  out1[i]<- i
}
results.general <- cbind(out1,result.general[,3])
results.general <- as.data.frame(results.general)
colnames(results.general)<- c("k", "Accuracy")
print(results.general)

library(ggplot2)

z<- results.general[which.max(results.general[,2]),1]                                                        


gen.plot <- ggplot( data=results.general, aes( x=k, y=Accuracy) ) + geom_line()+ labs(title="Generalization Curve: Accuracy Vs k") +
  labs(x="k-value", y="Accuracy :  Test Data")+geom_vline(aes(xintercept=c(z)),linetype = "longdash",color="red")
print(gen.plot)

