#setwd("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto")
#Required libraries:
require(data.table)
require(tm)
library(ROCR)
library(caret)
library(randomForest)

get.standard.record <- function(columnNames,dataFrame){
  
  dataFrame <- as.data.frame(dataFrame)
  #print (dataFrame)

  cols <- !(names(dataFrame) %in% columnNames)
  for(i in names(dataFrame)[cols]) {
    dataFrame[,i] <- NULL
  }

  cols <- jobs_signficative_terms[!(columnNames %in% names(dataFrame))]
  for(i in cols) {
    dataFrame[,i] <- 0
  }
  
  dataFrame <- dataFrame[,order(names(dataFrame))]
  
  return (dataFrame)
}


#Adapts tdm for regression models.
convert_count <- function(x) {
  y <- x
  y
}


load ("./sample.jobs.messages.processed.rda")
load("./sample.job.messages.dtm.rda")
load("./sample.job.messages.corpus.rda")
load ("./jobs_classifier.anova.rda")

(sample.job.messages.dtm)

jobs_classifier.anova

jobs_indices <- which(sample.job.messages$is_job_offer_factor == 1)
noise_indices <- which(sample.job.messages$is_job_offer_factor == 0)
training_indices <- which(sample.job.messages$is_for_training == 1)
testing_indices <- which(sample.job.messages$is_for_training == 0)

jobs_dtm_train <- sample.job.messages.dtm[training_indices,]
jobs_dtm_test <- sample.job.messages.dtm[testing_indices,]
jobs_corpus_train <- sample.job.messages.corpus[training_indices]
jobs_corpus_test <- sample.job.messages.corpus[testing_indices]
jobs_raw_train <- sample.job.messages[training_indices]
jobs_raw_test <- sample.job.messages[testing_indices]

five_times_words <- findFreqTerms(jobs_dtm_train, 5)
job_train <- DocumentTermMatrix(jobs_corpus_train, control=list(dictionary = five_times_words,wordLengths=c(3,Inf)))
job_test <- DocumentTermMatrix(jobs_corpus_test, control=list(dictionary = five_times_words,wordLengths=c(3,Inf)))

job_train <- apply(job_train, 2, convert_count)
job_test <- apply(job_test, 2, convert_count)

jobs_signficative_terms <- rownames( jobs_classifier.anova)
#The original model had 6,324 terms. Not they have been reduced to only 61.
jobs_signficative_terms = c(jobs_signficative_terms)
job_train <- as.data.table(job_train)
job_train <- job_train[,jobs_signficative_terms[1:60],with=F]


#### Variable Reduction I ####
job_test <- get.standard.record(jobs_signficative_terms,job_test)
job_train <- get.standard.record(jobs_signficative_terms,job_train)
job_train$`NA` <- NULL
job_test$`NA` <- NULL

#Logistic 
jobs_classifier <- glm(jobs_raw_train$is_job_offer_factor  ~ .,family=binomial(link='logit'),data = (job_train))
summary(jobs_classifier)
#http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
jobs.fitted.results <- predict(jobs_classifier,newdata=(job_test),type='response')
jobs.fitted.results <- ifelse(jobs.fitted.results >= 0.5,1,0)
misClasificError <- mean(jobs.fitted.results !=jobs_raw_test$is_job_offer  )
print(paste('Accuracy',1-misClasificError))

cm <- confusionMatrix(jobs.fitted.results,jobs_raw_test$is_job_offer,positive = "1" )
cm$table
cm$byClass
predictionAccuracy <- 1-cm$table[2,1]/cm$table[2,2]
print(predictionAccuracy)

anova(jobs_classifier, test="Chisq")

#Area Under the Curve (ROC curve)
p <- predict(jobs_classifier,newdata=as.data.frame(job_test),type='response')
pr <- prediction(p, as.data.frame(jobs_raw_test$is_job_offer))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
abline(h =  auc@y.values[1],col="red")
unlist(auc@y.values[1])


df <- data.frame(unlist(prf@alpha.values))
colnames(df) <- "avalues"
df[(df$avalues>0.86) & (df$avalues<0.88),]

auc@y.values[1] 

#jobs_classifier$effects
summary(jobs_classifier)


#### Variable Reduction II ####
#http://www.stat.missouri.edu/~speckman/stat461/boost.R
library(gbm)
#all_data <- rbind(job_test,job_train)
#allResults <- rbind (jobs_raw_test,jobs_raw_train)

set.seed(12345)
fit.gbm1 <- gbm( jobs_raw_train$is_job_offer ~ ., data=job_train, 
                 dist="adaboost", n.tree = 50000,
                shrinkage = 0.001, train.fraction = 0.7)
par(ask=T)
#Perf provides the expected number of trees, but this measure is not exact
perf <- gbm.perf(fit.gbm1)
#Best performance "perf" iterations...
p <- predict(fit.gbm1, job_test, n.trees = perf)
p <- ifelse (p>=0.5,1,0)
#p <- ifelse(rescale(p)>=0.5,1,0)
cm <- confusionMatrix(p,jobs_raw_test$is_job_offer )
cm$table

#Percentage of tweets that are job advertisements.
nJobAds <- nrow(jobs_raw_test[jobs_raw_test$is_job_offer==1,])+nrow(jobs_raw_train[jobs_raw_train$is_job_offer==1,])
nJobAds/3000*100
#14.3

#GBM Rebalanced
indexj1 <- which (jobs_raw_train$is_job_offer==1)
indexj0 <- which (jobs_raw_train$is_job_offer==0)
j1 <- jobs_raw_train[indexj1]
set.seed(12345)
j0 <- jobs_raw_train[sample(indexj0,nrow(j1))]
set.seed(12345)
jobs_raw_train <- sample(rbind (j1,j0))

#Explicitly: Not using those datasets anymore..
rm(j0)
rm(j1)

job_train <- as.data.table(job_train)
j1 <- job_train[indexj1]
set.seed(12345)
j0 <- job_train[sample(indexj0,nrow(j1))]
set.seed(12345)
job_train <- as.data.frame(sample(rbind (j1,j0)))

#Explicitly: Not using those datasets anymore..
rm(j0)
rm(j1)

length(jobs_raw_train$is_job_offer)
#job_test <- get.standard.record( columnNames =  jobs_signficative_terms, dataFrame =  as.data.frame(job_test))

job_test <- get.standard.record(jobs_signficative_terms,job_test)
job_train <- get.standard.record(jobs_signficative_terms,job_train)
job_test$`NA` <- NULL
job_train$`NA` <- NULL
#Reduction is not improving here accuracy probably due to the use of singificative
#variables taken from ANOVA.
set.seed(12345)
fit.gbm1 <- gbm( jobs_raw_train$is_job_offer ~ ., data=job_train, 
                 dist="adaboost", n.tree = 50000,
                 shrinkage = 0.001, train.fraction = 0.7)
par(ask=T)
#Perf provides the expected number of trees, but this measure is not exact
perf <- gbm.perf(fit.gbm1)
#Best performance "perf" iterations...
p <- predict(fit.gbm1, job_test, n.trees = perf)
p <- ifelse (p>=0.5,1,0)
#p <- ifelse(rescale(p)>=0.5,1,0)
cm <- confusionMatrix(p,jobs_raw_test$is_job_offer )
cm$table
Reference
#Prediction   0   1
#0 525  36
#1  88 101

#Logistic #2
jobs_classifier <- glm(jobs_raw_train$is_job_offer  ~ .,
                       family=binomial(link='logit'),data = as.data.frame(job_train))

summary(jobs_classifier)
#http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
jobs.fitted.results <- predict(jobs_classifier,newdata=as.data.frame(job_test),type='response')
jobs.fitted.results <- ifelse(jobs.fitted.results > 0.5,1,0)
misClasificError <- mean(jobs.fitted.results !=jobs_raw_test$is_job_offer  )
print(paste('Accuracy',1-misClasificError))
#Too many (so far) features to perform this.
#jobs_classifier.anova <- anova(jobs_classifier, test="Chisq")

table(jobs.fitted.results,jobs_raw_test$is_job_offer)
#jobs.fitted.results   0   1
#0 549  34
#1  64 103


#Area Under the Curve (ROC curve)
p <- predict(jobs_classifier,newdata=as.data.frame(job_test),type='response')
pr <- prediction(p, as.data.frame(jobs_raw_test$is_job_offer))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
abline(h =  auc@y.values[1],col="red")
#abline(h=0.8660947,col="red")
#abline(v=0.27,col="red")
auc@y.values[1] #0.8741382 showing that TPR is almost 90% 0.77
jobs_signficative_terms <- (summary(jobs_classifier))$coefficients
table(ifelse(p>=0.5,1,0),jobs_raw_test$is_job_offer)
#0   1
#0 549  34
#1  64 103

#Random Forest test
bestmtry <- tuneRF(job_train,jobs_raw_train$is_job_offer_factor, ntreeTry=2000, 
                              stepFactor=2,improve=0.001, trace=TRUE, plot=TRUE, 
                              dobest=F)

jobs.rf  <-randomForest(jobs_raw_train$is_job_offer_factor ~ .,data=job_train, mtry=7, 
                        ntree=2000, 
                        keep.forest=TRUE, importance=TRUE,test=rbind(jobs_raw_test$is_job_offer_factor,job_test))

p <- predict(jobs.rf,job_test)
table(p,jobs_raw_test$is_job_offer )
#41/100 -> 0.41

# Can be used to combine predictors.
getCombinedConditions <- function(l){
  result <- ""
  clist <- list()
  
  for (i in seq(1,ncol(l))){
    tmp <- ""
    clist <- l[,i]
    tmp <-  paste(clist[1:length(clist)],collapse = "*")
    if (result!=""){
      tmp <- paste0(" + ",tmp)
    }
    result <- paste0(result,tmp) 
  }
  
  return (result)
}


#Creates an empty table with the specified column names.
create.empty.dynamic.table <- function (columnNames){
  m <- matrix( nrow = 0, ncol = length(columnNames))
  m <- as.data.table (m)
  colnames(m) <- columnNames
  return (m)
}
