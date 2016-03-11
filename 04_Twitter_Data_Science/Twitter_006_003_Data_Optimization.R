#setwd("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto")
#Required libraries:
require(data.table)
require(tm)
library(ROCR)
library(caret)
library(randomForest)

#Converts a dataframe having the specified column names
#The function removes all the columns not included in the columnames list
#The function preserves all the data from the columns which names match the ones in the columnNames list.
get.standard.record <- function(columnNames,dataFrame){
  
  dataFrame <- as.data.frame(dataFrame)
  #print (dataFrame)
  
  #Inserts in the dataframe the columns in the list that are missing.
  cols <- !(columnNames %in% colnames(dataFrame))
  if (length(cols)>0)
    cols <- columnNames[cols]
  for(i in cols) {
    #print (i)
    dataFrame[,i] <- 0
  }
  
  #Removes all the columns of the dataframe not in the columnNames list.
  cols <- !(colnames(dataFrame) %in% columnNames)
  if(length(cols)>0){
    for(i in names(dataFrame)[cols]) {
      dataFrame[,i] <- NULL
    }
  }
  
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
load("./jobs_signficative_terms.rda")

#Separating jobs offers from other messages and creating the required text mining objects.
jobs_indices <- which(sample.job.messages$is_job_offer_factor == 1)
noise_indices <- which(sample.job.messages$is_job_offer_factor == 0)
training_indices <- which(sample.job.messages$is_for_training == 1)
testing_indices <- which(sample.job.messages$is_for_training == 0)
jobs_raw_train <- sample.job.messages[training_indices]
jobs_raw_test <- sample.job.messages[testing_indices]
jobs_corpus_train <- sample.job.messages.corpus[training_indices]
jobs_corpus_test <- sample.job.messages.corpus[testing_indices]
jobs_dtm_train <- sample.job.messages.dtm[training_indices,]
jobs_dtm_test <- sample.job.messages.dtm[testing_indices,]

#Removing infrequent words.
five_times_words <- findFreqTerms(jobs_dtm_train, 5)
job_train.dtm <- DocumentTermMatrix(jobs_corpus_train, control=list(dictionary = five_times_words,wordLengths=c(3,Inf)))
job_train <- job_train.dtm
job_test <- DocumentTermMatrix(jobs_corpus_test, control=list(dictionary = five_times_words,wordLengths=c(3,Inf)))
job_offers_index <- which (jobs_raw_train$is_job_offer==1)
not_job_offers_index <- which (jobs_raw_train$is_job_offer==0)

#Commented but left. Rebalancing the sample gave worst results than leaving it unmodified.
#REBALANCING
#RebalancingFactor = 0.5 #ratio -> jobs/nojobs
#indexj1 <- which (jobs_raw_train$is_job_offer==1)
#indexj0 <- which (jobs_raw_train$is_job_offer==0)

#j1 <- jobs_raw_train[indexj1]

#set.seed(12345)
#RowsToInclude <- min(nrow(jobs_raw_train),nrow(j1)/RebalancingFactor)
#j0 <- jobs_raw_train[sample(indexj0,RowsToInclude)]
#set.seed(12345)
#jobs_raw_train <- sample(rbind (j1,j0))#

#Explicitly: Not using those datasets anymore..
#rm(j0)
#rm(j1)


#j1 <- sample.job.messages.dtm[indexj1,]
#set.seed(12345)
#j0 <- sample.job.messages.dtm[sample(indexj0,RowsToInclude),]
#job_train <- do.call(tm:::c.DocumentTermMatrix, list(j0,j1) )
#set.seed(12345)
#job_train <-job_train [sample(c(seq(1,job_train$nrow))),]

#Explicitly: Not using those datasets anymore..
#rm(j0)
#rm(j1)

#job_train  <- removeSparseTerms(job_train,0.995)

#Converting text document matrix into dataframes and to the standard record format.
job_train <- as.data.frame(as.matrix(job_train))
job_test <- as.data.frame(as.matrix(job_test))

colnames(job_train)

#ANOVA analysis to reduce dimensionality through coefficients matrix.
fit.lm <- lm(jobs_raw_train$is_job_offer ~ . ,data=job_train)
p <- predict(fit.lm,job_test)
table(ifelse(p>=0.5,1,0),jobs_raw_test$is_job_offer)

s <- anova(fit.lm)

#Gets significant columns
fit.lm.coefficients <- as.data.frame(s)
colnames(fit.lm.coefficients) <- c("df","sumsq","meansq","fvalue","pvalue")
fit.lm.coefficients$terms <- rownames(s)
fit.lm.coefficients <-   fit.lm.coefficients[rownames(fit.lm.coefficients)!="Residuals",] 
rownames (fit.lm.coefficients) <- NULL
fit.lm.coefficients <- as.data.table(fit.lm.coefficients)
sum.coef <- sum(fit.lm.coefficients$sumsq)
#Leaving only statistically significant coefficients and calculating acumulative variability.
fit.lm.coefficients <- fit.lm.coefficients[fit.lm.coefficients$pvalue<=0.05,]
fit.lm.coefficients <- fit.lm.coefficients[order(-fit.lm.coefficients$sumsq)]
fit.lm.coefficients$sumsqRel <- fit.lm.coefficients$sumsq/sum.coef
fit.lm.coefficients$sumsqRelacum <- cumsum(fit.lm.coefficients$sumsqRel) 

#Adjusting job_train to the definition
job_train <- get.standard.record(fit.lm.coefficients$terms,job_train)
#Test set must have the same columns as the training set.
job_test <- get.standard.record(dataFrame = job_test,columnNames = colnames(job_train))

#Importance through logistic regression
fit <- glm(jobs_raw_train$is_job_offer ~ . -1,family=binomial(link='logit'),data=job_train)
summary(fit)
save (fit,file="glm.fit.rda")

p <- predict(fit,job_test)
table(ifelse(p>=0.5,1,0),jobs_raw_test$is_job_offer)

head(fit.lm.coefficients,25)
set.seed(12345)
fit.gbm3 <- gbm( jobs_raw_train$is_job_offer ~ ., data=job_train, 
                 dist="adaboost", n.tree = 50000,
                 shrinkage = 0.001, train.fraction = 0.7)

#Associations among terms
findAssocs(job_train.dtm,head(fit.lm.coefficients$terms,n=5),0.15)
#$appli
#hire click 
#0.32  0.24 

#$alert
#london       need technician    project 
#0.18       0.18       0.17       0.15 

#$job
#jobsearch    london 
#0.25      0.17 

#$administr
#assist   system  univers opportun   corpor    organ 
#0.28     0.19     0.19     0.17     0.16     0.16 

#$recruit
#limit develop 
#0.17    0.16 
#

#GRADIENT BOOSTING MACHING - CONDITIONAL VARIABLES
fit.gbm2 <- gbm( jobs_raw_train$is_job_offer ~  
                   #gouping most influencing variables among them.
                   appli*alert + appli*job + appli*recruit + appli*opportun+
                   appli*vacanc + appli*administr+appli*develop
                   +alert*job + alert*recruit + alert*opportun+alert*vacanc+
                   alert*administr+alert*develop+
                  job*recruit + job*opportun + job*vacanc+job*hire+job*administr+
                   job*develop+job*advisor +  job*assist+job*applic
                 +job*hire+
                  #grouping most influencing variables among their asssociated words
                   appli*hire + 
                   alert*need + alert*technician  +
                   jobsearch + job*jobsearch + 
                   administr*assist  + administr*opportun 
                   + administr*corpor  
                   +recruit*limit + recruit*develop
                   
                                  , data=job_train, 
                 dist="adaboost", n.tree = 50000,
                 shrinkage = 0.001, train.fraction = 0.7)

fit.gbm1 <- fit.gbm2
par(ask=T)
#Perf provides the expected number of trees, but this measure is not exact
perf <- gbm.perf(fit.gbm1)
#Best performance "perf" iterations...
p <- predict(fit.gbm1, job_test, n.trees = perf)
p <- ifelse (p>=0.5,1,0)
#p <- ifelse(rescale(p)>=0.5,1,0)
cm <- confusionMatrix(p,jobs_raw_test$is_job_offer )
cm$table

# Confusion matrix
#Prediction   0   1
#0 606  85
#1   7  52


head(fit.lm.coefficients,n=5)
#df     sumsq    meansq   fvalue       pvalue     terms   sumsqRel sumsqRelacum
#1:  1 18.941598 18.941598 356.4587 7.733461e-72     appli 0.11118637    0.1111864
#2:  1 14.465773 14.465773 272.2289 1.542334e-56     alert 0.08491347    0.1960998
#3:  1 12.754544 12.754544 240.0256 1.686247e-50       job 0.07486863    0.2709685
#4:  1  6.096285  6.096285 114.7250 6.861211e-26 administr 0.03578493    0.3067534
#5:  1  5.719075  5.719075 107.6263 1.945347e-24   recruit 0.03357073    0.3403241


#Random Forest test
#Find optimal mtry...
bestmtry <- tuneRF(job_train,jobs_raw_train$is_job_offer_factor, ntreeTry=2000, 
                   stepFactor=2,improve=0.001, trace=TRUE, plot=TRUE, 
                   dobest=F)

#Random Forest model.
jobs.rf  <-randomForest(jobs_raw_train$is_job_offer_factor ~ .,data=job_train, mtry=20, 
                        ntree=2000, 
                        keep.forest=TRUE, importance=TRUE,test=rbind(jobs_raw_test$is_job_offer_factor,job_test))

p <- predict(jobs.rf,job_test)
table(p,jobs_raw_test$is_job_offer )
#Confusion matrix
#p     0   1
#0 599  66
#1  14  71 <- 80%

#job_train  <- removeSparseTerms(job_train,0.99)
#Confusion matrix if sparsity filtering is applied through tm package.
#Reference
#Prediction   0   1
#0 610 103
#1   3  34


#random forest with conditional factors (no sparsity filtering)

bestmtry <- tuneRF(job_train,jobs_raw_train$is_job_offer_factor, ntreeTry=2000, 
                   stepFactor=2,improve=0.001, trace=TRUE, plot=TRUE, 
                   dobest=F)
bestmtry

jobs.rf  <-randomForest(jobs_raw_train$is_job_offer_factor ~ 
                          #gouping most influencing variables among them.
                          appli*alert + appli*job + appli*recruit + appli*opportun+
                          appli*vacanc + appli*administr+appli*develop
                        +alert*job + alert*recruit + alert*opportun+alert*vacanc+
                          alert*administr+alert*develop+
                          job*recruit + job*opportun + job*vacanc+job*hire+job*administr+
                          job*develop+job*advisor +  job*assist+job*applic
                        +job*hire+
                          #grouping most influencing variables among their asssociated words
                          appli*hire + 
                          alert*need + alert*technician  +
                          jobsearch + job*jobsearch + 
                          administr*assist  + administr*opportun 
                        + administr*corpor  
                        +recruit*limit + recruit*develop
                        ,data=job_train, mtry=20, 
                        ntree=2000, 
                        keep.forest=TRUE, importance=TRUE,test=rbind(jobs_raw_test$is_job_offer_factor,job_test))

p <- predict(jobs.rf,job_test)
table(p,jobs_raw_test$is_job_offer )
#Random forest confusion matrix
#p     0   1
#0 598  70
#1  15  67 <- 78%

#PCA is not giving good results.
p <- prcomp(cbind(jobs_raw_train$is_job_offer, job_train),scale. = T)
p
summary (p)
screeplot(p, type="lines",col=3)


#Logistic regression.
jobs_classifier <- glm(jobs_raw_train$is_job_offer  ~ .,
                       family=binomial(link='logit'),data = as.data.frame(job_train))

summary(jobs_classifier)
#http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
jobs.fitted.results <- predict(jobs_classifier,newdata=as.data.frame(job_test),type='response')
jobs.fitted.results <- ifelse(jobs.fitted.results > 0.5,1,0)
misClasificError <- mean(jobs.fitted.results !=jobs_raw_test$is_job_offer  )
print(paste('Accuracy',1-misClasificError))
#Another anova analysis
jobs_classifier.anova <- anova(jobs_classifier, test="Chisq")

table(jobs.fitted.results,jobs_raw_test$is_job_offer)
#jobs.fitted.results   0   1
#0 587  62
#1  26  75 <- 1/3

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

