#setwd("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto")
#Required libraries:
require(data.table)
require(tm)
require(dismo)
require(gbm)

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

getTfidffromCorpus <- function (corpus){
  #Term matrix with TF-IDF weighting
  return (DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))))
}


getCosineDistanceAmongDocsFromTdm <- function (tdm){
  #Cosine distance among documents within corpus
  require(slam)
  return(1 - crossprod_simple_triplet_matrix(tdm)/(sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2)))))
}

load ("./TwitterData/003_iii_rda/sample.jobs.messages.processed.rda")
load("./TwitterData/003_iii_rda/sample.job.messages.dtm.rda")
load("./TwitterData/003_iii_rda/sample.job.messages.corpus.rda")
#load ("./jobs_classifier.anova.rda")
#load("./jobs_signficative_terms.rda")
#load("./job_train.tc5.lr005.rda ")

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


job_train <- as.data.frame(as.matrix(job_train))
job_test <- as.data.frame(as.matrix(job_test))

#colnames(job_train)

#ANOVA analysis to reduce dimensionality through coefficients matrix.
fit.lm <- lm(jobs_raw_train$is_job_offer ~ . ,data=job_train)
summary(fit.lm)
#Residual standard error: 0.2305 on 1576 degrees of freedom
#Multiple R-squared:  0.6704,	Adjusted R-squared:  0.5297 
#F-statistic: 4.764 on 673 and 1576 DF,  p-value: < 2.2e-16


p <- predict(fit.lm,job_test)
table(ifelse(p>=0.5,1,0),jobs_raw_test$is_job_offer)
#0   1
#0 588  67
#1  25  70

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

fit.lm.coefficients$terms
job_train.f <- cbind(jobs_raw_train$is_job_offer,job_train)
job_test.f  <- cbind(jobs_raw_test$is_job_offer,job_test)
 
#head(job_train.f)


#==================================================================
#"Boosted Regression Trees for Ecological Modelling"-like processs
#==================================================================

# The improvement in the learning rate is determined by learning.rate = 0.005
# Setting it to learning.rate = 0.005 will produce a more complex model.
#Model re-optimisation using only significant temrs.
job_train.tc5.lr005 <- gbm.step(
  data=job_train.f, gbm.x = 2:ncol(job_train.f), gbm.y = 1,
  family = "bernoulli", tree.complexity = 5,
  learning.rate = 0.005, bag.fraction = 0.5)

preds <- predict.gbm(job_train.tc5.lr005, job_test.f,n.trees=5600, type="response")
table(ifelse(preds>0.5,1,0),jobs_raw_test$is_job_offer )


summary(job_train.tc5.lr005)
#save(job_train.tc5.lr005,file="job_train.tc5.lr005.rda")
#load("job_train.tc5.lr005.rda")

#Extraction of significative terms to the gbm cross-validation optimisation. 
s <- summary(job_train.tc5.lr005)
rownames(s) <- NULL
s <- s[s$rel.inf>0,]
fit.gbm.coefficients <- s
s <- NULL

job_train <- get.standard.record(
  columnNames = fit.gbm.coefficients$var
  ,job_train
)

job_test <- get.standard.record(
  columnNames = fit.gbm.coefficients$var
  ,job_test
)

job_train.f <- cbind(jobs_raw_train$is_job_offer,job_train)

#Redoing the model
job_train.tc8.lr003 <- gbm.step(
  data=job_train.f, gbm.x = 2:ncol(job_train.f), gbm.y = 1,
  family = "bernoulli", tree.complexity = 8,
  learning.rate = 0.003, bag.fraction = 0.5)

#save (job_train.tc8.lr003,file="job_train.tc8.lr003.rda")

preds <- predict.gbm(job_train.tc5.lr003, job_test.f,n.trees=7100, type="response")
table(ifelse(preds>0.5,1,0),jobs_raw_test$is_job_offer )

variablesToDrop <- 5
job_train.simplified <- gbm.simplify(job_train.tc8.lr003, n.drops = variablesToDrop)
#save (job_train.simplified,file="job_train.simplified.52.rda")
#load("job_train.simplified.52.rda")

#Two drops:
job_train.simplified.52$final.drops
summary(job_train.simplified.52)


job_test <- get.standard.record(columnNames = colnames(job_train),job_test)

#There is not much difference between l.rate = 0.001 and lr 0.1... 
job_train.simplified.gbm <- gbm.step(
  data=job_train.f, gbm.x = job_train.simplified.52$pred.list[[1]] , gbm.y = 1,
  family = "bernoulli", tree.complexity = 5,
  learning.rate = 0.1, bag.fraction = 0.5)

save (job_train.simplified.gbm,file="job_train.simplified.gbm.52.rda")
load("job_train.simplified.gbm.52.rda")

summary (job_train.simplified.gbm)
preds <- predict.gbm(job_train.simplified.gbm, job_test.f,n.trees=1200, type="response")
table(ifelse(preds>0.7,1,0),jobs_raw_test$is_job_offer )


#TFIDF
#ANOVA analysis to reduce dimensionality through coefficients matrix.
#Job_train reset:
job_train.tfidf  <- getTfidffromCorpus(jobs_corpus_train)
job_train.tfidf.df <- as.data.frame(as.matrix(job_train.tfidf))

job_test.tfidf  <- getTfidffromCorpus(jobs_corpus_test)
job_test.tfidf.df <- as.data.frame(as.matrix(job_test.tfidf))

fit.lm <- lm(jobs_raw_train$is_job_offer ~ . ,data=job_train.tfidf.df)
p <- predict(fit.lm,job_test)
table(ifelse(p>=0.5,1,0),jobs_raw_test$is_job_offer)
s <- anova(fit.lm)
summary(s)

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

fit.lm.coefficients$terms

#Adjusting job_train to the definition
job_train.tfidf.df <- get.standard.record(fit.lm.coefficients$terms,job_train.tfidf.df)

#Test set must have the same columns as the training set.
job_test.tfidf.df <-  get.standard.record(fit.lm.coefficients$terms,job_test.tfidf.df)

job_train.f <- cbind(jobs_raw_train$is_job_offer,job_train.tfidf.df)
job_test.f <- cbind(jobs_raw_test$is_job_offer,job_test.tfidf.df)

#Examine range over 1-1.000 trees.
job_train.tc5.lr005 <- gbm.step(
  data=job_train.f, gbm.x = 2:ncol(job_train.f), gbm.y = 1,
  family = "bernoulli", tree.complexity = 5,
  learning.rate = 0.1, bag.fraction = 0.5)

preds <- predict.gbm(job_train.tc5.lr005, job_test.f,n.trees=550, type="response")

table(ifelse(preds>0.5,1,0),jobs_raw_test$is_job_offer )
#Not so good.
#0   1
#0 599  85
#1  14  52
colnames(job_test.tfidf.df)
#Ops - jobs is missing!!.



#REBALANCING
#Twice no jobs than jobs:
RebalancingFactor = 0.01 #ratio -> jobs/nojobs

#Which messages for training are jobs offers and which are not.
indexj1 <- which (jobs_raw_train$is_job_offer==1)
indexj0 <- which (jobs_raw_train$is_job_offer==0)
j1 <- jobs_raw_train[indexj1]

#Number of rows to get:
RowsToInclude <- min(nrow(jobs_raw_train),nrow(j1)/RebalancingFactor)
set.seed(12345)
if (RowsToInclude>=nrow(jobs_raw_train)){
  j0 <- jobs_raw_train
}else{
  j0 <- jobs_raw_train[sample(indexj0, RowsToInclude)]
}



set.seed(12345)
#Tweet and other Twitter objects collection:
job_train.tfidf <- sample(rbind (j1,j0))

#Explicitly: Not using those datasets anymore..
rm(j0)
rm(j1)

#document term  matrix
#Now an additional DTM
job_train.tfidf.corpus  <- Corpus(VectorSource(job_train.tfidf$textProcessed))
job_train.tfidf.dtm <- getTfidffromCorpus(job_train.tfidf.corpus)
job_train.tfidf.dtm.df <- as.data.frame(as.matrix(job_train.tfidf.dtm))
job_train.tfidf.dtm.df <- as.data.table(job_train.tfidf.dtm.df)
job_train.tfidf.dtm.nonsparse  <- removeSparseTerms(job_train.tfidf.dtm,0.98)
job_train.tfidf.dtm.nonsparse.dt <- as.data.table(as.data.frame(as.matrix(job_train.tfidf.dtm.nonsparse)))

job_test.tfidf.corpus  <- Corpus(VectorSource(jobs_raw_test$textProcessed))
job_test.tfidf.dtm <- getTfidffromCorpus(job_test.tfidf.corpus)
job_test.tfidf.dtm.df <- as.data.frame(as.matrix(job_test.tfidf.dtm))
job_test.tfidf.dtm.df <- as.data.table(job_test.tfidf.dtm.df)
job_test.tfidf.dtm.df <- get.standard.record(colnames(job_train.tfidf.dtm.df),job_test.tfidf.dtm.df)
job_test.tfidf.dtm.nonsparse.dt <- get.standard.record(colnames(job_train.tfidf.dtm.nonsparse.dt),job_test.tfidf.dtm.df)


fit.lm <- lm(job_train.tfidf$is_job_offer ~ . ,data=job_train.tfidf.dtm.nonsparse.dt)
p <- predict(fit.lm,job_test.tfidf.dtm.nonsparse.dt)
table(ifelse(p>=0.5,1,0),jobs_raw_test$is_job_offer)
#0.6
#0   1
#0 539  37
#1  74 100

#1
#0   1
#0 519  26
#1  94 111

#0.01 / remo sparse 0.99
#0   1
#0 594  58
#1  19  79
#0.2405063
#Unbalanced data still provides better results.

#0.01 / remo sparse 0.98
#0   1
#0 604  77
#1   9  60
#Sparsity removal offers more balanced results.
