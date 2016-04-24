source(file = "./007_03_classification/data_preparation/DataPreparationCorpus.R")
require(data.table)
require(tm)

#Generates the training data
#For tdf and frequentist tdm
#Logarithmic and raw
#Centered.
genDataPreparationDataSets <- function (){
  load("./007_03_classification/data_objects/borutaFeatures.rda")
  load("./007_03_classification/data_objects/sample.job.messages.corpus.rda")
  load("./007_03_classification/data_objects/sample.jobs.messages.processed.rda")

  trainingIndexes <- sample.job.messages[,sample.job.messages$is_for_training==1]
  testingIndexes <- sample.job.messages[,sample.job.messages$is_for_training==0]
  
  dtm <- list()
  dtm$freq <- getFrequenceDTM(sample.job.messages.corpus) 
  dtm$freq <- removeSparseTerms(dtm$freq,0.99) %>% as.matrix () %>% as.data.frame()
  dtm$tfidf <- getTfidfDTM (sample.job.messages.corpus) 
  dtm$tfidf <- removeSparseTerms(dtm$tfidf,0.99) %>% as.matrix () %>% as.data.frame()
  
  
  dtm$freq <- get.standard.record(borutaFeatures$freqFeatures,as.data.frame(dtm$freq))
  dtm$tfidf <- get.standard.record(borutaFeatures$tfIdfFeatures,as.data.frame(dtm$tfidf))
  dtm$freqLog <- apply(dtm$freq,c(1,2),function(x) log(x+1e-28))
  dtm$tfidfLog <- apply(dtm$tfidf,c(1,2),function(x) log(x+1e-28))
  
  dtm$freq <- scale(dtm$freq,center = T,scale = F) %>% as.data.frame()
  dtm$tfidf <- scale(dtm$tfidf,center = T,scale = F) %>% as.data.frame()
  dtm$freqLog <- scale(dtm$freqLog,center = T,scale = F) %>% as.data.frame()
  dtm$tfidfLog  <- scale(dtm$tfidfLog,center = T,scale = F) %>% as.data.frame()
  dtm$freq <-  cbind(sample.job.messages$is_job_offer,dtm$freq)
  dtm$tfidf <-  cbind(sample.job.messages$is_job_offer,dtm$tfidf)
  dtm$freqLog <-  cbind(sample.job.messages$is_job_offer,dtm$freqLog)
  dtm$tfidfLog <-  cbind(sample.job.messages$is_job_offer,dtm$tfidfLog)
  
  return (dtm)
  }

dtm <- genDataPreparationDataSets  ()

dtm$freq.train <- dtm$freq[sample.job.messages$is_for_training==1,]
dtm$freq.test <- dtm$freq[sample.job.messages$is_for_training==0,]

dtm$tfidf.train <- dtm$tfidf[sample.job.messages$is_for_training==1,]
dtm$tfidf.test <- dtm$ftfidf[sample.job.messages$is_for_training==0,]

dtm$freqLog.train <- dtm$freqLog[sample.job.messages$is_for_training==1,]
dtm$freqLog.test <- dtm$freqLog[sample.job.messages$is_for_training==0,]

dtm$tfidfLog.train <- dtm$tfidfLog[sample.job.messages$is_for_training==1,]
dtm$tfidfLog.test <- dtm$tfidfLog[sample.job.messages$is_for_training==0,]




library(dismo)

model.gbm <- list()

model.gbm$freq <- gbm.step(
  data=dtm$freq.train, gbm.x = 2:ncol(dtm$freq.train), gbm.y = 1,
  family = "bernoulli", tree.complexity = 8,
  learning.rate = 0.002, bag.fraction = 0.5)

model.gbm$tfidf <- gbm.step(
  data=dtm$tfidf.train, gbm.x = 2:ncol(dtm$tfidf.train), gbm.y = 1,
  family = "bernoulli", tree.complexity = 8,
  learning.rate = 0.002, bag.fraction = 0.5)

model.gbm$freqLog <- gbm.step(
  data=dtm$freqLog.train , gbm.x = 2:ncol(dtm$freqLog.train), gbm.y = 1,
  family = "bernoulli", tree.complexity = 8,
  learning.rate = 0.002, bag.fraction = 0.5)


model.gbm$tfidfLog <- gbm.step(
  data=dtm$tfidfLog.train , gbm.x = 2:ncol(dtm$tfidfLog.train), gbm.y = 1,
  family = "bernoulli", tree.complexity = 8,
  learning.rate = 0.002, bag.fraction = 0.5)

save(model.gbm,file = "./007_03_classification/data_objects/model.gbm.rda")
variablesToDrop <- 5


model.gbm$freq.simplified <- gbm.simplify(model.gbm$freq, n.drops = variablesToDrop)
model.gbm$tfidf.simplified <- gbm.simplify(model.gbm$tfidf, n.drops = variablesToDrop)
model.gbm$freqLog.simplified <- gbm.simplify(model.gbm$freqLog, n.drops = variablesToDrop)
model.gbm$tfidfLog.simplified <- gbm.simplify(model.gbm$tfidfLog, n.drops = variablesToDrop)
save(model.gbm,file = "./007_03_classification/data_objects/2_model.gbm.rda")

job_train.simplified <- gbm.simplify(dtm$freq.train, n.drops = variablesToDrop)



summary(model.gbm$tfidf)


