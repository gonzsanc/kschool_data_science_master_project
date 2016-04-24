#setwd("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto/proyecto-fin-master")
#library(FactoMineR)
# require(data.table)

library(Boruta)
library(tm)
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


#12.4Mb Object storing text and corpora for testing and tranining.
getTestTrainCorpora <- function (){
  load ("007_03_classification/data_objects/sample.job.messages.corpus.rda")
  load ("007_03_classification/data_objects/sample.jobs.messages.processed.rda")
  jobs_indices <- which(sample.job.messages$is_job_offer_factor == 1)
  noise_indices <- which(sample.job.messages$is_job_offer_factor == 0)
  training_indices <- which(sample.job.messages$is_for_training == 1)
  testing_indices <- which(sample.job.messages$is_for_training == 0)
  jobs_raw_train <- sample.job.messages[training_indices,]
  jobs_raw_test <- sample.job.messages[testing_indices,]
  jobs_corpus_train <- sample.job.messages.corpus[training_indices]
  jobs_corpus_test <- sample.job.messages.corpus[testing_indices]
  jobs_corpus <- sample.job.messages.corpus[jobs_indices] 
    
  result <- list (
    rawTrainMessages = jobs_raw_train
    ,rawTestMessages = jobs_raw_test
    ,messageTrainCorpus = jobs_corpus_train
    ,messageTestCorpus = jobs_corpus_test
    ,jobOffersCorpus = jobs_corpus 
  )
  
  rm(sample.job.messages)
  rm(sample.job.messages.corpus)
  return (result)
}

#corpus <- sample.job.messages.corpus

#### Document term matrices... #######
#Returns a DTM of term frequencies
getFrequenceDTM <- function (corpus){
  load("007_03_classification/data_objects/sample.job.messages.dtm.rda")
  res <- DocumentTermMatrix(corpus, control=list(wordLengths=c(3,Inf)))
  five_times_words <- findFreqTerms(res, 5)
  res <- DocumentTermMatrix(corpus, control=list(dictionary = five_times_words,wordLengths=c(3,Inf)))
  rm (sample.job.messages.dtm)
  return (res)
}
#Returns a DTM of temr Tf-idf scores
getTfidfDTM <- function (corpus){
  #  load("007_03_classification/data_objects/sample.job.messages.dtm.rda")
  # load("007_03_classification/data_objects/sample.job.messages.corpus.rda")
  #corpus <- getTestTrainCorpora()
  #  res <- DocumentTermMatrix(corpus$messageTrainCorpus, control= list(wordLengths=c(3,Inf)))
  # corpus <- corpus$messageTrainCorpus

  load("007_03_classification/data_objects/sample.job.messages.dtm.rda")
  res <- DocumentTermMatrix(corpus, control= list(wordLengths=c(3,Inf)))
  five_times_words <- findFreqTerms(res, 5)
  res <- DocumentTermMatrix(
      corpus,control = list(weighting = function(x) weightTfIdf(x, 
                            normalize = FALSE)
                            ,dictionary = five_times_words,wordLengths=c(3,Inf) ))
    
    rm (sample.job.messages.dtm)
    return (res)
  
}



#Returns a logarightmic tdm scaled.
getLogDTM <- function (dtm){
  return (1)
}

getCorrelationDTM <- function (dtm){
     return (1)
}

getDTMToMatrix <-function (dtm){
  return(as.data.frame(as.matrix(dtm)))
}


#########  PCA #######################
#Extracts significative terms by PCA reduction over term document matrix.

getSignificativeTerms <- function (dtm,sparsity=0.99,method="tfidf"){
  #  load("007_03_classification/data_objects/sample.job.messages.corpus.rda")
  # load ("007_03_classification/data_objects/sample.jobs.messages.processed.rda")
  # dtm <- getTfidfDTM(sample.job.messages.corpus)
    dtm <- getFrequenceDTM(corpus = sample.job.messages.corpus)

  dtm <- removeSparseTerms(dtm, sparsity)
  
  dtm <- (as.matrix(dtm))
  dtm <- as.data.frame((dtm))
  
  #http://michael.hahsler.net/SMU/CSE7337/install/tm.R
  #Normalization for euclidean distances.
  norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
  #dtm <- norm_eucl(dtm)
  dtm <- scale(x=dtm,scale = F,center = T)
  #dtm <- dtm + 1e-16
 
  dtm.dt <-data.table(dtm)
  dtm.dt$is_job_offer <- sample.job.messages$is_job_offer
  
  vars.to.test <- colnames(dtm.dt)
  
  sample.df <- dtm.dt[,c(vars.to.test),with=F]
  sample.df <- as.data.table(sample.df)
  idx <- seq(1,nrow(sample.df),1)
  idx <- sample(idx,size = 0.7*nrow(sample.df))
  #idx <- createDataPartition(sample.df$is_job_offer,p=0.01,list=FALSE)
  sample.df <- sample.df[idx,]
  
  boruta_output <- Boruta(is_job_offer ~ ., data=na.omit(sample.df), doTrace=2) 
  boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
  print(boruta_signif)  # significant variables
#tfidf
 # [1] "account"    "alert"      "appli"      "bank"       "care"       "cook"       "design"    
  # [8] "develop"    "doctor"     "driver"     "engin"      "good"       "hire"       "job"       
  #[15] "journalist" "love"       "manag"      "market"     "model"      "nurs"       "peopl"     
  #[22] "sale"       "student"    "work"  

  tfIdfFeatures <- c('account','alert','appli','bank','care','cook','design','develop','doctor','driver','engin','good','hire','job','journalist','love','manag','market','model','nurs','peopl','sale','student','work')
#Freq:
  #[1] "account"    "alert"      "appli"      "bank"       "busi"       "care"       "cook"      
  #[8] "design"     "develop"    "doctor"     "driver"     "engin"      "good"       "hire"      
  #[15] "job"        "journalist" "love"       "manag"      "model"      "nurs"       "open"      
  #[22] "sale"       "student"    "teacher"    "today"      "work"

  freqFeatures<- c('account','alert','appli','bank','busi','care','cook','design','develop','doctor','driver','engin','good','hire','job','journalist','love','manag','model','nurs','open','sale','student','teacher','today','work')
  
  borutaFeatures <- list()
  borutaFeatures$tfIdfFeatures <-tfIdfFeatures
  borutaFeatures$freqFeatures <- freqFeatures

  save(file="./007_03_classification/data_objects/borutaFeatures.rda",borutaFeatures)
  
  plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot 
  
}
