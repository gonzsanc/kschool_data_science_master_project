tdm.pca <- 0

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
  #load("007_03_classification/data_objects/sample.job.messages.corpus.rda")
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

getSignificativeTerms <- function (dtm,variability=0.99,sparsity=0.99){
  #  load("007_03_classification/data_objects/sample.job.messages.corpus.rda")
  # dtm <- getTfidfDTM(sample.job.messages.corpus)
  dtm <- removeSparseTerms(dtm, sparsity)
  
  dtm.cor <- (as.matrix(dtm))
  dtm.cor <- as.data.frame(t(dtm.cor))
  
  #http://michael.hahsler.net/SMU/CSE7337/install/tm.R
  #Normalization for euclidean distances.
  norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
  dtm.cor <- norm_eucl(dtm.cor)
  norm_centered <- scale(x=dtm.cor,scale = F,center = T)
  dtm.cor <- dtm.cor + 1e-16
  dtm.cor <- log(dtm.cor)
  tdm.cor <- cor(dtm.cor)
  tdm.pca <<- prcomp(dtm)
  
  #Individual contribution per component.
  tdm.pca.contribution.groups <- round(100*(tdm.pca$sdev)^2 / sum(tdm.pca$sdev^2),6)
  tdm.pca.contribution.groups <- as.data.table(t(tdm.pca.contribution.groups))
  
  #Cumulativo contribution per component.
  tblPCACumulativeContribution <- cumsum((tdm.pca$sdev)^2 / sum(tdm.pca$sdev^2))
  
  #List of terms.
  tdm.terms <- colnames(dtm) 
  
  #Compoent ID as column name
  colnames(tdm.pca.contribution.groups) <- colnames(tdm.pca$x)
  
  pca.range.to.get <- 
    length(tblPCACumulativeContribution[tblPCACumulativeContribution<variability])+ 1
  
  #pca.range.to.get <- min(pca.range.to.get,10) <-- for visualization purposes only.
  df.pca.rel <- tdm.pca.contribution.groups[,1:pca.range.to.get,with=F]
  df.pca.rel <- df.pca.rel/1e2
  #extrae los pesos de cada componente.
  
  
  tblPCATopicsRelativeContribution <- as.data.frame(df.pca.rel)

  
  return(tdm.pca)
  
}




objCorpora <- getTestTrainCorpora()
dtm <- getTfidfDTM(corpus = objCorpora$jobOffersCorpus)
pca <- getSignificativeTerms(dtm = dtm,variability = 0.95,sparsity = 0.97)

summary(pca)
plot(pca)

sum(round(100*(pca$sdev)^2 / sum(pca$sdev^2),6))



