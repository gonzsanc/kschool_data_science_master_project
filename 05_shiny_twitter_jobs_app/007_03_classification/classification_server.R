if (!require(tm)) require(tm)
#Creates an object containing the configuration and 
#a placeholder 
predictTextClassification <- function(input) {
  rtrn <- list()
  rtrn$scoring <- input$scoring
  rtrn$selectScale <- input$selectScale
  rtrn$sldSensibility <- input$sldSensibility
  rtrn$simplify <- input$simplify
  rtrn$prediction <- NaN
  return(rtrn)
}


#Returns Document term matrix which scores are term frequencies across documents.
getFrequenceDTM <- function (corpus){
  res <- DocumentTermMatrix(corpus, control=list(wordLengths=c(3,Inf)))
  five_times_words <- findFreqTerms(res, 5)
  res <- DocumentTermMatrix(corpus, control=list(dictionary = five_times_words,wordLengths=c(3,Inf)))
  
  return (res)
}


#Returns a DTM of term Tf-idf scores
getTfidfDTM <- function (corpus){
  res <- DocumentTermMatrix(corpus, control= list(wordLengths=c(3,Inf)))
  five_times_words <- findFreqTerms(res, 5)
  res <- DocumentTermMatrix(
    corpus,control = list(weighting = function(x) weightTfIdf(x, 
                                                              normalize = FALSE)
                          ,dictionary = five_times_words,wordLengths=c(3,Inf) ))
  return (res)
}


#Generates a standard table with fixed column names and number.
#If the dataframe does not contain any of the standard columnames, they are created.
#Any existing column in the dataframe not matching the column names is dropped off.
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

gmb.predictions <- list()

#Generates the precalculated models that will be used to predict values.
getPredictionDataDTM <- function (sparsity=0.98){
  fp <- FactoryPredictor
  predDataDTM <- list()
  
  predDataDTM$dtm.freq <- removeSparseTerms(getFrequenceDTM(messages.corpus),sparsity) 
  predDataDTM$dtm.tfidf <- removeSparseTerms(getTfidfDTM(messages.corpus),sparsity) 
  predDataDTM$dtm.freq  <- predDataDTM$dtm.freq %>% as.matrix()  %>% as.data.frame()
  predDataDTM$dtm.tfidf <- predDataDTM$dtm.tfidf %>% as.matrix()  %>% as.data.frame()
  predDataDTM$dtm.freq <- scale(predDataDTM$dtm.freq,scale = F,center = T)
  predDataDTM$dtm.tfidf <- scale(predDataDTM$dtm.tfidf,scale = F,center = T)
  
  predDataDTM$dtm.freq <- get.standard.record(
    columnNames = fp$makeGBMPredictor$predictors$freqFeatures
    ,dataFrame = predDataDTM$dtm.freq
  )
  predDataDTM$dtm.tfidf <- get.standard.record(
    columnNames = fp$makeGBMPredictor$predictors$tfIdfFeatures
    ,dataFrame = predDataDTM$dtm.tfidf
  )
  
  return(predDataDTM)
}

#Returns all the prediction models for the dataset.
getGBMPredictionSet <- function (dataSet){
  models <- FactoryPredictor$makeGBMPredictor$model
  pred <- list()
  
  model <- models$freq
  data <- dataSet$dtm.freq
  pred$freq <- as.data.frame(predict.gbm(model,data,n.trees = model$n.tree,type="response"))
  
  model <- models$tfidf
  data <- dataSet$dtm.tfidf
  pred$tfidf <- as.data.frame(predict.gbm(model,data,n.trees = model$n.tree,type="response"))
  
  return(pred)  
}

#Returns a table containing the messages and their classification as jobs or otherwise.
getPrediction  <- function (sparsity=0.98,sensibility=0.5){
    predDataDTM <- getPredictionDataDTM  (sparsity)
    pred <- getGBMPredictionSet(predDataDTM)
    classifiedMessages <- list()
    
    freqScores <- round(100*pred$freq,2)
    tfidfScores <- round(100*pred$tfidf,2)
    freqClassification <- ifelse(pred$freq>=sensibility,T,F)
    tfidfClassification <- ifelse(pred$tfidf>=sensibility,T,F) 
   
    classifiedMessages$id <- tweets.messages.dt$id
    classifiedMessages$text <- tweets.messages.dt$text_original
    classifiedMessages$freqClassification <- freqClassification
    classifiedMessages$tfidfClassification <- tfidfClassification
    classifiedMessages$freqScores <- freqScores
    classifiedMessages$tfidfScores <- tfidfScores
    classifiedMessages <- as.data.frame(classifiedMessages)
    colnames(classifiedMessages) <- 
      c(
        "id","text","freqClassification","tfidfClassification","freqScores","tfidfScores"
        )
    return(as.data.frame(classifiedMessages))
}

#Returns a table with only those messages classified as job offers using the previously
#built models.

getPredictedJobs <- function (classifiedMessages,method="tfi"){
  rtrn <- NULL 

 if (method=="frq"){
   rtrn <- classifiedMessages[classifiedMessages$freqClassification==T,]
   rtrn <- rtrn[,c("id","text","freqScores")]
   }else{
   rtrn <- classifiedMessages[classifiedMessages$tfidfClassification==T,]
   rtrn <- rtrn[,c("id","text","tfidfScores")]  
   }

  colnames(rtrn)<-c("Tweet ID","Tweet Message","Prediction Score (%)")
return(rtrn)
 }

