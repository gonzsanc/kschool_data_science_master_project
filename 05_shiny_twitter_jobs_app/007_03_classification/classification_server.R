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



getFrequenceDTM <- function (corpus){
  res <- DocumentTermMatrix(corpus, control=list(wordLengths=c(3,Inf)))
  five_times_words <- findFreqTerms(res, 5)
  res <- DocumentTermMatrix(corpus, control=list(dictionary = five_times_words,wordLengths=c(3,Inf)))
  
  return (res)
}
#Returns a DTM of temr Tf-idf scores
getTfidfDTM <- function (corpus){
  res <- DocumentTermMatrix(corpus, control= list(wordLengths=c(3,Inf)))
  five_times_words <- findFreqTerms(res, 5)
  res <- DocumentTermMatrix(
    corpus,control = list(weighting = function(x) weightTfIdf(x, 
                                                              normalize = FALSE)
                          ,dictionary = five_times_words,wordLengths=c(3,Inf) ))
  return (res)
}


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

getPrediction  <- function (sparsity=0.98){
    predDataDTM <- getPredictionDataDTM  (sparsity)
    pred <- getGBMPredictionSet(predDataDTM)
    classifiedMessages <- list()
    
    freqClassification <- ifelse(pred$freq>=0.5,T,F)
    tfidfClassification <- ifelse(pred$tfidf>=0.5,T,F) 
   
    classifiedMessages$id <- tweets.messages.dt$id
    classifiedMessages$text <- tweets.messages.dt$text_original
    classifiedMessages$freqClassification <- freqClassification
    classifiedMessages$tfidfClassification <- tfidfClassification
    classifiedMessages <- as.data.frame(classifiedMessages)
    colnames(classifiedMessages) <- c("id","text","freqClassification","tfidfClassification")
    return(as.data.frame(classifiedMessages))
}


#Seguir por aquí
getPredictedJobs <- function (classifiedMessages,method="freq"){
 rtrn <- NULL
 
 if (method=="freq"){
   rtrn <- classifiedMessages[classifiedMessages$freqClassification==T,]
 }else{
   rtrn <- classifiedMessages[classifiedMessages$freqClassification==F,]
 }
 
 rtrn <- rtrn[,c("id","text")]
}

