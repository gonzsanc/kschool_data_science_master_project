library(topicmodels)
library(slam)
library(corrplot)

#library(GGally)

tblPCACorrelationMatrix <- data.frame()
tblPCATopicsAbsoluteContribution <- data.frame()
tblPCATopicsRelativeContribution <- data.frame()
tblPCACumulativeContribution <- data.frame()
tblPCAProp <- data.frame()
PCA.number.of.topics <-0
LDA.model <- 0
LDA.model.alpha <- 0
LDA.model.entropy <- 0
tfidf.dtm <- 0
PCA.model <- 0



getTfIDF <- function(dtm){
  tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
  return (tfidf)
}


GenLDATopicsAnalysis <- function (corpus,numberOfTopics=0){ 
  #PART 1: DOCUMENT TERM MATRIX
  #Following same steps as described on:
  #https://www.jstatsoft.org/article/view/v040i13
  
  dtm <- DocumentTermMatrix(corpus, 
                     control = list(minWordLength = 3, removePunctuation = TRUE))
  
  #summary(col_sums(dtm))
  
  #term_tfidf <- GenTfidffromCorpus(corpus) 
  term_tfidf <- getTfIDF (dtm)
    summary(term_tfidf)
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #0.01643 0.85910 0.95510 1.07700 1.19400 4.77500 
  dim (term_tfidf)
  
  cutoff <- median(term_tfidf)*.9
  #A bit below median to ensure very frequent terms are omitted
  dtm <- dtm[, term_tfidf >= cutoff]
  dtm <- dtm[row_sums(dtm) > 0,]
  #summary(col_sums(dtm))
  
  tfidf.dtm <<- dtm

  #PART II LDA
  #https://www.jstatsoft.org/article/view/v040i13
  #Model
  number.topics <- 0 
  ifelse(numberOfTopics==0, 
         number.topics <- max(as.integer(dim(dtm)[1]/25),3),
         number.topics<- numberOfTopics
  )
  
  k <- number.topics
  SEED <- 12345
  
  TM <- list(
    VEM = LDA(dtm, k = k, control = list(seed = SEED)),
    VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE,
     seed = SEED)),
    Gibbs = LDA(dtm, k = k, method = "Gibbs", control = list(
      seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
    CTM = CTM(dtm, k = k, control = list(seed = SEED,
    var = list(tol = 10^-4), em = list(tol = 10^-3))))
  
  LDA.model <<- TM
  
  #We see that if α is estimated it is set to a value much smaller than the 
  #default. This indicates that in this case the Dirichlet distribution has 
  #more mass at the corners and hence, documents consist only of few topics.
  VEM.alpha <- sapply(TM[1:2], slot, "alpha")
  #VEM   VEM_fixed 
  #0.006992151 1.666666667 
  LDA.model.alpha <<- VEM.alpha
  
  #Entropy measure: Higher values indicate that the topic distributions are more evenly spread
  #over the topics
  TM.entropy <- sapply(TM, function(x) mean(apply(posterior(x)$topics,1, function(z) - sum(z * log(z)))))
  LDA.model.entropy <<- TM.entropy
  Topic <- topics(TM[["Gibbs"]], 1)
  
  Terms <- terms(TM[["Gibbs"]], 5)
  Terms[, 1:2]
  
  return (TM)  
}


getLDADataTable <- function (corpus,numberOfTopics=5,method="Gibbs",recalculate=T){
  
  #method = "Gibbs"
  #corpus <- messages.corpus[1:100]
  #numberOfTopics <- 5
  if(is.numeric(LDA.model)) recalculate<-T

  if (recalculate) TM <- GenLDATopicsAnalysis (corpus,numberOfTopics)
  if (!recalculate) TM <- LDA.model
  
  tweets  <- tweets.messages.dt
  
  if (method=="VEM") TM <- TM$VEM
  if (method=="VEM_fixed") TM <- TM$VEM_fixed
  if (method=="Gibbs") TM <- TM$Gibbs
  if (method=="CTM") TM <- TM$CTM
  
  size <- TM@Dim[1] * TM@Dim[2]
  rows <- max(5,5*(round(TM@Dim[2] / max(TM@Dim[1],00000.1),0) + 1))
  lda.terms <- terms(TM,size) 
  
  lda.terms.melted <- melt(lda.terms[1:rows,1:numberOfTopics])
  lda.terms.melted <- as.list(lda.terms.melted)
  lda.terms.melted$X1 <- NULL
#  colnames(lda.terms.melted) <- c ("Topic","Term")
  
  rtrn <- as.data.frame(lda.terms.melted)
  rtrn[[1]] <- NULL
  return (as.data.table(rtrn))
}



#Having topics, let's get a matrix with each topic and the 
#TM$VEM@k
#TM$VEM@wordassignments$i
#Get topics iof ratings...
#lda.model <-GenLDATopicsAnalysis (corpus = corpus,numberOfTopics = numberOfTopics)

#====================
#Messages with Topics
#====================

#SEGUIR POR AQUI.
genMessagesWithTopics <- function(method=""){
  if (is.numeric(LDA.model)){return (NULL)}
  df <- data.frame(melt(topics(LDA.model$VEM,1)))
  df$docId <- as.numeric(row.names(df))
  colnames(df) <- c("Topic","Tweet")
  df$LDAMethod <- ""
  sbset <- tweets.messages.dt[df$Tweet,c("id","text_original","screenName","created"),with=F]
  sbset$text_original <- gsub("[\r\n]"," ",sbset$text_original)
  sbset$text_original <- gsub("  "," ",sbset$text_original)

  df <- df[,c("Tweet","Topic","LDAMethod")]
  rtrn <- as.data.table(cbind (df,sbset))
  rtrn$LDAMethod <- method
  return(rtrn)
  }

#==================================================================
#  PCA ANALYSIS
#==================================================================

genTopicsCorrPCAObjects <- function(corpus, minFreq, numberOfTopics){
  
  #corpus <- messages.corpus
  #minFreq <- sqrt(length(corpus))
  #numberOfTopics <- 6

  PCA.number.of.topics <<- numberOfTopics
  variance.to.explain <- 0.99
  
  tdm <- getTDMbyFreq(corpus,minFreq)
  tdm <- as.data.frame(as.matrix(tdm))
  tdm <- as.data.frame(t(tdm))
  tdm <- tdm + 0.00000000001
  tdm <- log(tdm)
  tdm.cor <- cor(tdm)
  
  tdm.pca <- prcomp(tdm.cor)
  PCA.model <<- tdm.pca 
  tdm.pca.contribution.groups <- round(100*(tdm.pca$sdev)^2 / sum(tdm.pca$sdev^2),6)

  tblPCACumulativeContribution <<- cumsum((tdm.pca$sdev)^2 / sum(tdm.pca$sdev^2))
  tdm.pca.contribution.groups <- as.data.table(t(tdm.pca.contribution.groups))
  
  tdm.terms <- colnames(tdm) 
  colnames(tdm.pca.contribution.groups) <- colnames(tdm.pca$x)

  #vars <- apply(tdm.pca$x, 2, var)  
  #props <- vars / sum(vars)
  #tblPCAProp <<- props
  
  #tdm.pca.cumulative.contribution <- cumsum(props)
  
  #tblPCATopicsAbsoluteContribution <<- colSums(abs(tdm.pca$x))
  #tblPCATopicsAbsoluteContribution/tblPCATotalContribution
  pca.range.to.get <- 
    length(tblPCACumulativeContribution[tblPCACumulativeContribution<variance.to.explain])+ 1
  
  pca.range.to.get <- min(pca.range.to.get,10)
  df.pca.rel <- tdm.pca.contribution.groups[,1:pca.range.to.get,with=F]

  #extrae los pesos de cada componente.

  tblPCATopicsRelativeContribution <<- as.data.frame(df.pca.rel)
  
  #row.names (tblPCATopicsRelativeContribution ) <- unlist(tdm.terms)[1:length(df.pca.rel)]
  
    barplot(as.matrix(df.pca.rel), las=2,main="Corpus PCA"
            ,names.arg=colnames(tdm.pca.contribution.groups)[1:length(df.pca.rel)]
            ,col="tomato",ylab = "Variance Explained",xlab="Groups"
            ,ylim=c(0,100)
            )
    lines(x = tblPCACumulativeContribution*100,type = "both")
 
}


getPCACorrMatrix <- function(corpus, minFreq, numberOfTopics){
  
  #corpus <- messages.corpus
  #minFreq <- sqrt(length(corpus))
  #numberOfTopics <- 6
  PCA.number.of.topics <<- numberOfTopics
  variance.to.explain <- 0.99
  
  tdm <- getTDMbyFreq(corpus,minFreq)
  tdm <- as.data.frame(as.matrix(tdm))
  tdm <- as.data.frame(t(tdm))
  tdm <- tdm + 0.00000000001
  tdm <- log(tdm)
  tdm.cor <- cor(tdm)
  tblPCACorrelationMatrix <<-tdm.cor
  tblPCACorrelationMatrix

  numwords <- length(tblPCACorrelationMatrix[,1])
  if(numwords<100){
    
    #ggcorr(tblPCACorrelationMatrix, hjust = 0.75, size = 2.5, color = "grey50", layout.exp = 1)
    
    
    corrplot(tblPCACorrelationMatrix,
             method = "circle",tl.col = "orangered3",tl.cex = min(20/numwords,12),
             mar=c(0,0,0,0))
    
  }else{
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.7, paste0("Too many terms.\n Please increase term frequency"), 
         cex = 1.6, col = "black")
  }
}




genCorrplotOld <- function (){
    
  numwords <- length(tblPCACorrelationMatrix[,1])
  if(numwords<100){
    
    #ggcorr(tblPCACorrelationMatrix, hjust = 0.75, size = 2.5, color = "grey50", layout.exp = 1)
    
    layout(matrix(c(1,2),1, 2, byrow = TRUE),
           widths=c(12,5), heights=c(11),respect=T)
    
    corrplot(tblPCACorrelationMatrix,
          method = "circle",tl.col = "orangered3",tl.cex = 20/numwords,
          mar=c(1,1,4,1))

  plot  (tblPCAProp[1:PCA.number.of.topics],main="PCA Variance \n per topic",type="b")
    
    
    }else{

    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.7, paste0("Too many terms.\n Please increase term frequency"), 
         cex = 1.6, col = "black")
  }
}

