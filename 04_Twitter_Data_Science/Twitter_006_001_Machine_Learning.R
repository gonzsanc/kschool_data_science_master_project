#setwd("C:/Descargas/Onedrive/KSCHOOL/Proyecto")

#setwd("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto")## LOAD LIBRARIES ##

#Required libraries:
require(data.table)
require(tm)
library(wordcloud)
library(reshape)
library(slam)
#require(doSNOW)
#cl <- makeCluster(2, type="SOCK")
#registerDoSNOW(cl)
library(party)
#library(randomForest)
#require(ggplot2)
# The multiplot function has been copied from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/

#Getting previously saved data and changing boolean values by numeric ones:

load("./TwitterData/002_iv_rda/alljobtweets.rda")
tweets.dt <- as.data.table(tweets.dt)
tweets.dt$geo_enabled <- ifelse(tweets.dt$geo_enabled,1,0)
tweets.dt$verified <- ifelse(tweets.dt$verified,1,0)
tweets.dt$retweeted <- ifelse(tweets.dt$retweeted,1,0)


tweets.dt.jobs <- tweets.dt
rm(tweets.dt)

sample.job.messages <- as.data.table(read.csv(file="./sample_job_messages_analyzed.csv",sep = ",",header = T))
save(x=sample.job.messages,file="sample_job_messages.rda")


load("./sample_job_messages.rda")


#Adapted from: http://stackoverflow.com/questions/28248457/gsub-in-r-with-unicode-replacement-give-different-results-under-windows-compared
trueunicode.hack <- function(string){
       
      string <- as.character(string)
       m <- gregexpr("<U\\+[0-9A-F]{4}>", string)
       if(-1==m[[1]][1])
       return(string)
       
       codes <- unlist(regmatches(string, m))
       replacements <- codes
       N <- length(codes)
       for(i in 1:N){
       replacements[i] <- intToUtf8(strtoi(paste0("0x", substring(codes[i], 4, 7))))
       }
       
       # if the string doesn't start with a unicode, the copy its initial part
       # until first occurrence of unicode
       if(1!=m[[1]][1]){
       y <- substring(string, 1, m[[1]][1]-1)
       y <- paste0(y, replacements[1])
       }else{
       y <- replacements[1]
       }
       
       # if more than 1 unicodes in the string
       if(1<N){
       for(i in 2:N){
       s <- gsub("<U\\+[0-9A-F]{4}>", replacements[i], 
       substring(string, m[[1]][i-1]+8, m[[1]][i]+7))
       Encoding(s) <- "UTF-8"
       y <- paste0(y, s)
       }
       }
       
       # get the trailing contents, if any
       if( nchar(string)>(m[[1]][N]+8) )
       y <- paste0( y, substring(string, m[[1]][N]+8, nchar(string)) )
       y
  }

sample.job.messages$text <- sapply(sample.job.messages$text, trueunicode.hack)


cleanText <- function(x){
  tmp <- as.character(x)
  tmp <- tolower(tmp)
  tmp <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", tmp)
  tmp <- gsub("&amp", " ", tmp)
  tmp <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tmp)
  tmp <- gsub("@\\w+", "", tmp)
  tmp <- gsub("[[:punct:]]|…|—", " ", tmp)
  #tmp <- gsub("([#])|[[:punct:]]|…|—", "\\1", tmp)
  tmp <- gsub("[[:digit:]]", " ", tmp)
  tmp <- gsub("http\\w+", " ", tmp)
  tmp <- gsub("[ \t]{2,}", " ", tmp)
  tmp <- gsub("^\\s+|\\s+$", " ", tmp)
  tmp <- gsub("\\b#\\b", " ", tmp)
  tmp <- gsub("^\\s+|\\s+$", " ", tmp)
  
  #tmp <- gsub ("('s‘|’","",tmp)
 x <- tmp
 return (x)
}

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}


#============= END FUNCTIONS

sample.job.messages$text <- sapply(as.list(sample.job.messages$text), cleanText)
stopwords.dt <- unlist(fread("./TwitterData/002_iii_rda_and_stopwords/stopwords_english.txt"))

sample.job.messages.corpus <- Corpus(VectorSource(sample.job.messages$text))
sample.job.messages.corpus <- tm_map(sample.job.messages.corpus, removeWords, stopwords.dt)
sample.job.messages.corpus <- tm_map(sample.job.messages.corpus, stemDocument)
sample.job.messages.corpus <- tm_map(sample.job.messages.corpus, removeWords, c("\\b#\\b"))
sample.job.messages.corpus <- tm_map(sample.job.messages.corpus, stripWhitespace)   

sample.job.messages$textProcessed <- unlist(sapply(sample.job.messages.corpus, `[`, "content"))
sample.job.messages$textProcessed <- as.character(sample.job.messages$textProcessed)
sample.job.messages$is_job_offer_factor <- as.factor(sample.job.messages$is_job_offer) 

head(sample.job.messages,n=1L)

set.seed(12345)
sample.job.messages$is_for_training <- sample(as.factor(c(rep((1),2250),rep((0),750))),size = 3000)

sample.job.messages.dtm <- DocumentTermMatrix(sample.job.messages.corpus,
                                              control=list(wordLengths=c(3,Inf)))

#https://www3.nd.edu/~steve/computing_with_data/20_text_mining/text_mining_example.html#/15
jobs_indices <- which(sample.job.messages$is_job_offer_factor == 1)
noise_indices <- which(sample.job.messages$is_job_offer_factor == 0)
training_indices <- which(sample.job.messages$is_for_training == 1)
testing_indices <- which(sample.job.messages$is_for_training == 0)

#http://www.r-bloggers.com/word-cloud-in-r/

require(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(sample.job.messages.corpus[jobs_indices], scale=c(11,.2),min.freq=4,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

wordcloud(sample.job.messages.corpus[noise_indices], scale=c(8,.2),min.freq=12,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)



old.par <- par(mfrow=c(1, 2))
> plot(faithful, main="Faithful eruptions")
> plot(large.islands, main="Islands", ylab="Area")
> par(old.par)



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

#Naive Bayes classifier.
library(e1071)
jobs_classifier <- naiveBayes(job_train, factor(unlist(jobs_raw_train$is_job_offer_factor)))
jobs_test_pred <- predict(jobs_classifier, newdata=job_test)

table(jobs_test_pred, jobs_raw_test$is_job_offer_factor)



#Tree classifier
mod.ctree <- ctree(jobs_raw_train$is_job_offer_factor  ~ .,   data = as.data.frame(job_train))
plot(mod.ctree)

# ¿Cuántos errores cometemos?
reales <- as.data.frame(job_train)
pred.ctree <- predict(mod.ctree)


# varias maneras de ver el error
sum(reales != pred.ctree)
mean(reales != pred.ctree)
table(reales, pred.ctree)

plot(mod.ctree)



#SBM classifier
library(gbm)      # gbm
library (caret)

train.control <- trainControl(method = "cv", number = 10)
grid <- expand.grid(n.trees = 1000 * 1,#1:4,
                    interaction.depth = 2 * (1:4),
                    shrinkage = 0.001,
                    n.minobsinnode = 10)

#siempre 10 para minobsinnode
#shirnkage <- 0.001
#grid es una malla con todas las posibilidades que se van a usar para entrenar
#el modelo gbm siguiente:
model.gbm <- train( as.character(factor(unlist(jobs_raw_train$is_job_offer_factor))) ~ ., data = job_train_pc$scores, 
                   trControl = train.control, 
                   tuneGrid = grid,
                   method = "gbm")

job_train_pc <- princomp(sapply(X = as.data.table(job_train),FUN = function(x) ifelse(x=="No",0,1)))
plot(job_train_pc)
summary(job_train_pc)

sapply(X = job_train,FUN = function(x) ifelse(x=="No",0,1))



# summarize results
print(model.gbm)
#Preproceso es escalado, etc.
#validacion cruzada con 10 bloques (folds)
#Accuracy SD -> desviación estándar del error.
#El hecho de que el error está en el extremo de la rejilla quiere decir que igual
#podría mejorar.

plot(model.gbm)
#Los colores muestran la profundidad de los árboles.

table(bank.csv.test$y, predict(model.gbm, bank.csv.test))

model.gbm.variableImportance <- as.data.table(summary(model.gbm))


model.gbm.variableImportance[,relativeImportance:= rel.inf/sum(rel.inf)]

setkey(model.gbm.variableImportance,relativeImportance)
model.gbm.variableImportance <- model.gbm.variableImportance[order(-relativeImportance)]
model.gbm.variableImportance[,relativeImportaneAcum := round(cumsum(relativeImportance),2)] 
model.gbm.selected.features <- list(model.gbm.variableImportance[relativeImportaneAcum<.99,]$var)
model.gbm.selected.features <- unlist(model.gbm.selected.features)
model.gbm.selected.features <- as.character(model.gbm.selected.features)

model.gbm.short <- model.gbm[,model.gbm.selected.features]







jobs_dtm_test.frequencies <-  (sort(colSums(as.matrix(jobs_dtm_test)), decreasing=TRUE) )
jobs_dtm_train.frequencies <- (sort(colSums(as.matrix(jobs_dtm_train)), decreasing=TRUE))

jobs_dtm_test.frequencies$classColumn = rep(0,length(jobs_dtm_test.frequencies))
jobs_dtm_test.frequencies <- melt(jobs_dtm_test.frequencies,id.vars="classColumn")
jobs_dtm_train.frequencies$classColumn = rep(0,length(jobs_dtm_train.frequencies))
jobs_dtm_train.frequencies <- melt(jobs_dtm_train.frequencies,id.vars="classColumn")

head(jobs_dtm_test.frequencies,n = 1L)


summary(unique(jobs_dtm_train.frequencies$value))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    5.75   11.50   21.08   20.25  153.00 
jobs_dtm_train.frequencies[jobs_dtm_train.frequencies$value>20,]

rf.tdm <- as.matrix(jobs_dtm_train)

rf.tdm <- as.data.table(data.frame(rf.tdm))
rf.tdm[,is_job_offer_factor:=jobs_raw_train$is_job_offer_factor]


set.seed(415)
tuned <- tune.svm(V2~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))
