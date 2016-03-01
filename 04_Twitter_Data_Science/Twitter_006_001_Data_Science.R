setwd("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto")
#Required libraries:
require(data.table)
require(tm)
library(wordcloud)
library(reshape)
library(slam)
library(party)
library(ROCR)

#Getting previously saved data and changing boolean values (T/F) by numeric ones (1/0):
load("./TwitterData/002_iv_rda/alljobtweets.rda")
tweets.dt <- as.data.table(tweets.dt)
tweets.dt$geo_enabled <- ifelse(tweets.dt$geo_enabled,1,0)
tweets.dt$verified <- ifelse(tweets.dt$verified,1,0)
tweets.dt$retweeted <- ifelse(tweets.dt$retweeted,1,0)
#Renaming datatable.
tweets.dt.jobs <- tweets.dt
rm(tweets.dt)

#Load manually analyzed messages (3.000 messages have been classified manually)
sample.job.messages <- as.data.table(read.csv(file="./sample_job_messages_analyzed.csv",sep = ",",header = T))
#backup
save(x=sample.job.messages,file="sample_job_messages.rda")

load("./sample_job_messages.rda")


#Adapted from: http://stackoverflow.com/questions/28248457/gsub-in-r-with-unicode-replacement-give-different-results-under-windows-compared
#This function fixes all the unicode issues in texts, replacing them by the propert UTF8 characters.
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

#Fix the loaded sample messages .
sample.job.messages$text <- sapply(sample.job.messages$text, trueunicode.hack)

#Generic function to remove non significant characters and text structures (ie links)
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

#Adapts tdm for regression models.
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  #y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
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


#Linear regression.
jobs_classifier <- lm(jobs_raw_train$is_job_offer  ~ .,data = as.data.frame(job_train))
jobs_test_pred <- predict(jobs_classifier, newdata=as.data.frame(job_test))
jobs_classifier.anova <- anova(jobs_classifier)
jobs_classifier.anova <- jobs_classifier.anova[jobs_classifier.anova$`Pr(>F)`<=0.005,]
jobs_classifier.anova <- jobs_classifier.anova[order(jobs_classifier.anova[,5]),]
jobs_classifier.anova
summary(jobs_classifier)

#Logistic regression
jobs_classifier <- glm(jobs_raw_train$is_job_offer  ~ .,family=binomial(link='logit'),data = as.data.frame(job_train))
summary(jobs_classifier)
#http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
jobs.fitted.results <- predict(jobs_classifier,newdata=as.data.frame(job_test),type='response')
jobs.fitted.results <- ifelse(jobs.fitted.results > 0.5,1,0)
misClasificError <- mean(jobs.fitted.results !=jobs_raw_test$is_job_offer  )
print(paste('Accuracy',1-misClasificError))

#Too many (so far) features to perform this.
#anova(jobs_classifier, test="Chisq")

#Area Under the Curve (ROC curve)
p <- predict(jobs_classifier,newdata=as.data.frame(job_test),type='response')
pr <- prediction(p, as.data.frame(jobs_raw_test$is_job_offer))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
#abline(h =  auc@y.values[1],col="red")
abline(h=0.822666666666667,col="red")
abline(v=0.5,col="red")
auc@y.values[1] #0.7752825 showing that TPR is almost 80% 0.77

#Tree classifier
library(party)
mod.ctree <- ctree(jobs_raw_train$is_job_offer  ~ .,   data = as.data.frame(job_train))
jobs_test_pred <- predict(mod.ctree, newdata=as.data.frame(job_train))
mod.ctree@tree
plot(mod.ctree )

#Naive Bayes classifier.
library(e1071)
jobs_classifier <- naiveBayes(job_train, factor(unlist(jobs_raw_train$is_job_offer_factor)))
jobs_test_pred <- predict(jobs_classifier, newdata=job_test)
table(jobs_test_pred, jobs_raw_test$is_job_offer_factor)
