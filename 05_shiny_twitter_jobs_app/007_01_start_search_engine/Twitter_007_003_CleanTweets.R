#setwd("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto")
#Required libraries:
require(data.table)
require(tm)

messages.corpus <- NULL
tweets.messages.dt <- NULL

tweets.messages.dtm <- NULL
messages.tfidf.dtm <- NULL
messages.tfidf.dtm.nonsparse <- NULL


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


#============= END FUNCTIONS


GenCorpus <- function (sampleDF){
  
  tweets.df <- sampleDF
  
  tweets.dt <- as.data.table(tweets.df)
  tweets.messages.dt <<- tweets.dt
  rm(tweets.dt)
  
  #Fix the loaded sample messages .
  tweets.messages.dt$text_original <<- tweets.messages.dt$text
  tweets.messages.dt <<- unique(tweets.messages.dt)
  tweets.messages.dt$text <<- sapply(tweets.messages.dt$text, function(x) gettext (x))
  tweets.messages.dt$text <<- iconv(tweets.messages.dt$text , "UTF-8", "ASCII")
  tweets.messages.dt$text <<- gsub("[^[:alnum:]///' ]", "",tweets.messages.dt$text)
  tweets.messages.dt <<- tweets.messages.dt[!is.na(tweets.messages.dt$text),]
 
   tweets.messages.dt$text <<- sapply(tweets.messages.dt$text, trueunicode.hack)
  tweets.messages.dt$text <<- sapply(as.list(tweets.messages.dt$text), cleanText)
  
  stopwords.dt <- unlist(fread("./stopwords_english.txt"))
  
  messages.corpus <<- Corpus(VectorSource(tweets.messages.dt$text))
  messages.corpus <<- tm_map(messages.corpus,
            content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
            mc.cores=1)
  
 
  messages.corpus <<- tm_map(messages.corpus, removeWords, stopwords.dt)
  
  messages.corpus <<- tm_map(messages.corpus, stemDocument)
  messages.corpus <<- tm_map(messages.corpus, removeWords, c("\\b#\\b","tco"))
  messages.corpus <<- tm_map(messages.corpus, stripWhitespace)   
  #tweets.messages.dt$text <<- unlist(sapply(messages.corpus, `[`, "content"))

  messages.corpus <<- tm_map(messages.corpus, stemDocument) 
  
  
  a<- list()
  for (i in seq_along(messages.corpus)) {
    a[i] <- gettext( messages.corpus[[i]][[1]])
  }
  
  tweets.messages.dt$text <<- unlist(a) 
  messages.corpus <<- Corpus(VectorSource(tweets.messages.dt$text))
  
  } 


GenTfidffromCorpus <- function (corpus, sparsity = 0.999999999999,wL=c(3,Inf)){
  #Term matrix with TF-IDF weighting
  tmp <- (DocumentTermMatrix(corpus,control = list(wordLengths=wL,weighting = function(x) weightTfIdf(x, normalize = FALSE))))
  tmp.nonsparse  <- removeSparseTerms(tmp,sparsity)
  tmp <- as.data.frame(as.matrix(tmp))
  
  messages.tfidf.dtm <<- as.data.table(tmp)
  messages.tfidf.dtm.nonsparse <<- as.data.table(as.data.frame(as.matrix(tmp.nonsparse)))
  
}


GenDTMfromCorpus <- function (corpus, wL=c(3,Inf)){
  
  tmp <- DocumentTermMatrix(corpus,
                           control=list(wordLengths=wL))
  tmp <- as.data.frame(as.matrix(tmp))
  tweets.messages.dtm <<- as.data.table(tmp)
}


GenAllTMObjects <- function(sampleDF){
  GenCorpus(sampleDF)
  GenDTMfromCorpus(messages.corpus)
  GenTfidffromCorpus(messages.corpus)
}

#RUN THIS ONE:
#GenAllTMObjects (sampleDF)
