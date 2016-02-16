## LOAD LIBRARIES ##
require(data.table)
require (tm)
require(qdapRegex)

## GET DATA ##

#Declare data.table variable
tweets.dt <- data.table()

#Local path where the rda files containing the tweets were stored.
#Those tweets were stored as data.tables/data.frames in a variable called "tmp"
filePath <- "./TwitterData/001_rda/"

#Get list with all the rda files at the location.
files <- list.files(filePath ,pattern = ".rda")


#All the previously parsed tweets are loaded into a single variable (tweets.dt)
#This loop assumes the tweet data was and is stored in the "tmp" variable.
#It may be important deleting the "tmp" variable after each loop to optimize the resources usage.
for (f in files){
  load(paste0(filePath,f))
  tweets.dt <- rbind(tweets.dt,tmp)
  rm(tmp)
}

#After loading the data it is a good idea saving it in a single file for later use.
#save(tweets.dt,file="allthetweets.rda", compress = T)
#load("./allthetweets.rda")


#If needed, the sample size may be reduced, i,e 1/3rd of its original size for better performance:
set.seed(123)
tweets.dt <- data.table(tweets.dt[sample(nrow(tweets.dt), nrow(tweets.dt)/3), ])


## PREPARE THE DATA ##
# Remove all special characters and mentions, leave hashtags.
tweets.dt.sample <- tweets.dt
tweets.dt.sample$text <- tolower(tweets.dt.sample$text)
tweets.dt.sample$text <- gsub(x=tweets.dt.sample$text,pattern = "^rt ",replacement = "" )
tweets.dt.sample$text <- gsub(x=tweets.dt.sample$text,pattern = "@\\S+",replacement = "" )
tweets.dt.sample$text <- gsub(x=tweets.dt.sample$text,pattern = "https?\\S+",replacement = "" )
tweets.dt.sample$text <- gsub(x=tweets.dt.sample$text,pattern = "\\bh\\b",replacement = " " )
tweets.dt.sample$text <- gsub(x=tweets.dt.sample$text,pattern = "[^[:alpha:] ]",replacement = " " )
tweets.dt.sample$text <- rm_white(tweets.dt.sample$text )

#Although stop word removal is usually done with tm package, data.table offers more performance.
#Besides the stopwods("en") method returns 174 words and for this study we want to extend that list.
#I am using the "long" list provided here: http://www.ranks.nl/stopwords.
stopwords.dt <- fread(input = "../stopwords_english.txt",encoding = "UTF-8",header = F)
stopwords.dt$V1 <- lapply(stopwords.dt$V1, function (x) paste0("\\b",x,"\\b"))
pattern.stopwords <- paste(stopwords.dt$V1, collapse = '|')

#Release memory
rm(stopwords.dt)
rm(tweets.dt)

#Custom fuction to remove stopwords from large data sets by chunks.
#This may take a few minutes to complete a sample of 400.000 tweets removing 600+ stopwords.
removeStopWords <- function (dtObject,chunksize=5e4){
  counter <- 1
  n <- as.numeric(nrow(dtObject))
  tmp <- data.table()
  print (paste("steps:",ceiling(n/chunksize)))
  
  for (i in seq(1, n, by=chunksize)){
    
    print (paste(count,i,min(c(i+chunksize-1,n)),sep =":"))
    
    from <- i
    to <- min(c(i+chunksize-1,n))
    #gets a portion of the dataset and replaces all the stop words on it.
    aux <- as.data.table(dtObject[from:to,])
    aux$text <- (gsub(x=aux$text,pattern = pattern.stopwords, replacement = ""))
    
    #appends the resulting dataset onto tmp and releases memory
    tmp <- rbind(tmp,aux)
    rm(aux)
    
    #counter...
    count <- count +1
  }
  
  return (tmp)
}

tweets.dt.sample <- removeStopWords(tweets.dt.sample)
tweets.dt.sample$text <- as.data.table(rm_white(tweets.dt.sample$text))
rm(pattern.stopwords)


#backup:
#save(tweets.dt.sample,file ="tweets.dt.sample.rda" )
load("./stream_001/tweets.dt.sample.rda")

#The tweets vector is converted into a corpus set containing all the messages.
cp <- Corpus(VectorSource(tweets.dt.sample$text))
rm(tweets.dt.sample)
#Stemming requires a lot of memory. It will improve the results if enabled but a powerful device is required.
#cp <- tm_map(cp,stemDocument,language="english")

#A common representation of the words is the term document matrix.
#In this case words of one character length are omitted.
cp.dtm <- TermDocumentMatrix(cp,control=list(minWordLength=2))

#backup
#save(cp,file="cp.tweets.dt.sample.rda",compress = T)
#save(cp.dtm,file ="cp.dtm.tweets.dt.sample.rda",compress=T)

#Print in stdout those terms that are repeated at least 5000 times across all the documents on the corups .
findFreqTerms(cp.dtm,5000)
