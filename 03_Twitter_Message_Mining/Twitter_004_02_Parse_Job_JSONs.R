#This script extracts data from streamR's JSON files and saves them as data.table files.
#The resulting files are easy to load on R.

#streamR to parse JSON tweets into data.frame
library (streamR)
#data.table converts to a more convenient and fast object type the data.fames.
library(data.table)

#1st step: copy the JSON files in a folder, i.e. c:\downloads\
filePath <- "C:/Downloads/"
#Get a list with each file on that folder (not recursively) containing the "json" string.
files <- list.files(filePath, pattern = "json")

#With the second datafile (500MB+) I could not work with a 4GB machine. I divide the file in chunks:
#install.packages("NCmisc")
#require (NCmisc)
#file.split(fn = paste0(filePath,"NAME_OF_FILE"),same.dir = T,size = 70000, suf = "part")

#A counter to assign as a suffix to each output rda file generated.
i <- 1
#Loop over the json files: 
for (f in files){
  #Adding path to file name
  f <- paste0(filePath,f)
  
  #Simple log displayed on Console pane. 
  cat(paste0("Processing ", f))
  
  #parse the json tweet files and convert it into a data.table object
  #This process takes time to complete...
  tmp <- data.table(
    # simplify = FALSE: we want geographic location included.
    parseTweets( f, simplify = FALSE)
  )
  
  #Preliminary data cleansing:
  #Duplicates removal.
  tmp <- unique(tmp)
  #Non-UTF8 messages removal.
  tmp$text <- iconv(tmp$text , "UTF-8", "UTF-8")
  #Empty messages removal.
  tmp <- tmp[!is.na(tmp$text),]
  
  #save data.table converted tweet set object as rda file. 
  save(tmp,file=paste0("image_",as.character(i),".rda"), compress = T)
  
  i=i+1
  #Release back memory resources.
  rm(tmp)
}

## Removing incomplete tweets ##

#The files created were stored in a subfolder: "./TwitterData/003_rda/"
#Those tweets were stored as data.tables/data.frames in a variable called "tmp"
filePath <- "./TwitterData/003_rda/"

#Get list with all the rda files at the location.
files <- list.files(filePath ,pattern = "job")

#Declare data.table variable
tweets.dt <- data.table()

#All the previously parsed tweets are loaded into a single variable (tweets.dt)
#This loop assumes the tweet data was and is stored in the "tmp" variable.
#It may be important deleting the "tmp" variable after each loop to optimize the resources usage.
for (f in files){
  load(paste0(filePath,f))
  tweets.dt <- rbind(tweets.dt,tmp)
  rm(tmp)
}

#After loading the data it is a good idea saving it in a single file for later use.
#save(tweets.dt,file="alljobtweets.rda", compress = T)
#load("./TwitterData/004_rda/alljobtweets.rda")
tweets.dt<-data.table(tweets.dt)

#Sometimes Twitter delivers incomplete data, not very often, which counts are negative.
#The reason is that partical data is taking too long to be fetched.
# find explanation here -> https://dev.twitter.com/streaming/overview/processing#Missing_counts
head(sort(unique(tweets.dt[tweets.dt$friends_count<0,]$friends_count,decreasing = F)))
#[1] -704  -15   -6   -3

#The number of missing items is low.
nrow(tweets.dt[tweets.dt$friends_count<0,])
#[1] 23

#Removing incomplete tweets:
tweets.dt <- tweets.dt[tweets.dt$friends_count>=0,]

#backup:
save(tweets.dt,file="alljobtweets.rda", compress = T)
