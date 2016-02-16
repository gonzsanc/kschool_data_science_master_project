setwd("C:/Descargas/Onedrive/KSCHOOL/Proyecto")

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