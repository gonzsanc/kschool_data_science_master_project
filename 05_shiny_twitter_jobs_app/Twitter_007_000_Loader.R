
#Twitter search function
if(!exists("getTweetsDataFrame", mode="function")) 
  source("./007_01_start_search_engine/Twitter_007_001_SearchTwitter.R")

#TM Objects function
if(!exists("cleanText", mode="function")) 
  source("./007_01_start_search_engine/Twitter_007_003_CleanTweets.R")

#Summary Pane Contents
if(!exists("genTweetTabTable", mode="function")) 
  source("./007_01_start_search_engine/Twitter_007_004_Search_Jobs_TabPanel.R")


#Summary Pane Contents
if(!exists("corpusPanel", mode="function")) 
  source("./007_02_corpus/Twitter_0007_05_corpus_main.R")


getRandomColors <- function (len = 1,colorMix = c(64,64,64)){
  
  #debug   len=1
  #debug   colorMix  <- c(32,32,32)
  
  colors <- c()
  l <- len
  set.seed(12345)
  
  while(l>0){
    l <- l-1
    red <- round(runif(1, 0, 255),0)
    green <- round(runif(1, 0, 255),0)
    blue <- round(runif(1, 0, 255),0)
    
    red <- round( (red+colorMix[1])/2 ,0)
    green <- round( (green+colorMix[2])/2 ,0)
    blue <- round( (blue+colorMix[3])/2 ,0)
    
    red <-  format(as.hexmode(red),2,F)
    green <-  format(as.hexmode(green),2,F)
    blue <-  format(as.hexmode(blue),2,F)
    
    color <- paste0("#",red,blue,green)
    colors <- c(colors,color)
    
  }
  return (colors)
}
