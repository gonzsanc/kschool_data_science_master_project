#global variables
LinkBefore <- '<a href="'
linkMiddle <- '" target="_blank">'
linkAfter <- '</a>'

#Extracts first Twitter Shortlink from message
extractShortLinks <- function (messageList){
  
  pattern <-"(https?://(bit\\.ly|t\\.co|lnkd\\.in|tcrn\\.ch)\\S*)\\b"
  tmp <- NULL
  tmp <- str_extract(messageList,pattern)
  tmp <- unlist(lapply(tmp,function (x) ifelse(is.na(x),"N/A",
                                        paste0(LinkBefore,x,linkMiddle,"link",linkAfter)) ))
  return(tmp)
}


#Gets link to the tweet.
getTweetLink <- function(screenName,id,text=NULL){
    
  link <- paste0("https://twitter.com/",screenName,"/status/",id)
  ifelse(is.null(text) ,text<-link,FALSE)
  link <- paste0(LinkBefore,link,linkMiddle,text,linkAfter)
  return (link)
}

#Returns link to the twitter user page.
getTwitterUserLink <- function(screenName,text=NULL){
  
  link <- paste0("https://twitter.com/",screenName)
  ifelse(is.null(text) ,text<-link,FALSE)
  link <- paste0(LinkBefore,link,linkMiddle,text,linkAfter)
  return (link)
}

#Generates the table to be displayed on the search pannel. 
genTweetTabTable <- function(tweets.messages.dt){

  summary.dt <- as.data.table(tweets.messages.dt)
  
  setkey(summary.dt,id)
  summary.dt <- summary.dt[,c("text_original","id","screenName","created","text"),with=F]
  
  summary.dt[,tweetURL:=getTweetLink(screenName,id,id)]
  summary.dt[,userURL:=getTwitterUserLink(screenName,screenName)]
  summary.dt$link <- extractShortLinks(summary.dt$text_original)

  summary.dt <- summary.dt[,c("created","text_original","link","tweetURL","userURL"),with=F]
  colnames(summary.dt) <- c("Date Created","Text","Link","Tweet","User")

  return(summary.dt)
}


#Generates the find jobs panel.
findJobsPanel <- tabPanel("Search Jobs",
                          sidebarPanel(
                            
                            textInput("txtSearchBox", "1. Insert search string:","")
                            ,sliderInput("n", 
                                         "2. Set number of tweets to get:", 
                                         value = 200,
                                         min = 1, 
                                         max = 250)
                            ,br()
                            ,strong("3. Click on the button:")
                            ,br()
                            ,actionButton("cmdSearchTwitter","Go!")
                            
                            )
                          
                          
                          ,mainPanel(
                            DT::dataTableOutput("tblTweets")
                            
                          )
)

