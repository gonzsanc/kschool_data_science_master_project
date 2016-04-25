library(twitteR)
library(ROAuth)
library(stringr)
library(XML)


# Declare Twitter API Credentials & Create Handshake
#dfCredentials <- xmlToDataFrame(doc ="./auth_cred/twitter_credentials.xml" )
#Assigning the secret keys got from the XML file.
#consumerKey <- as.character(dfCredentials[[1]][1])
#consumerSecret <- as.character(dfCredentials[[1]][2])

#cred <- OAuthFactory$new(consumerKey=consumerKey,
#                         consumerSecret=consumerSecret,
#                         requestURL='https://api.twitter.com/oauth/request_token',
#                         accessURL='https://api.twitter.com/oauth/access_token',
#                         authURL='https://api.twitter.com/oauth/authorize')

#download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#cred$handshake(cainfo="cacert.pem")

#save(cred, file="twitter authentication.Rdata")
#rm (dfCredentials)
#rm(consumerKey)
#rm(consumerSecret)



load("./auth_cred/twitter authentication.Rdata")
options(httr_oauth_cache=F)
setup_twitter_oauth(cred$consumerKey
                    ,cred$consumerSecret
                    ,cred$oauthKey
                    ,cred$oauthSecret
)


getTweetsDataFrame <- function (query,n=750,preformat=T){
  
  tweets <- twitteR::twListToDF (searchTwitter(query,  n=n, retryOnRateLimit=1, lang = "en"))
  
  if (preformat){
    tweets$text <- sapply(tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  }
  
  
  return(tweets)
}

#Uses job specific keywords to retrieve tweets. 
#retrieve similar topics from twitter.
getTweetsTrainingKeywords <- function (n=750,preformat=T){
  
  keywords <-  paste(c("job",'"job offer"','"vacant position"',"vacancy",'"vacant positions"',
                       "hiring","full-time","part-time",'"permanent job"','"temporary job"','"temporary worker"'
                       ,'"freelance worker"','freelancer','"season worker"',
                       "position","vacant","apply","application","hire")
                     ,collapse = " OR ")
  
  return(getTweetsDataFrame (query=keywords,n=n,preformat=T))
  
}


#sampleDF <- getTweetsDataFrame("job boston")
