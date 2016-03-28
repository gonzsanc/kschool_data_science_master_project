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


getTweetsDataFrame <- function (query,n=250){
  
  return( twitteR::twListToDF (searchTwitter(query,  n=n, retryOnRateLimit=1, lang = "en")))
}


#sampleDF <- getTweetsDataFrame("job boston")
