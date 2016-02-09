#This script creates an OAuth credentials file.
#It will be used as a "key" to login in Twitter by StreamR.
# Original idea and more info:
# http://pablobarbera.com/blog/archives/1.html

#Load required library.
library(ROAuth)

#Get Twitter API credentials from XML file.
dfCredentials <- xmlToDataFrame(doc ="twitter_credentials.xml" )

### PART 1 ### 
# Declare Twitter API Credentials & Create Handshake

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

#Assigning the secret keys got from the XML file.
consumerKey <- as.character(dfCredentials[[1]][1])
consumerSecret <- as.character(dfCredentials[[1]][2])

#New instance of the Oauth factory object.
my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

#Remove variables to avoid sensitive information to be accessed from memory.
rm (consumerSecret)
rm (consumerKey)
rm (requestURL)
rm (accessURL)
rm (dfCredentials)

#Execute the next step and wait for the web browser to open. Then grant access to your previously created application here https://apps.twitter.com/app/new
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

#Now wait and introduce on R console the PIN provided on Twitter so that the OAuth file is generated.
### STOP HERE!!! ###


##### PART 2 #####
# Save the my_oauth data to an .Rdata file. This file will be used in future steps to login on Twitter when running automated tasks...
save(my_oauth, file = "my_oauth_twitter.Rdata")

#Now the key that will be used by StreamR to connect to Twitter is ready. This script has the only purpose of generating it and won't be 
#used anymore except if another PIN is required for a new application.
