#Load streamR library to capture Stream tweets from Twitter API.
library(streamR)

#Get authentication file.
load("my_oauth_twitter.Rdata")

#Capture tweets based upon multiple keywords.
#Save input as JSON file.
#Wait 4 hours before closing connection.
filterStream(file.name = "DELETEME.json", # Save tweets in a json file
             track = c("recommend","advice","recommended","adviced","needed",
                       "search","I search","we search","we're searching","we need"
                       ,"where can I","were can","where is there","do you know","does anybody know"
                       ,"where can we","give advice","advice requested","please", "can't find"
                       ,"compare","comparing", "I don't find","how to find","which is better"
                       ,"we wish","help","info","information", "inform","helping","you help"
                       ,"urgent","do you know","does anybody know","could you tell me",
                       "looking for","I am looking for","we look for","we are looking for",
                       "require","required","know of","wanted","necessity"
                       ), 
             language = c("en")
             ,timeout = 60*60*4, 
             oauth = my_oauth)

#Remark. Using 4Gb's RAM the computer hardly was able to handle files larger than 500Mbs.
#        With 8Gbs 1Gb size files were handled easily, and 3Gb's seemed to make R crash.
#        For this reason, it is adviced to monitor the size of the output file and cancel
#        the extraction once the critical size is reached and create a new file if more 
#        data is required.