
#Twitter search function
if(!exists("getTweetsDataFrame", mode="function")) 
  source("./007_01_start_search_engine/Twitter_007_001_SearchTwitter.R")

#TM Objects function
if(!exists("cleanText", mode="function")) 
  source("./007_01_start_search_engine/Twitter_007_003_CleanTweets.R")

#Summary Pane Contents
if(!exists("genTweetTabTable", mode="function")) 
  source("./007_01_start_search_engine/Twitter_007_004_Search_Jobs_TabPanel.R")

