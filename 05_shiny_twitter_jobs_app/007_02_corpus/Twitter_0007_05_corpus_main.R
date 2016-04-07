#PLOT SIZE DYNAMIC
#http://stackoverflow.com/questions/13792195/how-do-i-conditionally-change-the-aspect-ratio-of-charts-in-rs-shiny-package

require(cluster)
library(ggplot2)
#Loads dendrogram panel.
source("./007_02_corpus/Twitter_0007_06_corpus_dendrogram.R")
source("./007_02_corpus/Twitter_0007_07_corpus_terms.R")
source("./007_02_corpus/Twitter_0007_08_corpus_topics.R")

#Main menu function ui.R
corpusPanel <- navbarMenu("Corpus Inspection",
                          DendrogramtabPanel
                          ,TemrsTabPanel
                          
)



#Topic Model -> https://rstudio-pubs-static.s3.amazonaws.com/66739_c4422a1761bd4ee0b0bb8821d7780e12.html