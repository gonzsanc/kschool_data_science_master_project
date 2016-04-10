library(igraph)
require(SnowballC)
require(tm)
#require (ggplot2)

#Generates TDM filtered by term frequency.
getTDMbyFreq <- function (corpus,
                            minFreq=10){
  corpus <- messages.corpus
  tdm <- TermDocumentMatrix(corpus)
  #tdm <- TermDocumentMatrix(messages.corpus)
  frqTerms <- findFreqTerms(tdm, lowfreq = minFreq)
  tdm <- tdm[frqTerms, 1:tdm$ncol]
  return(tdm)
}

#Draws a term frequency plot filtered by minimun term occurence.
drawTermFreqPlot <- function (corpus, minFreq=10){
  #debug corpus<-messages.corpus
  #debug minFreq<-10
  tdm <- getTDMbyFreq(corpus,minFreq)
  tdm.matrix <- as.matrix(tdm)
  tdm.matrix.termsFreq <- colSums(t(tdm.matrix))
  tdm.matrix.termsFreq <-melt(tdm.matrix.termsFreq)
  
  tdm.matrix.termsFreq$term <- rownames(tdm.matrix.termsFreq)
  colnames(tdm.matrix.termsFreq) <- c("count","term")
  tdm.matrix.termsFreq <- tdm.matrix.termsFreq[order(tdm.matrix.termsFreq$count,decreasing = T),]
  
  f <- ggplot(tdm.matrix.termsFreq , aes(x = reorder(term,-count) , y = count))
  f + geom_bar(aes(color=term),
               stat="identity", fill="white") +
  xlab("Terms") + ylab("Count") + 
  geom_text(aes(label=count), hjust=-0.5, size=3.0) +
  coord_flip(ylim = c(0, max(tdm.matrix.termsFreq$count)*1.05))
}
#drawTermFreqPlot(messages.corpus,8)

#    ==================================
#    ui.R
#    ==================================
    TemrsTabPanel <- tabPanel("Term Analysis",
      fluidRow(
        
        column(width = 2, numericInput("termFreq","Min. Term Frequency",10,min = 1,step = 1))
        
      )
      
      ,hr()
      ,mainPanel(
        
        #Pages of corpus -> term analysis tabsetpanel.
        tabsetPanel(
          #TERM FREQUENCY
          tabPanel("Term Frequency", h2("Terms Occurrences"),plotOutput("termFreqPlot")),
          
          #PCA sample
          tabPanel("PCA Terms Correlation",
                   h2("Tf-idf PCA Correlation Matrix & Variability"),
                   
                   fluidRow(
                   sidebarPanel(width=4,p("here is the correlation matrix")),   
                  column(width=8,
                   plotOutput("PCACorrelationMatrix"))
                   ) 
                  ),
          #TOPICS
          tabPanel("Topics" 
                   ,fluidRow(
                    
                     column(width = 4,
                     h2("Latent Dirichlet Allocation"))
                     ,
                    
                     column(width = 4,
                     selectInput("LDAMethod", "Method:",
                                 c("VEM algorithm" = "VEM",
                                   "VEM algorithm - Fixed" = "VEM_fixed",
                                   "Gibbs Sampling" = "Gibbs",
                                   "CTM model" = "CTM"
                                   ))
                     ,
                     downloadButton("downloadLDA","Download Data")
                     
                     ) 
                     ,
                     column(width = 4,
                            sliderInput("numberOfTopicsSlider", label="Number of Topics",min = 1,max=50,value = 2,step = 1)
                     )
                   )#eof fluidrow
                    ,hr()
                   ,fluidRow (
                     column(width = 12, DT::dataTableOutput("tblLDA"))  
                   )
                   ) #https://rstudio-pubs-static.s3.amazonaws.com/66739_c4422a1761bd4ee0b0bb8821d7780e12.html
          
          
        )
          
      )
      
    )