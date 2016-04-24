#library(igraph)
require(SnowballC)
require(tm)
require(rCharts)
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
  
  p <- nPlot(count ~ term, 
             data =as.data.frame(tdm.matrix.termsFreq),
             type = 'multiBarHorizontalChart'
             ,heigth=10
  )
 
    p$params$height <-   25 * (nrow(tdm.matrix.termsFreq))

  p$chart(margin = max(tdm.matrix.termsFreq$count)*1.4)
  p$yAxis (axisLabel="Term Frequency in Corpus")
  p$chart(valueFormat="#!d3.format('d')!#")
  p$chart(showValues=T)
  p$chart(showControls=F)
  
  bcol <- getRandomColors(len = nrow(tdm.matrix.termsFreq))
  p$chart(barColor = bcol)
  
  
  return (p)
}

#Draws a term frequency plot filtered by minimun term occurence.
drawTermFreqPlotOld <- function (corpus, minFreq=10){
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
  xlab("Terms") + ylab("Occurrences") + 
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
          tabPanel("Term Frequency", h2("Number of Term Occurrences"),showOutput("termFreqPlot","nvd3")),
          
          #PCA sample
          tabPanel("PCA Terms Correlation", 
                   h2("Tf-idf PCA Correlation Matrix & Variability"),
                  fluidRow(   
                    sidebarPanel(width=5,
                                 plotOutput("PCAVariance"),
                                 hr(),
                                 p("Regularization with unsupervised Principal Component Analysis is used
                                   for both reducing data dimensionality and organizing the data
                                   into clusters. The generated groups, namely the components, may also
                                   be considered as a topic collection that defines the composition of the corpus. 
                                   The central idea is to transform a set of data with p attributes into a new data
                                   set with k attributes, being k<p (dimensionality reduction), while keeping most
                                   of the data variability in the new reduced data set. It is possible then to 
                                   examine still the new reduced model and infere the influence of the different
                                   factors on the original data set by analyzing the reduced one.")
                                 
                                 
                                 ),
                    column(width=7,
                            fluidRow(plotOutput("PCACorrelationMatrix"))
                     ,
                    br(),
                    hr(),
                    br(),
                     p("The correlation or covariance matrix displays the strenght and direction of 
                       the associations among temrs across the corpus. It is created from
                       the term document matrix, extracting the correlation among terms."),
                    p("The scores in the term document matrix are usually
                        normalized, in either logarithmic or binary unscaled weights.
                      The most popular score transformation is Term frequency - inverse document frequency
                      because it allows to remove noise and highlight the most characteristic and
                      significative terms that constitute the signal in the corpora.")
                    ,p("As the term frequency requierement is increased (numeric box above), it may be verified 
                      how more specific terms are isolated and the PCA model improves because the noise is reduced, corresponding 
                       to the same technique as in Tf-idf scoring.")
                    )
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
                     

                     #,downloadButton("downloadLDA","Download Data")
                     
                     ) 
                     ,
                     column(width = 4,
                            sliderInput("numberOfTopicsSlider", label="Number of Topics",min = 2,max=100,value = 2,step = 1)
                            #,downloadButton("downloadTopics","Download Messages")
                            )
                   )#eof fluidrow
                   ,fluidRow(
                      column(width = 4,p()),
                      column(width = 4,downloadButton("downloadLDA","Download Data")),
                      column(width = 4,downloadButton("downloadMessagesLDA","Download Messages"))
                   )
                    ,hr()
                   ,fluidRow (
                     column(width = 12, DT::dataTableOutput("tblLDA"))  
                   )
                   ) #https://rstudio-pubs-static.s3.amazonaws.com/66739_c4422a1761bd4ee0b0bb8821d7780e12.html
          
          
        ),style='width: 900px;'
          
      )
      
    )
