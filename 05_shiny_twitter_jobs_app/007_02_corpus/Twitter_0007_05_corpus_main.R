require(cluster)
drawDendrogram <- function (corpus,
                            sparsity=0.97,number_of_clusters=10, fontSize=1.0){
    #Deondogram
    #corpus <- messages.corpus
    dtm <- DocumentTermMatrix(corpus)
    dtmss <- removeSparseTerms(x=dtm,sparse = sparsity)
    number_of_clusters <- min(dtmss$ncol,number_of_clusters)
    d <- dist(t(dtmss), method="euclidian")   
    fit <- hclust(d=d, method="ward.D2")
    #plot(fit)
    plot(fit, hang = -1, cex = fontSize,xlab = "Document Term Matrix Distances",main=NULL)
    if (number_of_clusters>1){
      rect.hclust(fit, k=number_of_clusters, border="red")
    }
}

    
#    ==================================
#    i.R
#    ==================================
#    Generates the find jobs subpanel.
    DendrogramtabPanel <- tabPanel("Dendrogram",
                            
                              fluidRow(
                                column(3,
                                       
                                     sliderInput('ddSparsity', 'Matrix Sparsity', 
                                                 min=0.01, max=0.9999, value=0.98, 
                                                 step=0.002)
                                    )
                              
                              ,column(3,
                                    
                                    sliderInput('ddClusters', 'Number of Clusters', 
                                                  min=1, max=25, value=5, 
                                                  step=1)
                              )
                            
                              ,column(3,
                                      #
                                      sliderInput('ddFontSize', 'Font Size', 
                                                  min=0.3, max=3, value=1, 
                                                  step=0.1)
                              )
                              
                              )
                           ,hr()
                           ,plotOutput("Dendrogram")
                           
)



#Main menu function ui.R
corpusPanel <- navbarMenu("Corpus Inspection",
                          DendrogramtabPanel
)
