require(graphics)

#DENDROGRAM================================
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
    plot(fit, hang = -1, cex = fontSize,xlab = "Document-Term-Matrix Cluster Dendrogram",main=NULL)
    if (number_of_clusters>1){
      rect.hclust(fit, k=number_of_clusters, border="red")
    }
}

#DENDROGRAM================================
#CLUSTER
#http://stackoverflow.com/questions/24140339/tree-cut-and-rectangles-around-clusters-for-a-horizontal-dendrogram-in-r
rhc <- function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2, 
                 cluster = NULL) 
{
  if (length(h) > 1L | length(k) > 1L) 
    stop("'k' and 'h' must be a scalar")
  if (!is.null(h)) {
    if (!is.null(k)) 
      stop("specify exactly one of 'k' and 'h'")
    k <- min(which(rev(tree$height) < h))
    k <- max(k, 2)
  }
  else if (is.null(k)) 
    stop("specify exactly one of 'k' and 'h'")
  if (k < 2 | k > length(tree$height)) 
    stop(gettextf("k must be between 2 and %d", length(tree$height)), 
         domain = NA)
  if (is.null(cluster)) 
    cluster <- cutree(tree, k = k)
  clustab <- table(cluster)[unique(cluster[tree$order])]
  m <- c(0, cumsum(clustab))
  if (!is.null(x)) {
    if (!is.null(which)) 
      stop("specify exactly one of 'which' and 'x'")
    which <- x
    for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
  }
  else if (is.null(which)) 
    which <- 1L:k
  if (any(which > k)) 
    stop(gettextf("all elements of 'which' must be between 1 and %d", 
                  k), domain = NA)
  border <- rep_len(border, length(which))
  retval <- list()
  for (n in seq_along(which)) {
    rect(
      ybottom = m[which[n]] + 0.66,
      xright = par("usr")[3L],
      ytop = m[which[n] + 1] + 0.33,
      xleft = mean(rev(tree$height)[(k - 1):k]),
      border = border[n])
    retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
  }
  invisible(retval)
}

#DENDROGRAM DRAW
drawDendrogramOld <- function (corpus,
                               sparsity=0.97,number_of_clusters=10, fontSize=1.0){
  #Deondogram
  #corpus <- messages.corpus
  #sparsity=0.97
  
  #fontSize=1.0
  #number_of_clusters=5  
  dtm <- DocumentTermMatrix(corpus)
  dtmss <- removeSparseTerms(x=dtm,sparse = sparsity)
  number_of_clusters <- min(dtmss$ncol,number_of_clusters)
  d <- dist(t(dtmss), method="euclidian")   
  fit <- hclust(d=d, method="ward.D2")
  #plot(fit)
  
  clusDendro <- as.dendrogram(fit)
  #clusDendro <- dendrapply(clusDendro, fit$labels)
  op <- par(mar = par("mar") + c(0,0,0,2))
  plot(clusDendro,horiz=T)
  
  if (number_of_clusters>1){
  #  rect.hclust(fit, k=number_of_clusters, border="red")
    rhc(fit, k=number_of_clusters, border="red")
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
# END DENDROGRAM================================
