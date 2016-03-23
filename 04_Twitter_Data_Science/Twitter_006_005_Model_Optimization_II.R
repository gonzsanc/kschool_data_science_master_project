#setwd("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto")
#Required libraries:
require(data.table)
require(tm)
require(dismo)
require(gbm)

#Converts a dataframe having the specified column names
#The function removes all the columns not included in the columnames list
#The function preserves all the data from the columns which names match the ones in the columnNames list.
get.standard.record <- function(columnNames,dataFrame){
  
  dataFrame <- as.data.frame(dataFrame)
  #print (dataFrame)
  
  #Inserts in the dataframe the columns in the list that are missing.
  cols <- !(columnNames %in% colnames(dataFrame))
  if (length(cols)>0)
    cols <- columnNames[cols]
  for(i in cols) {
    #print (i)
    dataFrame[,i] <- 0
  }
  
  #Removes all the columns of the dataframe not in the columnNames list.
  cols <- !(colnames(dataFrame) %in% columnNames)
  if(length(cols)>0){
    for(i in names(dataFrame)[cols]) {
      dataFrame[,i] <- NULL
    }
  }
  
  return (dataFrame)  
}


getTfidffromCorpus <- function (corpus){
  #Term matrix with TF-IDF weighting
  return (DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))))
}

#Adapts tdm for regression models.
#convert_count <- function(x) {
#  y <- x
#  y
#}


load ("./TwitterData/003_iii_rda/sample.jobs.messages.processed.rda")
load("./TwitterData/003_iii_rda/sample.job.messages.dtm.rda")
load("./TwitterData/003_iii_rda/sample.job.messages.corpus.rda")


#Separating jobs offers from other messages and creating the required text mining objects.
jobs_indices <- which(sample.job.messages$is_job_offer_factor == 1)
noise_indices <- which(sample.job.messages$is_job_offer_factor == 0)
training_indices <- which(sample.job.messages$is_for_training == 1)
testing_indices <- which(sample.job.messages$is_for_training == 0)
jobs_raw_train <- sample.job.messages[training_indices]
jobs_raw_test <- sample.job.messages[testing_indices]


#REBALANCING
#Twice no jobs than jobs:
RebalancingFactor = 0.01 #ratio -> jobs/nojobs
#RebalancingFactor = 0.6 #ratio -> jobs/nojobs

#Which messages for training are jobs offers and which are not.
indexj1 <- which (jobs_raw_train$is_job_offer==1)
indexj0 <- which (jobs_raw_train$is_job_offer==0)
j1 <- jobs_raw_train[indexj1]

#Number of rows to get:
RowsToInclude <- min(nrow(jobs_raw_train),nrow(j1)/RebalancingFactor)
set.seed(12345)
if (RowsToInclude>=nrow(jobs_raw_train)){
  j0 <- jobs_raw_train
}else{
  j0 <- jobs_raw_train[sample(indexj0, RowsToInclude)]
}
set.seed(12345)
#Tweet and other Twitter objects collection:
job_train.tfidf <- sample(rbind (j1,j0))

#Explicitly: Not using those datasets anymore..
rm(j0)
rm(j1)

#The following process analyzes effects after changes on sparsity in the data set.
bar = seq(0.90,0.999, by=0.001)
getTPRatio <- function (){
  r <- data.frame(TPRatio=numeric(),sparsity=numeric()
                  ,TN=numeric(),FP=numeric(),FN=numeric(),TP=numeric())
  for (i in 1:length(bar) ) {
    sparsity <- bar[i]
    print (bar[i])
    job_train.tfidf.corpus  <- Corpus(VectorSource(job_train.tfidf$textProcessed))
    job_train.tfidf.dtm <- getTfidffromCorpus(job_train.tfidf.corpus)
    job_train.tfidf.dtm.df <- as.data.frame(as.matrix(job_train.tfidf.dtm))
    job_train.tfidf.dtm.df <- as.data.table(job_train.tfidf.dtm.df)
    job_train.tfidf.dtm.nonsparse  <- removeSparseTerms(job_train.tfidf.dtm,sparsity)
    job_train.tfidf.dtm.nonsparse.dt <- as.data.table(as.data.frame(as.matrix(job_train.tfidf.dtm.nonsparse)))
    
    job_test.tfidf.corpus  <- Corpus(VectorSource(jobs_raw_test$textProcessed))
    job_test.tfidf.dtm <- getTfidffromCorpus(job_test.tfidf.corpus)
    job_test.tfidf.dtm.df <- as.data.frame(as.matrix(job_test.tfidf.dtm))
    job_test.tfidf.dtm.df <- as.data.table(job_test.tfidf.dtm.df)
    job_test.tfidf.dtm.df <- get.standard.record(colnames(job_train.tfidf.dtm.df),job_test.tfidf.dtm.df)
    job_test.tfidf.dtm.nonsparse.dt <- get.standard.record(colnames(job_train.tfidf.dtm.nonsparse.dt),job_test.tfidf.dtm.df)
    
    
    f <- as.formula(job_train.tfidf$is_job_offer ~ .)
    fit.glm <- glm(f ,data=job_train.tfidf.dtm.nonsparse.dt, family = "binomial")
    p <- predict(fit.glm,job_test.tfidf.dtm.nonsparse.dt)
    tab <-  (table(ifelse(p>=0.5,1,0),jobs_raw_test$is_job_offer))
    r <- rbind(r,data.frame(TPRatio=(1-tab[2]/tab[4]),sparsity=sparsity,TN=tab[1],FP=tab[2],FN=tab[3],TP=tab[4]))
    save(r,file="res.rda")
    print (r[nrow(r),])
  }
  
  return (r)
}

#res <- getTPRatio()
#res <- as.data.frame(res)
#save(res,file="res.rda")
#TPRatio sparsity  TN  FP  FN TP
#1    0.9000000    0.900 610   3 107 30
#..   .........    .........   . ... ...
#55   0.9000000    0.954 610   3 107 30
#56   0.8333333    0.955 607   6 101 36
#..   .........    .........   . ... ...
#71   0.8636364    0.970 607   6  93 44
#..   .........    .........   . ... ...
#81   0.8474576    0.980 604   9  78 59
#..   .........    .........   . ... ...
#84   0.8305085    0.983 603  10  78 59
#..   .........    .........   . ... ...
#97   0.5679012    0.996 578  35  56 81
#98   0.3188406    0.997 566  47  68 69
#99   0.1388889    0.998 551  62  65 72
#100 -0.9047619    0.999 493 120  74 63


#document term  matrix
#Now an additional DTM to filter by sparsity is included.
job_train.tfidf.corpus  <- Corpus(VectorSource(job_train.tfidf$textProcessed))
job_train.tfidf.dtm <- getTfidffromCorpus(job_train.tfidf.corpus)
job_train.tfidf.dtm.df <- as.data.frame(as.matrix(job_train.tfidf.dtm))
job_train.tfidf.dtm.df <- as.data.table(job_train.tfidf.dtm.df)
job_train.tfidf.dtm.nonsparse  <- removeSparseTerms(job_train.tfidf.dtm,0.980)
job_train.tfidf.dtm.nonsparse.dt <- as.data.table(as.data.frame(as.matrix(job_train.tfidf.dtm.nonsparse)))

job_test.tfidf.corpus  <- Corpus(VectorSource(jobs_raw_test$textProcessed))
job_test.tfidf.dtm <- getTfidffromCorpus(job_test.tfidf.corpus)
job_test.tfidf.dtm.df <- as.data.frame(as.matrix(job_test.tfidf.dtm))
job_test.tfidf.dtm.df <- as.data.table(job_test.tfidf.dtm.df)
job_test.tfidf.dtm.df <- get.standard.record(colnames(job_train.tfidf.dtm.df),job_test.tfidf.dtm.df)
job_test.tfidf.dtm.nonsparse.dt <- get.standard.record(colnames(job_train.tfidf.dtm.nonsparse.dt),job_test.tfidf.dtm.df)

job_train.tfidf.dtm.nonsparse.dt.f <- NULL
job_train.tfidf.dtm.nonsparse.dt.f$is_job_offer <- job_train.tfidf$is_job_offer
job_train.tfidf.dtm.nonsparse.dt.f <- as.data.table(job_train.tfidf.dtm.nonsparse.dt.f)
job_train.tfidf.dtm.nonsparse.dt.f <- cbind(job_train.tfidf.dtm.nonsparse.dt.f,job_train.tfidf.dtm.nonsparse.dt)

job_test.tfidf.dtm.nonsparse.dt.f <- NULL
job_test.tfidf.dtm.nonsparse.dt.f$is_job_offer <- jobs_raw_test$is_job_offer
job_test.tfidf.dtm.nonsparse.dt.f <- as.data.table(job_test.tfidf.dtm.nonsparse.dt.f)
job_test.tfidf.dtm.nonsparse.dt.f <- cbind(job_test.tfidf.dtm.nonsparse.dt.f,job_test.tfidf.dtm.nonsparse.dt)
job_test.tfidf.dtm.nonsparse.dt.f <- get.standard.record(colnames(job_train.tfidf.dtm.nonsparse.dt.f),dataFrame = job_test.tfidf.dtm.nonsparse.dt.f)


f <- as.formula(job_train.tfidf$is_job_offer ~ .)
fit.glm <- glm(f ,data=job_train.tfidf.dtm.nonsparse.dt, family = "binomial")
p <- predict(fit.glm,job_test.tfidf.dtm.nonsparse.dt)
table(ifelse(p>=0.5,1,0),jobs_raw_test$is_job_offer)
#0   1
#0 604  77
#1   9  60


#Oblique classification trees.
#install.packages("oblique.tree")

library(oblique.tree)
N=nrow(job_train.tfidf)
fit.oblique.tree <-oblique.tree ( as.factor(is_job_offer) ~., data = job_train.tfidf.dtm.nonsparse.dt.f
                                  ,oblique.splits = "on",
                                  control = tree.control (nobs =N ,mincut =50) , 
                                  split.impurity ="deviance")


pred <-predict (fit.oblique.tree , newdata = job_test.tfidf.dtm.nonsparse.dt.f,type ="class")
table(job_test.tfidf.dtm.nonsparse.dt.f$is_job_offer,pred , dnn =c("Observed Class","Predicted Class" ))
#The classification tree is not influenced by the sparsity.
#               Predicted Class
#Observed Class   0   1
#             0 597  16
#             1  76  61


#Conditional trees (forward stepwise)
library(partykit) 
#install.packages("partykit")
fit.ctree <- ctree (is_job_offer ~., data = job_train.tfidf.dtm.nonsparse.dt.f)

plot(fit.ctree, gp = gpar(fontsize = 9),
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = F, 
       id = FALSE)
)

p <- predict(fit.ctree,job_test.tfidf.dtm.nonsparse.dt)
table(ifelse(p>=0.5,1,0),jobs_raw_test$is_job_offer)
#    0   1
#0 528  41
#1  85  96
#CHECK Linear Model Based Recursive PartitioninG AND OTHERS.


#https://cran.r-project.org/web/packages/evtree/vignettes/evtree.pdf
#Global
#install.packages("evtree")
library(evtree)
#The results of recursive tree methods are only locally optimal,  as splits are chosen to maximize homogeneity at the next  step  only.

evtc <- evtree.control (minbucket = 7L, minsplit = 20L, maxdepth = 5,
                        niterations = 10000L, ntrees = 1000L, alpha = 1)
fit.evtree <- evtree (is_job_offer ~., data = job_train.tfidf.dtm.nonsparse.dt.f,
                      control =evtc )

#save(fit.evtree,file="fit.evtree.rda")
pred <- predict(fit.evtree,job_test.tfidf.dtm.nonsparse.dt)
table(ifelse(pred>=0.5,1,0),jobs_raw_test$is_job_offer)
#0   1
#0 598  79
#1  15  58
#1-15/58
plot(fit.evtree)

#SVN
require(e1071)
fit.svm <- svm(as.factor(is_job_offer) ~.
               , data = job_train.tfidf.dtm.nonsparse.dt.f, 
               kernel = "polynomial",type="C-classification"
               ,degree=4)

pred <- predict(fit.svm,job_test.tfidf.dtm.nonsparse.dt)
tab <- table(pred,jobs_raw_test$is_job_offer)
tab
#red   0   1
#0 600  77
#1  13  60
#1- tab[2]/tab[4]
#0.7833333

summary(fit.svm)
#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  1 
#degree:  4 
#gamma:  0.05263158 
#coef.0:  0 

#Number of Support Vectors:  859
#( 423 436 )
#Number of Classes:  2 

#Levels: 
#  0 1


#install.packages("scatterplot3d")
require(scatterplot3d)
scatterplot3d ( results$nu , results$gamma , results$Performance , xlab ="nu",ylab ="gamma",zlab ="Performance")

#Relevance Vector Machine
#The RVM algorithms use identical operating model as the SVMs, but 
#they use probabilistic Bayesian inference instead for regression and classification.
require (kernlab)
fit.rvm <- rvm (is_job_offer ~ .,
             data = job_train.tfidf.dtm.nonsparse.dt.f,
             kernel =rbfdot ,
             kpar =list( sigma =0.5) ,
             cross =10)
save (fit.rvm,file="fit.rvm.rda")


pred <- predict(fit.rvm,job_test.tfidf.dtm.nonsparse.dt)
tab <- table(ifelse(pred>=0.5,1,0),jobs_raw_test$is_job_offer)
tab
#   0   1
#0 613 131
#1   0   6
fit.rvm.poly <- rvm (is_job_offer ~ .,
                data = job_train.tfidf.dtm.nonsparse.dt.f,
                kernel =polydot ,
                kpar =list(degree=3, offset=2, scale=1.5) ,
                cross =2)
save (fit.rvm.poly,file="fit.rvm.poly.rda")
pred <- predict(fit.rvm.poly,job_test.tfidf.dtm.nonsparse.dt)
tab <- table(ifelse(pred>=0.2,1,0),jobs_raw_test$is_job_offer)
tab

#Error in chol.default(crossprod(Kr)/var + diag(1/thetatmp)) : 
#the leading minor of order 4 is not positive definite
#Probably unbalanced data


fit.rvm.test <- rvm (is_job_offer ~ .,
                data = job_train.tfidf.dtm.nonsparse.dt.f,
                kernel =rbfdot ,
                kpar =list( sigma =0.9) ,
                cross =5)
#save (fit.rvm.test,file="fit.rvm.test.rda")


pred <- predict(fit.rvm.test,job_test.tfidf.dtm.nonsparse.dt)
tab <- table(ifelse(pred>=0.5,1,0),jobs_raw_test$is_job_offer)
tab
#0   1
#0 613 131
#1   0   6

fit.rvm.test <- rvm (is_job_offer ~ .,
                     data = job_train.tfidf.dtm.nonsparse.dt.f,
                     kernel =rbfdot ,
                     kpar =list( sigma =0.01) ,
                     cross =10)
#save (fit.rvm.test,file="fit.rvm.test.rda")
pred <- predict(fit.rvm.test,job_test.tfidf.dtm.nonsparse.dt)


tab <- table(ifelse((pred)>0,1,0),jobs_raw_test$is_job_offer)
tab
#    0   1
#1 613 137


#NEURAL
install.packages ("grnn")
require(grnn)

grnn_fit.basic <- learn (job_train.tfidf.dtm.nonsparse.dt.f)
grnn_fit <- smooth (grnn_fit.basic , sigma = 1.2)
summary(grnn_fit)


for (i in 1:nrow(job_test.tfidf.dtm.nonsparse.dt))
{
  pred [i]<-guess(grnn_fit,as.matrix(job_test.tfidf.dtm.nonsparse.dt[i,]))

}
tab <- table(ifelse((pred)>0.5,1,0),jobs_raw_test$is_job_offer)
tab
#0   1
#0 599  78
#1  14  59
#  0.2372881

#https://cran.r-project.org/web/packages/RSNNS/RSNNS.pdf
install.packages("RSNNS")
require(RSNNS)

number_of_layers <- 1
neurons_per_layer <- ceiling(ncol(job_train.tfidf.dtm.nonsparse.dt.f)/1)

#set.seed(12345)
fit.elman <- elman(job_train.tfidf.dtm.nonsparse.dt,job_train.tfidf.dtm.nonsparse.dt.f$is_job_offer
               ,size =c(number_of_layers ,neurons_per_layer) ,
               learnFuncParams =c (0.001) ,
               maxit =1000)

summary(fit.elman)

plotIterativeError(fit.elman)

pred <- predict(fit.elman,job_test.tfidf.dtm.nonsparse.dt)
tab <- table(ifelse((pred)>0.5,1,0),jobs_raw_test$is_job_offer)
tab
1-tab[2]/tab[4]


#jordan network is failing, probably because the data set is not optimized.
#fit.jordan <- jordan (job_train.tfidf.dtm.nonsparse.dt,job_train.tfidf.dtm.nonsparse.dt.f$is_job_offer
#                   ,size =c(number_of_layers ,neurons_per_layer) ,
#                   learnFuncParams =c (0.1) ,
#                   maxit =1000)


#Random Forests
#Rebalancing factor reset to 0.6 and data regenerated for this test.
library(randomForest)

fit.rf <- randomForest(
  as.factor(is_job_offer) ~ ., data = job_train.tfidf.dtm.nonsparse.dt.f
  , importance =T, ntree=500, proximity=T,oob.prox=T
  )

plot(fit.rf)
fit.rf

varImpPlot(fit.rf)
