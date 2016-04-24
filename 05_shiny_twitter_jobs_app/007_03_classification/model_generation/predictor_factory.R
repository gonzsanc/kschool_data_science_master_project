#FACTORY PATTERN TO MAKE PREDICTORS.
if (!require(dismo)) require(dismo)

#General Factory function.
factoryPredictor <- function (){
  rtrn <- list()
  rtrn$makeGBMPredictor <- makeGBMPredictor()
  return (rtrn)
  
}

#GBM Predictor producer.
makeGBMPredictor <- function(){
  load("007_03_classification/data_objects/borutaFeatures.rda")
  load("007_03_classification/data_objects/model.gbm.rda")
  rtrn <- list()
  rtrn$model <- model.gbm
  rtrn$predictors <- borutaFeatures
  return (rtrn)
}

#Factory predictor object.
FactoryPredictor <- factoryPredictor()
rm(factoryPredictor)
rm(makeGBMPredictor)

