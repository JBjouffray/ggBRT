#' ggPerformance
#'
#' Function to extract the models performance of one or several boosted regression trees obtained with the gbm.step routine in the dismo package.
#'
#' @param ... one or several gbm.step objects (object of S3 class gbm)
#' @param global.env Logical. If TRUE (default FALSE) exports the result into the R global environment as an object called ModelPerf. Be aware of the critics of exporting to the global environment
#'
#' @return Returns a dataframe with mean total deviance, mean residual deviance, correlation, area under the receiver operating curve (AUC), percentage deviance explained, cross-validated deviance, cross-validated correlation, cross-validated AUC and cross-validated percentage deviance explained (calculated with the formula 1-(cross-valdiated deviance/Total.Deviance))
#' @export
ggPerformance<-function (...,global.env=F){

  BRT <- list(...)
  nBRT <- length(BRT)

  totDEV<-list()
  resDEV<-list()
  COR<-list()
  AUC<-list()
  PER<-list()
  cvDEV<-list()
  cvCOR<-list()
  cvAUC<-list()
  cvPER<-list()

  # Extract model performance parameters
  for(i in 1:nBRT){
    totDEV[[i]]<-BRT[[i]]$self.statistics$mean.null
    resDEV[[i]]<-BRT[[i]]$self.statistics$mean.resid
    COR[[i]]<-BRT[[i]]$self.statistics$correlation
    AUC[[i]]<-BRT[[i]]$self.statistics$discrimination
    PER[[i]]<-(1-BRT[[i]]$self.statistics$mean.resid/BRT[[i]]$self.statistics$mean.null)*100
    cvDEV[[i]]<-BRT[[i]]$cv.statistics$deviance.mean
    cvCOR[[i]]<-BRT[[i]]$cv.statistics$correlation.mean
    cvAUC[[i]]<-BRT[[i]]$cv.statistics$discrimination.mean
    cvPER[[i]]<-(1-BRT[[i]]$cv.statistics$deviance.mean/BRT[[i]]$self.statistics$mean.null)*100
  }

  # Create empty dataframe to receive the results
  ModelPerf<-data.frame(matrix(vector(), 9, nBRT))

  # Assign the different parameters to the dataframe
  for(i in 1:nBRT){
    ModelPerf[1,i]<-totDEV[i]
    ModelPerf[2,i]<-resDEV[i]
    ModelPerf[3,i]<-COR[i]
    ModelPerf[4,i]<-AUC[i]
    ModelPerf[5,i]<-PER[i]
    ModelPerf[6,i]<-cvDEV[i]
    ModelPerf[7,i]<-cvCOR[i]
    ModelPerf[8,i]<-cvAUC[i]
    ModelPerf[9,i]<-cvPER[i]
    if(is.null(names(BRT))){
      colnames(ModelPerf)[i] <- paste("Model", i, sep = " ")}
    else{
      colnames(ModelPerf)[i] <- names(BRT)[i]}
  }
  rownames(ModelPerf)<-c("Total.Deviance","Residual.Deviance","Correlation","AUC","Per.Expl","cvDeviance","cvCorrelation","cvAUC","cvPer.Expl")

  ## Be aware of the critics of exporting to the global environment
  if(global.env==T){
    assign("ModelPerf", ModelPerf, envir = .GlobalEnv)
  }
  return(ModelPerf)
}
