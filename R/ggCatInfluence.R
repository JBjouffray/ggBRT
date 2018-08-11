#' ggCatInfluence
#'
#'Function to return the relative influence per category of predictors from boosted regression trees obtained with the gbm.step routine in the dismo package.
#'
#' @param ... several gbm.step object (object of S3 class gbm)
#' @param category the name of the column that contains the different categories associated to the predictors
#' @param df a dataframe that contains the predictors and their categories
#' @param signif Logical. If TRUE (default FALSE), retains only the "significant" predictors (Muller et al. 2013) and rescale their influence to 100
#' @export
ggCatInfluence<-function (...,category=NULL,data=df,signif=FALSE){

  BRT<-list(...)
  nBRT<-length(BRT)
  dfContr<-list()
  SigPred<-list()
  SigCat<-list()
  SigInf<-list()
  A=NULL

  if(signif==F){

    # Extract the relative influence of each predictor for each model
    for(i in 1:nBRT){
      dfContr[[i]]<-data.frame(BRT[[i]]$contributions)
      colnames(dfContr[[i]])<-c("Predictor",paste("Model", i, sep = ""))
      rownames(dfContr[[i]]) <- 1:nrow(dfContr[[i]])}

    for(i in 1:nBRT){
      if(is.null(category)){
        return(list(Relative.Influence=dfContr))}

      else {
        for(i in 1:nBRT){
          if (i == 1) {
            dfCat<-merge(data,dfContr[[i]])}
          else {
            dfCat<-merge(dfCat,dfContr[[i]])}
        }
      }
    }
    Rel.Infl<-aggregate(dfCat[paste0('Model',1:nBRT)],by=dfCat[category],FUN=sum)
    Rel.Infl
  }

  else{
    for(i in 1:nBRT){
      dfContr[[i]]<-data.frame(BRT[[i]]$contributions)
      colnames(dfContr[[i]])<-c("Predictor",paste("Influence", i, sep = ""))

      if (i == 1) {
        dfCat<-merge(data,dfContr[[i]])}
      else {
        dfCat<-merge(dfCat,dfContr[[i]])}

      SigPred[[i]]<-subset(dfContr[[i]],dfContr[[i]][[2]]>(100/length(dfContr[[i]][[1]])))
      SigPred[[i]]$sig.inf<-SigPred[[i]][[2]]*100/sum(SigPred[[i]][[2]])
      SigCat[[i]]<-merge(data,SigPred[[i]])
      SigInf[[i]]<-ddply(SigCat[[i]], .(get(paste(category))), summarize,  Influence=sum(sig.inf))
      SigInf[[i]]$Regime<-paste("Model", i, sep = "")
      A<-bind_rows(A,SigInf[[i]])}
    colnames(A)<-c("Category","Influence","Model")
    A<-reshape::cast(A, Category ~ Model,value="Influence")
    A
  }
}
