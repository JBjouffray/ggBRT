#' ggInfluence
#'
#' Function to return and plot the relative influence of predictors from boosted regression trees obtained with the gbm.step routine in the dismo package.
#'
#' @param gbm.object a gbm.step object (object of S3 class gbm)
#' @param plot Logical. If TRUE (default) draws a horizontal barplot of the relative influence of each predictor
#' @param col.bar color of the bars
#' @param show.signif Logical. If TRUE (default), display a vertical dashed line indicating the threshold of significance (100/number of predictors, Muller et al. 2013)
#' @param col.signif color of the dashed line
#' @param main title for the plot (by default the name of the response variable)
#' @param x.label title for the x-axis
#' @param signif Logical. If TRUE (default FALSE), retains only the "significant" predictors (Muller et al. 2013) and rescale their influence to 100
#'
#' @return Returns a dataframe with the relative influence of each predictor in the model and a barplot

#'@export
ggInfluence<-function (gbm.object,col.bar="skyblue3", show.signif = TRUE,
col.signif="#8B3A3A",main=gbm.call$response.name,
x.label="Relative influence (%)",signif=FALSE,plot=T,...){

  gbm.call <- gbm.object$gbm.call

  if(signif==F){

    dfContr<-data.frame(gbm.object$contributions)
    Influence<-dfContr[2]

    # Create the barplot
    ggInfluence<- ggplot(dfContr, aes(x=reorder(var,rel.inf),y=rel.inf))+
      geom_bar(fill=col.bar,stat="identity")+
      coord_flip()+
      theme(axis.title=element_text(size=13),
            axis.text = element_text(face = "plain"),
            plot.title = element_text(size = 14, face = "bold"),
            axis.line.x = element_line(size = 0.3),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_line(size = 0.2,linetype = "dotted",color="grey"),
            panel.grid.major.x = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"))+
      ylab(x.label)+
      xlab("")+
      ggtitle(main)

    if (show.signif == TRUE) {
      ggInfluence<- ggInfluence+geom_hline(yintercept=100/length(dfContr$var),colour=col.signif,linetype="dashed")
    }

    if (plot==T){
      print(ggInfluence)}
    return(Influence)
  }

  else{
    dfContr<-data.frame(gbm.object$contributions)

    # Retain only the "significant" predictors and rescale to 100%
    dfContr<-subset(dfContr,dfContr[[2]]>(100/length(dfContr[[1]])))
    dfContr[[2]]<-dfContr[[2]]*100/sum(dfContr[[2]])
    Influence<-dfContr[2]

    # Create the barplot
    ggInfluence<- ggplot(dfContr, aes(x=reorder(var,rel.inf),y=rel.inf))+
      geom_bar(fill=col.bar,stat="identity")+
      coord_flip()+
      theme(axis.title=element_text(size=13),
            axis.text = element_text(face = "plain"),
            plot.title = element_text(size = 14, face = "bold"),
            axis.line.x = element_line(size = 0.3),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_line(size = 0.2,linetype = "dotted",color="grey"),
            panel.grid.major.x = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"))+
      ylab(x.label)+
      xlab("")+
      ggtitle(main)

    if (plot==T){
      print(ggInfluence)}
    return(Influence)
  }
}
