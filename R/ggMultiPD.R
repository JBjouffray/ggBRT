#' ggMultiPD
#'
#' Function to draw partial dependency plots with fitted functions from several boosted regression trees obtained with the gbm.step routine in the dismo package. The models need to have the same set of predictors.
#'
#' @param ... several gbm.step object (object of S3 class gbm)
#' @param predictor select a single predictor to plot. Can be either a character name (use "") or a number indicating its ranking in terms of relative influence
#' @param n.plots number of predictors to display, ranked by decreasing relative influence (default=all)
#' @param nrow number of plots per row
#' @param ncol number of plots per column
#' @param col.lines a vector with as many colors as there are BRTs
#' @param cex.line size of the lines
#' @param legend.pos position for the legend ("none", "right", "left", "bottom", "top")
#' @param smooth Logical. If TRUE (default FALSE) add a smoother to each fitted function
#' @param cex.smooth size of the smoothers
#' @param span span of the smoothers (default = 0.3)
#' @export
ggMultiPD<-function (...,n.plots=length(pred.names),predictor=NULL,col.lines=c(),
                     cex.line=0.5,legend.pos="right",smooth=F,cex.smooth=0.3,se=F,
                     span=0.3,nrow=NULL,ncol=NULL){

  BRTlist <- list(...)
  pred.names<-BRTlist[[1]]$gbm.call$predictor.names
  nBRT <- length(BRTlist)

  ggMulti.plotsPD<-function (...){


    if (!all(sapply(BRTlist, inherits, "gbm")))
      stop("Argument must be a gbm.step outcome")

    if (is.null(BRTnames <- names(BRTlist)))
      BRTnames <- paste("R", 1:nBRT, sep = "")

    if (any(empty <- BRTnames == ""))
      BRTnames[empty] <- paste("R", 1:nBRT, sep = "")[empty]
    names(BRTlist) <- BRTnames

    X<-lapply(BRTlist,ggMulti.list)

    for (i in 1:nBRT) {
      if(i==1){
        fitfunc<-X[[i]]$FUNC
        fitval<-X[[i]]$VAL
      }

      else{
        fitfunc<-rbind(fitfunc,X[[i]]$FUNC)
        fitval<-rbind(fitval,X[[i]]$VAL)
      }
    }

    if (is.null(predictor)){

      df<-list()
      ggMulti<-list()

      for (i in 1:n.plots) {
        df[[i]]<-subset(fitfunc,fitfunc$Predictor==pred.names[[i]])

        ggMulti[[i]]<- ggplot(df[[i]], aes(x=x,y=y,color=response))+
          geom_line(size=cex.line)+
          ylab("Fitted function")+
          xlab(pred.names[[i]])+
          theme_bw()+
          theme(panel.grid.minor = element_line(linetype = "blank"),
                panel.grid.major = element_line(linetype = "blank"),
                legend.position=legend.pos,
                legend.title=element_blank(),
                legend.key = element_rect(colour = "white"),
                axis.title.x  = element_text(size=12),
                axis.line.y = element_line(size=0.1),
                axis.line.x=element_line(size=0.1))

        if (smooth==T){
          ggMulti[[i]]<-ggMulti[[i]]+geom_smooth(span=span,se=se,size=cex.smooth,linetype=2)
        }

        if (length(col.lines==nBRT)){
          ggMulti[[i]]<-ggMulti[[i]]+scale_color_manual(values=col.lines)
        }
      }

      list(ggMulti=ggMulti)
    }

    else {

      if (is.numeric(predictor)){
        predictor<-pred.names[predictor]}

      df2<-subset(fitfunc,fitfunc$Predictor==predictor)
      ggMultiPlot<- ggplot(df2, aes(x=x,y=y,color=response))+
        geom_line(size=cex.line)+
        ylab("Fitted function")+
        xlab(predictor)+
        theme_bw()+
        theme(panel.grid.minor = element_line(linetype = "blank"),
              panel.grid.major = element_line(linetype = "blank"),
              legend.position=legend.pos,
              legend.title=element_blank(),
              legend.key = element_rect(colour = "white"),
              axis.title.x  = element_text(size=12),
              axis.line.y = element_line(size=0.1),
              axis.line.x=element_line(size=0.1))

      if (smooth==T){
        ggMultiPlot<-ggMultiPlot+geom_smooth(span=span,se=se,size=cex.smooth,linetype=2)
      }

      if (length(col.lines==nBRT)){
        ggMultiPlot<-ggMultiPlot+scale_color_manual(values=col.lines)
      }

      list(ggMultiPlot=ggMultiPlot)}
  }

  plot<-ggMulti.plotsPD(...)

  if(is.null(predictor)){
    do.call(grid.arrange,c(plot$ggMulti,list(nrow=nrow,ncol=ncol)))}
  else grid.draw(plot$ggMultiPlot)
}
