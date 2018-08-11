#' ggPD
#'
#' Function to draw partial dependency plots with fitted function from a boosted regression trees obtained with the gbm.step routine in the dismo package.
#'
#' @param gbm.object a gbm.step object (object of S3 class gbm)
#' @param common.scale Logical. If TRUE (default), all the plots have the same y limits
#' @param n.plots number of predictors to display, ranked by decreasing relative influence (default=all)
#' @param nrow: number of plots per row
#' @param ncol number of plots per column
#' @param predictor select a single variable. Can be either a character name (use "") or a number indicating its ranking in terms of relative influence
#' @param col.line color of the line
#' @param cex.line size of the line (default = 0.5)
#' @param smooth Logical. If TRUE, (default FALSE) add a smoother to the fitted function
#' @param col.smooth color of the smoother
#' @param cex.smooth size of the smoother line (default = 0.3)
#' @param span span of the smoother (default = 0.3)
#' @param rug Logical. If TRUE (default FALSE), add rugs to the plot indicating the actual data points
#' @param rug.pos position of the rugs (default = "t"). Can be either "t" (top), "l" (left), "b" (bottom), or "r" (right)
#' @param y.label title for the y-axis (default = "Fitted function")
#' @param x.label title for the x-axis (by default the predictor name and its relative influence)
#' @export
ggPD<-function (gbm.object,n.plots = length(pred.names),predictor = NULL, nrow=NULL,ncol=NULL,
                col.line="darkorange",cex.line=0.5,smooth = FALSE,col.smooth="blue",cex.smooth=0.3,
                span=0.3,rug = FALSE,rug.pos="t",common.scale = TRUE, y.label = "Fitted function",
                x.label=NULL,...){

  gbm.call <- gbm.object$gbm.call
  pred.names <- gbm.call$predictor.names

  ggPD.plots<-function (gbm.object) {
    if (!requireNamespace("gbm")) {
      stop("you need to install the gbm package to run this function")
    }
    requireNamespace("splines")
    gbm.x <- gbm.call$gbm.x
    response.name <- gbm.call$response.name
    data <- gbm.call$dataframe
    xdat <- as.data.frame(data[, gbm.x])

    max.vars <- length(gbm.object$contributions$var)
    if (n.plots > max.vars) {
      n.plots <- max.vars
      warning("reducing no of plotted predictors to maximum available (",
              max.vars, ")")
    }
    predictors <- list(rep(NA, n.plots))
    responses <- list(rep(NA, n.plots))

    for (j in c(1:max.vars)) {
      k <- match(gbm.object$contributions$var[j], pred.names)

      if (is.null(x.label)) {
        var.name <- gbm.call$predictor.names[k]
      }
      else {
        var.name <- x.label
      }
      pred.data <- data[, gbm.call$gbm.x[k]]
      response.matrix <- gbm::plot.gbm(gbm.object, k, return.grid = TRUE)
      predictors[[j]] <- response.matrix[, 1]
      if (is.factor(data[, gbm.call$gbm.x[k]])) {
        predictors[[j]] <- factor(predictors[[j]], levels = levels(data[,gbm.call$gbm.x[k]]))
      }
      responses[[j]] <- response.matrix[, 2] - mean(response.matrix[, 2])

      if (j == 1) {
        ymin = min(responses[[j]])
        ymax = max(responses[[j]])
        dat<-data.frame(pred.data)
      }
      else {
        ymin = min(ymin, min(responses[[j]]))
        ymax = max(ymax, max(responses[[j]]))
        dat<-data.frame(dat,pred.data)
      }
    }

    if (is.null(predictor)){

      fittedFunc<-list()
      fittedVal<-list()
      ggPD<-list()

      for (i in 1:n.plots) {
        k <- match(gbm.object$contributions$var[i], pred.names)
        var.name <- gbm.call$predictor.names[k]

        fittedFunc[[i]]<-data.frame(predictors[i],responses[i])
        colnames(fittedFunc[[i]])<-c("x","y")

        fittedVal[[i]]<-data.frame(gbm.object$fitted,dat[i])
        colnames(fittedVal[[i]])<-c("y","x")

        if (is.factor(fittedFunc[[i]]$x)) {
          ggPD[[i]]<- ggplot(fittedFunc[[i]], aes(x=x,y=y))+
            geom_boxplot(color=col.line,size=cex.line)+
            ylab(y.label)+
            xlab(paste(var.name, "  (", round(gbm.object$contributions[i,2], 1), "%)", sep = ""))+
            theme_bw()+
            theme(panel.grid.minor = element_line(linetype = "blank"),
                  panel.grid.major = element_line(linetype = "blank"),
                  axis.text.x  = element_text(size=6),
                  axis.title.x  = element_text(size=10),
                  axis.line.y = element_line(size=0.1),
                  axis.line.x=element_line(size=0.1))

          if (common.scale==T){
            ggPD[[i]]<-ggPD[[i]]+ylim(c(ymin,ymax))}
        }
        else {

          ggPD[[i]]<- ggplot(fittedFunc[[i]], aes(x=x,y=y))+
            geom_line(color=col.line,size=cex.line)+
            ylab(y.label)+
            xlab(paste(var.name, "  (", round(gbm.object$contributions[i,2], 1), "%)", sep = ""))+
            theme_bw()+
            theme(panel.grid.minor = element_line(linetype = "blank"),
                  panel.grid.major = element_line(linetype = "blank"),
                  axis.title.x  = element_text(size=10),
                  axis.line.y = element_line(size=0.1),
                  axis.line.x=element_line(size=0.1))

          if (smooth==T){
            ggPD[[i]]<-ggPD[[i]]+geom_smooth(span=span,size=cex.smooth,color=col.smooth,se=F,linetype=2)
          }

          if (rug==T){
            ggPD[[i]]<-ggPD[[i]]+geom_rug(data=fittedVal[[i]],aes(x=x,y=y),sides=rug.pos,position="jitter",color="#EBEBEB")
          }

          if (common.scale==T){
            ggPD[[i]]<-ggPD[[i]]+ylim(c(ymin,ymax))
          }
        }
      }
      list(ggPD=ggPD)
    }

    else{

      if (is.character(predictor)){
        predictor<-match(predictor,gbm.object$contributions$var)}

      k <- match(gbm.object$contributions$var[predictor], pred.names)
      var.name <- gbm.call$predictor.names[k]

      fittedFunc<-data.frame(predictors[predictor],responses[predictor])
      colnames(fittedFunc)<-c("x","y")

      fittedVal<-data.frame(gbm.object$fitted,dat[predictor])
      colnames(fittedVal)<-c("y","x")

      if (is.factor(fittedFunc$x)) {
        ggPD<- ggplot(fittedFunc, aes(x=x,y=y))+
          geom_boxplot(color=col.line,size=cex.line)+
          ylab(y.label)+
          xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor,2], 1), "%)", sep = ""))+
          theme_bw()+
          theme(panel.grid.minor = element_line(linetype = "blank"),
                panel.grid.major = element_line(linetype = "blank"),
                axis.text.x  = element_text(size=6),
                axis.title.x  = element_text(size=10),
                axis.line.y = element_line(size=0.1),
                axis.line.x=element_line(size=0.1))

        if (common.scale==T){
          ggPD<-ggPD+ylim(c(ymin,ymax))}
      }
      else {

        ggPD<- ggplot(fittedFunc, aes(x=x,y=y))+
          geom_line(color=col.line,size=cex.line)+
          ylab(y.label)+
          xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor,2], 1), "%)", sep = ""))+
          theme_bw()+
          theme(panel.grid.minor = element_line(linetype = "blank"),
                panel.grid.major = element_line(linetype = "blank"),
                axis.title.x  = element_text(size=10),
                axis.line.y = element_line(size=0.1),
                axis.line.x=element_line(size=0.1))

        if (smooth==T){
          ggPD<-ggPD+geom_smooth(span=span,size=cex.smooth,color=col.smooth,se=F,linetype=2)
        }

        if (rug==T){
          ggPD<-ggPD+geom_rug(data=fittedVal,aes(x=x,y=y),sides=rug.pos,position="jitter",color="#EBEBEB")
        }

        if (common.scale==T){
          ggPD<-ggPD+ylim(c(ymin,ymax))
        }
      }
      list(ggPD=ggPD)
    }
  }
  plot<-ggPD.plots(gbm.object)

  if(is.null(predictor)){
    do.call(grid.arrange,c(plot$ggPD,list(nrow=nrow,ncol=ncol)))}
  else grid.draw(plot$ggPD)
}



