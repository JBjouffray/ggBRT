#' ggPDfit
#'
#' Function to draw partial dependency plots with fitted values from a boosted regression trees obtained with the gbm.step routine in the dismo package.
#'
#' @param gbm.object a gbm.step object (object of S3 class gbm)
#' @param n.plots number of predictors to display, ranked by decreasing relative influence (default=all)
#' @param nrow number of plots per row
#' @param ncol number of plots per column
#' @param predictor select a single variable. Can be either a character name (use "") or a number indicating its ranking in terms of relative influence
#' @param col.dot color of the dots
#' @param cex.dot size of the dots
#' @param alpha.dot transparency of the dots (default = 0.4)
#' @param smooth Logical. If TRUE, (default FALSE) add a smoother to the fitted values
#' @param col.smooth color of the smoother
#' @param cex.smooth size of the smoother line (default = 0.3)
#' @param span span of the smoother (default = 0.3)
#' @param se Logical. If TRUE (default FALSE), add standard error around the smoother
#' @param y.label title for the y-axis (default = "Fitted values")
#' @param x.label title for the x-axis (by default the predictor name and its relative influence)
#' @export
ggPDfit<-function (gbm.object,n.plots = length(pred.names),predictor = NULL, nrow=NULL,ncol=NULL,
                       col.dot="grey",cex.dot=1,alpha.dot=0.4,smooth = FALSE,col.smooth="red",span=0.3, se=F,
                       cex.smooth=0.3,y.label = "Fitted values", x.label=NULL,...){


  gbm.call <- gbm.object$gbm.call
  pred.names <- gbm.call$predictor.names

  ggPDfit.plots<-function (gbm.object) {
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

      fittedVal<-list()
      ggPDfit<-list()

      for (i in 1:n.plots) {
        k <- match(gbm.object$contributions$var[i], pred.names)
        var.name <- gbm.call$predictor.names[k]

        fittedVal[[i]]<-data.frame(gbm.object$fitted,dat[i])
        colnames(fittedVal[[i]])<-c("y","x")

        ggPDfit[[i]]<- ggplot(fittedVal[[i]],aes(x=x,y=y))+
          geom_point(color=col.dot,alpha=alpha.dot,cex=cex.dot)+ylab(y.label)+
          xlab(paste(var.name, "  (", round(gbm.object$contributions[i,2], 1), "%)", sep = ""))+
          theme_bw()+
          theme(panel.grid.minor = element_line(linetype = "blank"),
                panel.grid.major = element_line(linetype = "blank"),
                axis.title.x  = element_text(size=10),
                axis.line.y = element_line(size=0.1),
                axis.line.x=element_line(size=0.1))

        if (smooth==T){
          ggPDfit[[i]]<-ggPDfit[[i]]+geom_smooth(span=span,size=cex.smooth,color=col.smooth,se=se,linetype=2)
        }
      }
      list(ggPDfit=ggPDfit)
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
      ggPDfit<- ggplot(fittedVal, aes(x=x,y=y))+
        geom_point(color=col.dot,alpha=alpha.dot,cex=cex.dot)+
        ylab(y.label)+
        xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor,2], 1), "%)", sep = ""))+
        theme_bw()+
        theme(panel.grid.minor = element_line(linetype = "blank"),
              panel.grid.major = element_line(linetype = "blank"),
              axis.title.x  = element_text(size=10),
              axis.line.y = element_line(size=0.1),
              axis.line.x=element_line(size=0.1))

      if (smooth==T){
        ggPDfit<-ggPDfit+geom_smooth(span=span,size=cex.smooth,color=col.smooth,se=se,linetype=2)
      }
    }
    list(ggPDfit=ggPDfit)
  }

  plot<-ggPDfit.plots(gbm.object)

  if(is.null(predictor)){
    do.call(grid.arrange,c(plot$ggPDfit,list(nrow=nrow,ncol=ncol)))}
  else grid.draw(plot$ggPDfit)
}
