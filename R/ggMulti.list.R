#' ggMulti.list
#'
#'Function used in ggMultiPD and ggMultiPDfit to display results from different BRTs (with same predictors) on a single plot
#' @export
ggMulti.list<-function (gbm.object, variable.no = 0, n.plots = length(pred.names),
                        y.label = "Fitted function", x.label=NULL,...)
{
  if (!requireNamespace("gbm")) {
    stop("you need to install the gbm package to run this function")
  }
  requireNamespace("splines")
  gbm.call <- gbm.object$gbm.call
  gbm.x <- gbm.call$gbm.x
  pred.names <- gbm.call$predictor.names
  response.name <- gbm.call$response.name
  data <- gbm.call$dataframe
  xdat <- as.data.frame(data[, gbm.x])

  if (length(variable.no) > 1) {
    stop("only one response variable can be plotted at a time")
  }
  if (variable.no > 0) {
    n.plots <- 1
  }
  max.vars <- length(gbm.object$contributions$var)
  if (n.plots > max.vars) {
    n.plots <- max.vars
    warning("reducing no of plotted predictors to maximum available (",
            max.vars, ")")
  }
  predictors <- list(rep(NA, n.plots))
  responses <- list(rep(NA, n.plots))

  for (j in c(1:n.plots)) {
    if (n.plots == 1) {
      k <- variable.no
    }
    else {
      k <- match(gbm.object$contributions$var[j], pred.names)
    }
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
      predictors[[j]] <- factor(predictors[[j]], levels = levels(data[,
                                                                      gbm.call$gbm.x[k]]))
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

  fittedFunc<-list()
  fittedVal<-list()
  FUNC<-list()
  VAL<-list()

  for (i in 1:n.plots) {

    k <- match(gbm.object$contributions$var[i], pred.names)
    var.name <- gbm.call$predictor.names[k]

    fittedFunc[[i]]<-data.frame(predictors[i],responses[i],var.name,response.name)
    colnames(fittedFunc[[i]])<-c("x","y","Predictor","response")

    fittedVal[[i]]<-data.frame(gbm.object$fitted,dat[i],var.name,response.name)
    colnames(fittedVal[[i]])<-c("y","x","Predictor","response")

    if (i==1){
      FUNC<-fittedFunc[[1]]
      VAL<-fittedVal[[1]]
    }
    else{
      FUNC<-rbind(FUNC,fittedFunc[[i]])
      VAL<-rbind(VAL,fittedVal[[i]])
    }
  }

  list(FUNC=FUNC,VAL=VAL)
}
