#' ggInteract_3D
#'
#' Function to draw 3D interaction plot showing predicted values for two predictors from a boosted regression trees obtained with the gbm.step routine in the dismo package. Values for all other variables are set at their mean by default. Possibility to export an interactive version of the plot in html format.
#'
#' @param gbm.object a gbm.step object (object of S3 class gbm)
#' @param x the first predictor. Can be either a character name (use "") or a number indicating its index
#' @param y the second predictor. Can be either a character name (use "") or a number indicating its index
#' @param col.gradient a vector of two colors to define the color gradient of the plot or a predefined palette
#' @param legend Logical. If TRUE (default) shows the legend
#' @param main title for the plot
#' @param x.label title for the x-axis
#' @param y.label title for the y-axis
#' @param z.label title for the z-axis
#' @param x.range manual range for the x-axis
#' @param y.range manual range for the y-axis
#' @param z.range manual range for the z-axis
#' @param pred.means allows specification of values for other variables (see gbm.perspec in dismo)
#' @param smooth controls smoothing of the predicted surface (default= "none")
#' @export
ggInteract_3D<-function (gbm.object,x = 1, y = 2,col.gradient="Blues",legend=T,
                         main="", x.label = NULL,y.label = NULL,x.range = NULL,
                         y.range = NULL,z.label = "Fitted value",z.range = NULL,
                         pred.means = NULL,smooth = "none",...) {

  gbm.call <- gbm.object$gbm.call
  pred.names <- gbm.call$predictor.names
  gbm.x <- gbm.call$gbm.x
  n.preds <- length(gbm.x)
  gbm.y <- gbm.call$gbm.y
  family = gbm.call$family
  have.factor <- FALSE

  if (is.character(x)){
    x<-match(x,pred.names)}

  if (is.character(y)){
    y<-match(y,pred.names)}

  x.name <- gbm.call$predictor.names[x]
  if (is.null(x.label)) {
    x.label <- gbm.call$predictor.names[x]}

  y.name <- gbm.call$predictor.names[y]
  if (is.null(y.label)) {
    y.label <- gbm.call$predictor.names[y]}

  data <- gbm.call$dataframe[, gbm.x, drop = FALSE]
  n.trees <- gbm.call$best.trees

  if (is.vector(data[, x])) {
    if (is.null(x.range)) {
      x.var <- seq(min(data[, x], na.rm = T), max(data[, x], na.rm = T), length = 50)
    }
    else {
      x.var <- seq(x.range[1], x.range[2], length = 50)
    }
  } else
  {
    x.var <- names(table(data[, x]))
    have.factor <- TRUE
  }

  if (is.vector(data[, y])) {
    if (is.null(y.range)) {
      y.var <- seq(min(data[, y], na.rm = T), max(data[, y], na.rm = T), length = 50)
    }
    else {
      y.var <- seq(y.range[1], y.range[2], length = 50)
    }
  } else {
    y.var <- names(table(data[, y]))
    if (have.factor) {
      stop("at least one marginal predictor must be a vector!")
    }
    else {
      have.factor <- TRUE
    }
  }

  pred.frame <- expand.grid(list(x.var, y.var))
  names(pred.frame) <- c(x.name, y.name)
  pred.rows <- nrow(pred.frame)

  if (have.factor) {
    if (is.factor(pred.frame[, 2])) {
      pred.frame <- pred.frame[, c(2, 1)]
      x.var <- y.var
    }
  }

  j <- 3
  for (i in 1:n.preds) {
    if (i != x & i != y) {
      if (is.vector(data[, i])) {
        m <- match(pred.names[i], names(pred.means))
        if (is.na(m)) {
          pred.frame[, j] <- mean(data[, i], na.rm = T)}
        else pred.frame[, j] <- pred.means[m]
      }
      if (is.factor(data[, i])) {
        m <- match(pred.names[i], names(pred.means))
        temp.table <- table(data[, i])
        if (is.na(m)) {
          pred.frame[, j] <- rep(names(temp.table)[2],pred.rows)}
        else {
          pred.frame[, j] <- pred.means[m]
        }
        pred.frame[, j] <- factor(pred.frame[, j], levels = names(temp.table))
      }
      names(pred.frame)[j] <- pred.names[i]
      j <- j + 1
    }
  }

  # form the prediction

  prediction <- gbm::predict.gbm(gbm.object, pred.frame, n.trees = n.trees,type = "response")

  # model smooth if required

  if (smooth == "model") {
    pred.glm <- glm(prediction ~ ns(pred.frame[,1], df = 8) * ns(pred.frame[,2], df = 8), data=pred.frame,family=poisson)
    prediction <- fitted(pred.glm)
  }

  # report the maximum value and set up realistic ranges for z

  max.pred <- max(prediction)
  cat("maximum value = ",round(max.pred,2),"\n")

  if (is.null(z.range)) {
    if (family == "bernoulli") {
      z.range <- c(0, 1)
    }
    else if (family == "poisson") {
      z.range <- c(0, max.pred * 1.1)
    }
    else {
      z.min <- min(data[, y], na.rm = T)
      z.max <- max(data[, y], na.rm = T)
      z.delta <- z.max - z.min
      z.range <- c(z.min - (1.1 * z.delta), z.max + (1.1 * z.delta))
    }
  }

  # form the matrix

  pred.matrix <- matrix(prediction,ncol=50,nrow=50)

  # kernel smooth if required

  if (smooth == "average") {  #apply a 3 x 3 smoothing average
    pred.matrix.smooth <- pred.matrix
    for (i in 2:49) {
      for (j in 2:49) {
        pred.matrix.smooth[i,j] <- mean(pred.matrix[c((i-1):(i+1)),c((j-1):(j+1))])
      }
    }
    pred.matrix <- pred.matrix.smooth
  }

  # PLOT

  p3D<-plot_ly(x =y.var, y=x.var, z=pred.matrix, colors = col.gradient, showscale = legend,
               cmin = z.range[1],cmax = z.range[2],colorbar=list(title=z.label)) %>%
    add_surface() %>%
    layout(title = main,
           scene = list(xaxis = list(title = y.label),
                        yaxis = list(title = x.label),
                        zaxis = list(title = z.label,range=z.range)))
  p3D
}
