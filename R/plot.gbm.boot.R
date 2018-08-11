#' plot.gbm.boot
#'
#' This is the plot.gbm code edited to allow it to get the var.levels from the model fitted to the full data set, but then to plot functions from models fitted to datasets with difft ranges (as in what happens for bootstrapped data). This is used within the gbm.bootstrap.functions code.
#' 'gbm.bootstrap.functions', 'plot.gbm.4list' and 'plot.gbm.boot' were originally written by Jane Elith and John Leathwick, but not released publicly. We thank them for providing this code.
#' @export
plot.gbm.boot <- function (x, list.4.preds, i.var = NULL, n.trees = x$n.trees, continuous.resolution = 100,
                           return.grid = TRUE,  ...){

  # updated 2013 for change in plotting somewhere in R
  # x is the gbm.object fitted to the full datasets
  # list.4.preds is the list of X values for each predictor in turn from a single run of a cutdown version of plot.gbm
  res <- matrix(NA, nrow=continuous.resolution, ncol=length(x$var.names))
  for(zz in 1:length(x$var.names)){
    X <- list.4.preds[[zz]]
    amt.data <- nrow(X)
    i.var <- zz

    X$y <- .Call("gbm_plot", X = as.double(data.matrix(X)), cRows = as.integer(nrow(X)),
                 cCols = as.integer(ncol(X)), n.class = as.integer(x$num.classes), i.var = as.integer(i.var -
                                                                                                        1), n.trees = as.integer(n.trees), initF = as.double(x$initF),
                 trees = x$trees, c.splits = x$c.splits, var.type = as.integer(x$var.type),
                 PACKAGE = "gbm")

    if (return.grid) {
      #names(X)[1:length(i.var)] <- x$var.names[i.var]
      res[1:amt.data,zz] <- X$y
    }
  }
  return(res)
}
