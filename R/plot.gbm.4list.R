#' plot.gbm.4list
#'
#''gbm.bootstrap.functions', 'plot.gbm.4list' and 'plot.gbm.boot' were originally written by Jane Elith and John Leathwick, but not released publicly. We thank them for providing this code.
#'
#' @export

plot.gbm.4list <-
  function (x, i.var = NULL, n.trees = x$n.trees, continuous.resolution = 100,
            return.grid = TRUE,  ...)
  {
    res <- list()
    for(zz in 1:length(x$var.names)){
      i.var <- zz
      if (all(is.character(i.var))) {
        i <- match(i.var, x$var.names)
        if (any(is.na(i))) {
          stop("Plot variables not used in gbm model fit: ",
               i.var[is.na(i)])
        }
        else {
          i.var <- i
        }
      }
      if ((min(i.var) < 1) || (max(i.var) > length(x$var.names))) {
        warning("i.var must be between 1 and ", length(x$var.names))
      }
      if (n.trees > x$n.trees) {
        warning(paste("n.trees exceeds the number of trees in the model, ",
                      x$n.trees, ". Plotting using ", x$n.trees, " trees.",
                      sep = ""))
        n.trees <- x$n.trees
      }
      if (length(i.var) > 3) {
        warning("plot.gbm creates up to 3-way interaction plots.\nplot.gbm will only return the plotting data structure.")
        return.grid = TRUE
      }
      grid.levels <- vector("list", length(i.var))
      for (i in 1:length(i.var)) {
        if (is.numeric(x$var.levels[[i.var[i]]])) {
          grid.levels[[i]] <- seq(min(x$var.levels[[i.var[i]]]),
                                  max(x$var.levels[[i.var[i]]]), length = continuous.resolution)
        }
        else {
          grid.levels[[i]] <- as.numeric(factor(x$var.levels[[i.var[i]]],
                                                levels = x$var.levels[[i.var[i]]])) - 1
        }
      }
      X <- expand.grid(grid.levels)
      names(X) <- paste("X", 1:length(i.var), sep = "")
      #assign("myres",X,immediate=T)
      res[[zz]] <- X
    }
    return(res)
  }
