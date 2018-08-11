#' gbm.bootstrap.functions
#'
#' Function to calculate bootstrap estimates of 95% confidence intervals for partial plots of a brt model fitted with gbm.step. See e.g. Buston and Elith 2011 (J. Anim. Ecol). 'gbm.bootstrap.functions', 'plot.gbm.4list' and 'plot.gbm.boot' were originally written by Jane Elith and John Leathwick, but not released publicly. We thank them for providing this code.
#'
#' @param gbm.object a gbm object describing sample intensity
#' @param list.predictors a list of data from a run of plot.gbm.4list
#' @param n.reps number of bootstrap samples
#' @param n.divisions the number of used in continuous resolution of plot.gbm
#' @param verbose control reporting
#'
#' @export
gbm.bootstrap.functions <- function(
  gbm.object,          # a gbm object describing sample intensity
  list.predictors,  			  # a list of data from a run of plot.gbm.4list
  n.reps = 200,               # number of bootstrap samples
  n.divisions = 100,          # the number of used in continuous resolution of plot.gbm
  verbose = T)                # control reporting
{

  # based version 2.9 - J. Leathwick/J. Elith - June 2007
  # updated by Jane Elith 2008 to 2010, to deal with asin data and other various fixes
  # updated by JE Nov 2010 to cut it down just for partial plots, and use the plot.gbm code within that
  # updated by J-B Jouffray March 2017 to replace the use of eval(parse(text=))

  require(gbm)

  # first get the original analysis details..

  gbm.call <- gbm.object$gbm.call
  train.data <- gbm.call$dataframe
  n.obs <- nrow(train.data)
  gbm.x <- gbm.call$gbm.x
  gbm.y <- gbm.call$gbm.y
  family <- gbm.call$family
  lr <- gbm.call$learning.rate
  tc <- gbm.call$tree.complexity
  response.name <- gbm.call$response.name
  predictor.names <- gbm.call$predictor.names
  n.preds <- length(gbm.x)
  n.trees <- gbm.call$best.trees
  weights <- gbm.object$weights

  function.preds <- array(0, dim=c(n.divisions, n.preds,n.reps))

  cat("gbm.pred.bootstrap - version 2.9","\n\n")
  cat("bootstrap resampling gbm.step model for ",response.name,"\n",sep="")
  cat("with ",n.trees," trees and ",n.obs," observations\n\n",sep="")

  # initiate timing call

  z1 <- unclass(Sys.time())

  # create gbm.fixed function call
  # Note: the models fitted within the bootstrap all have the same number of
  # trees, and are not optimised for each bootstrap sample. This is a shortcut
  # that saves computation time; the effect on the outcome has not been explored
  # but intuitively it will be conservative in the sense that it will introduce
  # more noise if anything.

  gbm.call.string <- paste("gbm.fixed(data=boot.data,gbm.x=gbm.x,gbm.y=gbm.y,",sep="")
  gbm.call.string <- paste(gbm.call.string,"family=family,learning.rate=lr,tree.complexity=tc,",sep="")
  gbm.call.string <- paste(gbm.call.string,"n.trees = ",n.trees,", site.weights = weights,verbose=FALSE)",sep="")

  # now start the main bootstrap loop

  for (i in 1:n.reps) {

    if (i == 6 & verbose) {

      z2 <- unclass(Sys.time())
      est.time <- (z2 - z1)/60  # time for five reps
      est.time <- est.time * (n.reps/5)
      cat("five bootstrap samples processed \n"," estimated time for completion is ",
          round(est.time,1)," minutes \n",sep="")
    }
    else {
      if (verbose) cat(i,"\n")
    }

    # create a vector with which to select the bootstrap sample

    i.select <- sample(1:n.obs, n.obs, replace = T)
    boot.data <- train.data[i.select,]

    #fit the model
    boot.model.gbm <- eval(parse(text=gbm.call.string))
    #make the predictions
    function.preds[,,i] <- plot.gbm.boot(boot.model.gbm, list.4.preds = list.predictors, continuous.resolution = n.divisions)
  }

  # final timing call
  z2 <- unclass(Sys.time())
  elapsed.time <- round((z2 - z1)/60,2)
  cat("analysis took ",round(elapsed.time,1)," minutes \n\n")
  gbm.call$n.bootstrap.reps <- n.reps
  gbm.call$bootstrap.time <- elapsed.time
  final.object <- list(gbm.call = gbm.call)
  final.object$function.preds <- function.preds

  return(final.object)
}
