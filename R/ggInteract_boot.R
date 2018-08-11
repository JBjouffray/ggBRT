#' ggInteract_boot
#'
#' Function to randomize the response n times to generate a distribution under the null hypothesis of no interaction among predictors for one or several pairs of predictors (following Pinsky and Byler 2015). Model parameters should be the same as for the observed interaction strength. Results allow to compare with observed interaction strength.
#'
#' @param ... one or several pair of interacting predictors as vectors (e.g. c(pred1, pred2), c(pred3,pred4))
#' @param nboots the number of randomization (default=100)
#' @param data the dataset for the boosted regression trees (BRT) (default=NULL)
#' @param response the response variable of the BRT (default=NULL)
#' @param predictors the set of predictors for the BRT (default=NULL)
#' @param tc tree complexity for the BRT (default=3)
#' @param lr learning rate for the BRT (default=0.005)
#' @param bf bag fraction for the BRT (default=0.75)
#' @param family the distribution family for the BRT (default=“bernoulli”)
#' @param global.env Logical. If TRUE (default) export the result into the R global environment as an object called Interact_boots
#'
#' @return Returns a dataframe of interaction sizes with as many columns as there are pairs of predictors (+1) and as many rows as the number of randomizations.
#' @export

ggInteract_boot<-function(...,nboots=100,data=NULL,response=NULL,predictors=NULL,
                            tc=3,lr=0.005,bf=0.75,family="bernoulli", global.env=T){

  IntList<-list(...)
  nInt<-length(IntList)

  if (nInt<1) {
    stop("you need to specify pairs of interacting predictors as vectors")
  }
  if (is.null(data)) {
    stop("you need to specify the BRT dataset")
  }
  if (is.null(predictors)) {
    stop("you need to specify the BRT predictors as a vector")
  }
  if (is.null(response)) {
    stop("you need to specify the BRT response")
  }

  if (is.character(response)){
    response<-match(response,names(data))}

  a <- rep(NA, nboots)
  boots = data.frame(maxint = a)
  for (l in 1:nInt){
    boots = data.frame(boots,col=a)
    names(boots)[ncol(boots)] <- paste0("Interaction", l)
  }

  i <- 1
  while(i <= nboots){
    print(paste('ITERATION', i))
    dat = data
    dat[[response]] = sample(dat[[response]], size = nrow(dat)) # randomize the response
    brt <- gbm.step(data=dat,
                    gbm.x = predictors,
                    gbm.y = response,
                    family = family,
                    tree.complexity = tc,
                    learning.rate = lr,
                    bag.fraction = bf, plot.main=FALSE, verbose=FALSE, silent=TRUE)

    if(!is.null(brt)){
      interact <- gbm.interactions(brt)
      boots$maxint[i] = interact$rank.list$int.size[1]

      for (k in 1:nInt){
        j <- which((interact$rank.list$var1.names == IntList[[k]][1] & interact$rank.list$var2.names == IntList[[k]][2]) | (interact$rank.list$var1.names == IntList[[k]][2] & interact$rank.list$var2.names == IntList[[k]][1]))
        if(length(j)>0){
          boots[[paste0("Interaction", k)]][i] = interact$rank.list$int.size[j]
        } else {
          boots[[paste0("Interaction", k)]][i] = min(interact$rank.list$int.size)
        }
      }
      i <- i+1
    }
  }

  if (global.env==T){
    assign("Interact_boots", boots, envir = .GlobalEnv)}

  return(boots)
}
