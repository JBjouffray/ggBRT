#' ggInteract_list
#'
#' Function to return the interactions size for a given boosted regression trees obtained with the gbm.step routine in the dismo package.
#'
#' @param gbm.object a gbm.step object (object of S3 class gbm)
#' @param index Logical. If TRUE (default) returns the interactions table with the index of each predictors in addition to their names
#' @param global.env Logical. If TRUE (default FALSE) exports the result into the R global environment as an object called  Interact_list. Be aware of the critics of exporting to the global environment
#'
#' @return Returns a dataframe with the interaction sizes ranked from largest to smallest
#' @export
ggInteract_list<-function (gbm.object, index=T, global.env=F){

  interact<-gbm.interactions(gbm.object)
  Interact_list<-data.frame(interact$rank.list)

  ## Be aware of the critics of exporting to the global environment
  if (global.env==F){
    if (index==T){
      return(Interact_list)}

    else {
      Interact_list<-Interact_list[,c(2,4,5)]
      return(Interact_list)
    }
  }

  else{
    if (index==T){
      assign("Interact_list", Interact_list, envir = .GlobalEnv)
      return(Interact_list)}

    else {
      Interact_list<-Interact_list[,c(2,4,5)]
      assign("Interact_list", Interact_list, envir = .GlobalEnv)
      return(Interact_list)}
  }
}
