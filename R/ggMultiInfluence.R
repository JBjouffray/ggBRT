#' ggMultiInfluence
#'
#' Function to return and plot (heatmap) the relative influence of predictors from several boosted regression trees obtained with the gbm.step routine in the dismo package. The models need to have the same set of predictors.
#'
#' @param ... several gbm.step objects (object of S3 class gbm)
#' @param round indicates the number of decimal places to be used to round the relative influence values (default = 1)
#' @param col.gradient a vector of two colors to define the color gradient of the plot
#' @param col.text color of the text within the grid cells
#' @param col.grid color of the contours of the cells (default= NULL)
#' @param size.grid thickness of the contours of the cells (if col.grid is provided)
#' @param legend.pos position for the legend ("none", "right", "left", "bottom" (default), "top")
#' @param legend.dir: direction for the legend ("vertical", "horizontal" (default))
#' @param scale.gradient: a vector with min and max limits for the gradient values (default=c(0,100))
#'
#' @return Returns a dataframe with the relative influence of each predictor for the different models and plot a heatmap
#' @export
ggMultiInfluence<-function (...,col.gradient=c("white","lightblue4"),round=1,
                            col.text="grey10",col.grid=NULL,size.grid=0.3,legend.pos="bottom",
                            legend.dir="horizontal", scale.gradient=c(0,100)){

  BRT<-list(...)
  nBRT<-length(BRT)
  dfContr<-list()

  # Extract the relative influence of each predictor for each model
  for (i in 1:nBRT){
    dfContr[[i]]<-data.frame(BRT[[i]]$contributions)
    if(is.null(names(BRT))){
      colnames(dfContr[[i]])<-c("Predictor",paste("Model", i, sep = " "))}
    else{
      colnames(dfContr[[i]])<-c("Predictor",names(BRT)[i])}
  }

  # Merge them into a single dataframe
  Influence<-join_all(dfContr, by = 'Predictor')

  # Transform the data frame and round the values
  I.melted <- melt(Influence)
  I.melted$value<-round(I.melted$value,round)

  # Plot the heatmap
  p<-ggplot(I.melted, aes(x = variable, y = Predictor))+
    geom_tile(aes(fill=value),size=size.grid)+
    geom_text(aes(label=value,size=value), color=col.text,show.legend = FALSE)+
    scale_x_discrete(position = "top") +
    scale_fill_gradient(low=col.gradient[1],high =col.gradient[2],name="Relative influence (%)",
                        limits=scale.gradient)+
    labs(x= "",y = "")+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(face="bold"),
          legend.position=legend.pos,legend.direction=legend.dir)

  if(!is.null(col.grid)){
    p<-p+geom_tile(aes(fill=value),colour=col.grid,size=size.grid)+
      geom_text(aes(label=value,size=value), color=col.text,show.legend = FALSE)}

  print(p)
  print(Influence,digits = 3)
}
