#' ggInteract_boot_hist
#'
#'Function to plot a distribution histogram with confidence intervals
#'
#' @param data a dataframe that contains the values
#' @param column which column to plot
#' @param cis a vector to specify the confidence intervals (default=c(0.025, 0.975))
#' @param bindwidth bindwidth of the barplot (default = 1)
#' @param obs the observed value to compare to the distribution and confidence intervals
#' @export
ggInteract_boot_hist<-function(data=NULL,column=NULL,cis=c(0.025, 0.975),
                               bindwidth=1,obs=NULL){

  if (is.null(obs)) {
    stop("you need to specify the observed interaction size with the argument obs=")}

  # Plot distribution of interaction size and the 95% CIs
  qt_low<-quantile(data[[column]],cis[1])
  qt_high<-quantile(data[[column]],cis[2])

  p<-ggplot(data, aes(x=data[[column]])) +
    geom_histogram(fill="grey",colour="darkgrey",binwidth = bindwidth) +
    geom_vline(xintercept = qt_low, colour="red",lty=2)+
    geom_vline(xintercept = qt_high, colour="red",lty=2)+
    geom_vline(xintercept = obs, colour="green")+
    xlab("Interaction size")+
    theme_bw()+
    theme(panel.grid=element_blank())
  p
}
