# ggBRT
Explore and visualise the results of boosted regression trees


## Overview


*ggBRT* is a set of R functions that uses ggplot2 to explore and visualize the results of boosted regression trees fitted with the gbm.step routine in the dismo package. 
The functions are built based on original codes from the dismo package (Hijmans et al. 2017) and additional sources (Leathwick/Elith 2007, Pinsky and Byler 2015). 
*gbm.bootstrap.functions*, *plot.gbm.4list* and *plot.gbm.boot* were originally written by Jane Elith and John Leathwick, but not released publicly. We thank them for providing this code.
The package is designed to facilitate the exploration and interpretation of the results. The use of ggplot2 and plotly for visualisation provides users high flexibility.


## Installation 


``` r
install.packages("devtools")
devtools::install_github("JBjouffray/ggBRT")
```
