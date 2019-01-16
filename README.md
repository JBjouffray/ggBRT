# ggBRT
Explore and visualise the results of boosted regression trees

**Author:** Jean-Baptiste Jouffray (2019)

**Correspondence:** jean-baptiste.jouffray@su.se

## Overview

[ggBRT](https://github.com/JBjouffray/ggBRT) contains a set of R functions that use [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) (Wickham 2016) to explore and visualize the results of boosted regression trees fitted with the gbm.step routine (Elith et al 2008) in the [dismo](https://cran.r-project.org/web/packages/dismo/index.html) package (Hijmans et al. 2017). The package is designed to facilitate the exploration and interpretation of the results while allowing great flexibility in visualisation. 

The functions are largely based on codes from the [dismo](https://cran.r-project.org/web/packages/dismo/index.html) package (Hijmans et al. 2017) as well as additional sources (e.g., Pinsky and Byler 2015). The functions *plot.gbm.4list*, *gbm.bootstrap.functions* and *plot.gbm.boot* were originally written by Jane Elith and John Leathwick, but not released publicly. We thank them for providing the codes.

[ggBRT](https://github.com/JBjouffray/ggBRT) requires the following R packages: [directlabels](https://cran.r-project.org/web/packages/directlabels/index.html) (Hocking 2017), [dismo](https://cran.r-project.org/web/packages/dismo/index.html) (Hijmans et al. 2017), [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html) (Wickham and François 2016), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) (Wickham 2016), [ggthemes](https://cran.r-project.org/web/packages/ggthemes/index.html) (Arnold 2017), [gridExtra](https://cran.r-project.org/web/packages/gridExtra/index.html) (Auguie 2016), [plotly](https://cran.r-project.org/web/packages/plotly/index.html) (Sievert et al. 2017), [plyr](https://cran.r-project.org/web/packages/plyr/index.html) (Wickham 2011) and [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html) (Wickham 2007).


## Installation 

``` r
install.packages("devtools") # in case "devtools" has not already been installed
devtools::install_github("JBjouffray/ggBRT") # will take several minutes to install
```

## Usage
[ggBRT](https://github.com/JBjouffray/ggBRT) is designed for use with object of S3 class gbm, obtained from the gbm.step routine in the [dismo](https://cran.r-project.org/web/packages/dismo/index.html) package. 

Examples for each function using data from Jouffray et al. (2019) are found in this [vignette](https://jbjouffray.github.io/ggBRT/ggBRT_Tutorial.html).


## Citation
The package [ggBRT](https://github.com/JBjouffray/ggBRT) originally accompanied: 

Jouffray J-B, Wedding L.M., Norstrom A.V., Donovan M.K., Williams G.J., Crowder L.B., Erickson A.L., Friedlander A.M., Graham N.A.J., Gove J.M., Kappel C.V., Kittinger J.N., Lecky J., Oleson K.L.L., Selkoe K.A., White C., Williams I.D., Nystrom M. 2019. Parsing Human and Biophysical Drivers of Coral Reef Regimes. _Proc. R. Soc. B._


## References

Arnold J. B. (2017). ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version3.4.0. https://CRAN.R-project.org/package=ggthemes

Auguie B. (2016). gridExtra: Miscellaneous Functions for “Grid" Graphics. R package version 2.2.1. https://CRAN.R-project.org/package=gridExtra

Elith, J., Leathwick, J. R., & Hastie, T. (2008). A working guide to boosted regression trees. Journal of Animal Ecology, 77(4), 802-813.

Hijmans R.J., Phillips S., Leathwick J.R. and Elith J. (2017). dismo: Species Distribution Modeling. R package version 1.1-4. https://CRAN.R-project.org/package=dismo 

Hocking T. (2017). directlabels: Direct Labels for Multicolor Plots. R package version 2017.01.03. http://directlabels.r-forge.r-project.org/

Jouffray JB, Wedding L.M., Norstrom A.V., Donovan M.K., Williams G.J., Crowder L.B., Erickson A.L., Friedlander A.M., Graham N.A.J., Gove J.M., Kappel C.V., Kittinger J.N., Lecky J., Oleson K.L.L., Selkoe K.A., White C., Williams I.D., Nystrom M. (2019). Parsing Human and Biophysical Drivers of Coral Reef Regimes. Proc. R. Soc. B.

Pinsky, M. L., & Byler, D. (2015). Fishing, fast growth and climate variability increase the risk of collapse. In Proc. R. Soc. B (Vol. 282, No. 1813, p. 20151053). The Royal Society.

Sievert C., Parmer C., Hocking T., Chamberlain S., Ram K., Corvellec M. and Despouy P. (2017). plotly: Create Interactive Web Graphics via 'plotly.js'.
https://plot.ly/r, https://cpsievert.github.io/plotly_book/

Wickham H. (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/.

Wickham H. (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software,40(1),1-29.URL http://www.jstatsoft.org/v40/i01/.

Wickham H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. 

Wickham H. and Francois R. (2016). dplyr: A Grammar of Data Manipulation. R package version0.5.0. https://CRAN.R-project.org/package=dplyr

