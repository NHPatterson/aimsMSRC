# Mass Spectrometry Research Center AIMS.2018 R package

This is the R package accompanying the AIMS R data analysis course, it will give uses a very brief introduction to the open-source Cardinal in R for basic data pre-processing and analysis tasks and includes some MSRC in-house developed functions for data analysis.

## Installation

Please download and install R followed by RStudio.

1. [Download and install latest version of R for your platform](https://mirrors.nics.utk.edu/cran/)

2. [Download and install latest version of RStudio Desktop for your platform](https://www.rstudio.com/products/rstudio/download/)



## Getting started for the first time
*Open RStudio and run the following lines in the R Console to install the necessary packages:*

```r
##Cardinal must be installed before any other steps!!
source("http://bioconductor.org/biocLite.R")
biocLite("Cardinal")

##install R package devtools to install from github
install.packages('devtools')

##install the aimsMSRC package
require(devtools)
install_github('nhpatterson/aimsMSRC')
```

## What's next?

* Examine the tutorials:
 
    * [Data reading and pre-processing to peaks, demo only, raw data not included due to size](https://htmlpreview.github.io/?https://github.com/NHPatterson/aimsMSRC/blob/master/markdown/aimsMSRC_data_prepro.html),  [R Markdown](https://github.com/NHPatterson/aimsMSRC/blob/master/markdown/aimsMSRC_data_prepro.Rmd)
    * [Comparse two user selected ROIs using ROC analysis](https://htmlpreview.github.io/?https://github.com/NHPatterson/aimsMSRC/blob/master/markdown/aimsMSRC_ROI_comparison.html),  [R Markdown](https://github.com/NHPatterson/aimsMSRC/blob/master/markdown/aimsMSRC_ROI_comparison.Rmd)
    * [Exploratory multivariate analysis, PCA and clustering](https://htmlpreview.github.io/?https://github.com/NHPatterson/aimsMSRC/blob/master/markdown/aimsMSRC_multivariate.html),  [R Markdown](https://github.com/NHPatterson/aimsMSRC/blob/master/markdown/aimsMSRC_multivariate.Rmd)
    * [Export peak intensity tables into .csv](https://htmlpreview.github.io/?https://github.com/NHPatterson/aimsMSRC/blob/master/markdown/aimsMSRC_exporting_data.html),  [R Markdown](https://github.com/NHPatterson/aimsMSRC/blob/master/markdown/aimsMSRC_exporting_data.Rmd)
    * [Convert & export data for data-driven image fusion](https://htmlpreview.github.io/?https://github.com/NHPatterson/aimsMSRC/blob/master/markdown/aimsMSRC_fusion_example.html),  [R Markdown](https://github.com/NHPatterson/aimsMSRC/blob/master/markdown/aimsMSRC_fusion_example.Rmd)


