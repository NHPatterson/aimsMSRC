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
install_github('outrepo')
```

## What's next?

* Examine the tutorial [scripts](https://github.com/) and [R markdown](https://github.com) files for workflow examples


