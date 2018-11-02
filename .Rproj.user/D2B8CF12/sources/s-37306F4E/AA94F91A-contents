---
title: "README"
output: html_document
---

# Introduction 

This package helps the user to estimate the natural rate of interest based on the Holsten-Laubach-Williams (2017) technique. It does this with the help of support functions that automatically downloading the newest data from the St.Louis Fed website.

Current countries that have support functions:
*United States
*South Africa

More countries to be added soon. You are welcome to generate your own dataset, but be aware of the transformations used in the dataset generating processes here. They are essential for the estimation procedure to run.

This package is still under development, so play nice.


# Installation

```{r}
if (!require("devtools")) install.packages("devtools")
library(devtools)
devtools::install_github("JannesRed/rStar")
library(rStar)
```

This will automatically install all dependencies required. 

Should you not already have a version of mFilter installed, then you might have to manually install it. This can be done as follows:
* download from this link: https://cran.r-project.org/src/contrib/Archive/mFilter/
* and install as per below (adjusting the filepath as necessary):

```{r}
install.packages("C:/Users/xxx/xxx/mFilter_0.1-3.tar.gz", repos = NULL, type = "source")
```
## Dependencies

*  lubridate
*  mFilter
*  nloptr
*  seasonal
*  stats
*  tidyverse
*  tis
*  tools
*  utils

# Functions

It is always recommended to work in a R Project. 

These are the functions that are currently available: 

## Function: get.US.data or get.SA.data

These are simple, just specify the folder and file path for output. They have sensible defaults for when you are working in a project - just make sure to create a folder called "inputData" when using the defaults. 

For the US there is an additional option to specify alternate output measures. The user has the option to chose between real GDP and real GDP per capita. Unfortunately, South Africa does not publish quarterly per capita GDP.

### defaults

```{r}
#for US
folder = "data/input/"
file ="rstar.data.us.csv" 
pcGDP = FALSE

#for SA
folder = "data/input/"
file ="rstar.data.sa.csv"
```

### Example 1

```{r}
# for South Africa
folder = "data/input/"
file ="rstar.data.sa.csv"
get.SA.data(folder = folder, file = file)

# for US
folder = "data/input/"
file ="rstar.data.us.csv" 
get.US.data(folder = folder, file = file, pcGDP = FALSE)
```

### Example 2

```{r}
# when using default folder, file, and output measure
# just make sure you have a "data/inputData/" folder structure from 
# your project root if using this approach
get.US.data()
get.SA.data()

```


## Function: hlw_nri

The user has to manually specify the input file, country name, and output folder. 

run.se and niter are optional. defaults are TRUE and 5000, respectively.

run.se takes either TRUE or FALSE as arguments. If TRUE, then standard errors for the estimation are calculated. default is set to TRUE. Set to FALSE if you want to speed up the estimation procedure.

niter sets the number of iterations for Monte Carlo Simulation. The default is 5000, and should not be changed unless the user really understands the source code.

### Example 1

```{r}
# run without standard error estimation and 5000 iterations for Monte Carlo simulation
# running standard errors is computationally intensive

input_file <-  "data/input/rstar.data.us.test.csv"  # location of input file
country_name <-  "US"                               #country name for output file
output_folder <-  "data/output/"                    # output folder for estimation
run.se <-  FALSE                                    # turn on standard errors
niter <-5000                                        #iterations for monte carlo simulation

hlw_nri(input_file = input_file,
        country_name = country_name,
        output_folder = output_folder,
        run.se = run.se,
        niter = niter)
```

### Example 2

```{r}
# run with defaults for standard error estimation and 5000 iterations for Monte Carlo simulation
# this is runs substantially faster than example 1
input_file <-  "data/input/rstar.data.us.test.csv"  # location of input file
country_name <-  "US"                               #country name for output file
output_folder <-  "data/output/"                    # output folder for estimation


hlw_nri(input_file = input_file,
        country_name = country_name,
        output_folder = output_folder)
```

### Example 3

```{r}
# same as example 2, but the short way

hlw_nri(input_file = "data/input/rstar.data.us.test.csv",
        country_name = "US",
        output_folder = "data/output/")
```
