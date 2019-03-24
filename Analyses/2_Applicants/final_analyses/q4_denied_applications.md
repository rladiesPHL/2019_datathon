PAWS Denied Applications Analysis
================
Veena
3/23/2019

-   [Load Packages](#load-packages)
-   [Load Data](#load-data)
-   [Explore Data](#explore-data)

1.  Describe an adoption application's trajectory at PAWS <br> 2.4 What are the applicant and animal characteristics that predict a denied application?

Load Packages
-------------

``` r
#load packages
library(readr)
library(dplyr)
library(tidyr)

library(tidyverse)
library(lubridate)
library(skimr)
library(gtools)
library(knitr)  #for outputting tables as kables
```

Load Data
---------

``` r
apps_cards <- readRDS("../appsCards.rds")
```

Explore Data
------------

``` r
dim(apps_cards)
```

    ## [1] 1631  203

``` r
is.data.frame(apps_cards)
```

    ## [1] TRUE

``` r
#summary(apps_cards)

#select(apps_cards, matches("denied")) #to find how many columns have some sort of indicator of the app being denied

dplyr::count(apps_cards, label.names_denied_ind) #how many applications were denied
```

|  label.names\_denied\_ind|     n|
|-------------------------:|-----:|
|                         0|  1600|
|                         1|    12|
|                        NA|    19|

``` r
#colnames(apps_cards)

df <- dplyr::filter(apps_cards, label.names_denied_ind == 1)

ggplot()
```

![](q4_denied_applications_files/figure-markdown_github/explore_data-1.png)
