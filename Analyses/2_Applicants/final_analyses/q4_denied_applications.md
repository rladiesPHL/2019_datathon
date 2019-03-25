PAWS Denied Applications Analysis
================
Veena
3/23/2019

-   [Load Packages](#load-packages)
-   [Load Data](#load-data)
-   [Explore Data](#explore-data)
-   [Visualization and further analysis for applications that were denied and withdrawn](#visualization-and-further-analysis-for-applications-that-were-denied-and-withdrawn)
-   [Visualization and further analysis for applications that are red flagged](#visualization-and-further-analysis-for-applications-that-are-red-flagged)

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
library(cowplot)
```

Load Data
---------

``` r
apps_cards <- readRDS("../appsCards.rds")
```

Explore Data
------------

``` r
#dim(apps_cards)
is.data.frame(apps_cards)
```

    ## [1] TRUE

``` r
#summary(apps_cards)

#select(apps_cards, matches("denied")) #label.names_denied_ind
#select(apps_cards, matches("withdraw")) #label.names_withdrawn_ind
#select(apps_cards, matches("red")) #label.names_red.flag_ind

dplyr::count(apps_cards, label.names_denied_ind) #how many applications were denied; 12 denied
```

|  label.names\_denied\_ind|     n|
|-------------------------:|-----:|
|                         0|  1595|
|                         1|    12|
|                        NA|    24|

``` r
dplyr::count(apps_cards, label.names_withdrawn_ind) #19 withdrawn
```

|  label.names\_withdrawn\_ind|     n|
|----------------------------:|-----:|
|                            0|  1588|
|                            1|    19|
|                           NA|    24|

``` r
dplyr::count(apps_cards, label.names_red.flag_ind) #133 red flagged
```

|  label.names\_red.flag\_ind|     n|
|---------------------------:|-----:|
|                           0|  1474|
|                           1|   133|
|                          NA|    24|

``` r
#colnames(apps_cards)

df_denied <- dplyr::filter(apps_cards, label.names_denied_ind == 1)
df_redflag <- dplyr::filter(apps_cards, label.names_red.flag_ind == 1)
#df_withdrawn <- dplyr::filter(apps_cards, label.names_withdrawn_ind == 1)
```

Visualization and further analysis for applications that were denied and withdrawn
----------------------------------------------------------------------------------

<br> There were 12 applications that were denied and 19 that were withdrawn. The analysis below shows the characteristics of the applications that were denied. Below are visualizations that show the budget, allergies, home owner, home pet policy, and experience breakdown for denied applications. <br>
<pre>
Key takeaways:
- No allergies for the denied application
- Budget has no impact on denied application
- Based on the denied applications, all household members agreed
- Majority did not enter the home pet policy and not everyone is the home owner</pre>
``` r
#budget, all household agree, allergies, homeowner, home pet policy (lot of NA), experience
ggplot(df_denied, aes(x=budget_monthly_ranges)) + 
  geom_bar(aes(fill=animal_type), width=.5) +
  ggtitle("Budget for Denied Applications") +
  labs(x = "Budget Range ($)",                                                            
       y= "Count",
       fill = "Animal Type") +
  coord_cartesian(ylim=c(0, 10))
```

![](q4_denied_applications_files/figure-markdown_github/denied%20and%20withdrawn-1.png)

``` r
ggplot(df_denied, aes(x=allergies)) + 
  geom_bar(aes(fill=animal_type), width=.5) +
  ggtitle("Allergy Status for Denied Applications") +
  labs(x = "Allergies",                                                            
       y= "Count",
       fill = "Animal Type") +
  coord_cartesian(ylim=c(0, 15))
```

![](q4_denied_applications_files/figure-markdown_github/denied%20and%20withdrawn-2.png)

``` r
ggplot(df_denied, aes(x=all_household_agree)) + 
  geom_bar(aes(fill=animal_type), width=.5) +
  ggtitle("Household Agrees Status for Denied Applications") +
  labs(x = "Household Agrees",                                                            
       y= "Count",
       fill = "Animal Type") +
  coord_cartesian(ylim=c(0, 15))
```

![](q4_denied_applications_files/figure-markdown_github/denied%20and%20withdrawn-3.png)

``` r
g1 <- ggplot(df_denied, aes(x=home_owner)) + 
  geom_bar(aes(fill=animal_type), width=.5) +
  labs(y= "Count",
       fill = "Animal Type") +
  coord_cartesian(ylim=c(0, 10)) +
  theme(axis.text.x = element_text(angle=70, vjust=0.5))

g2 <- ggplot(df_denied, aes(x=home_pet_policy)) + 
  geom_bar(aes(fill=animal_type), width=.5) +
  labs(fill = "Animal Type") +
  coord_cartesian(ylim=c(0, 10)) +
  theme(axis.text.x = element_text(angle=70, vjust=0.5))

theme_set(theme_cowplot(font_size=12))
plot_grid(g1, g2, align='h')
```

![](q4_denied_applications_files/figure-markdown_github/denied%20and%20withdrawn-4.png)

``` r
#trying to get all the experiences to show in one chart
df3 <- gather(df_denied, key="Experience", value="Count", "experience_currently.have.pet_ind":"experience_pet.ran.away_ind", -date_submitted)
```

Visualization and further analysis for applications that are red flagged
------------------------------------------------------------------------
