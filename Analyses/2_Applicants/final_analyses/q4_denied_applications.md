PAWS Denied Applications Analysis
================
Veena
3/23/2019

-   [Load Packages](#load-packages)
-   [Load Data](#load-data)
-   [Explore Data](#explore-data)
-   [Visualization and further analysis for applications that were denied](#visualization-and-further-analysis-for-applications-that-were-denied)
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

masterData <- readRDS("../masterapps_20190324.rds")
```

Explore Data
------------

``` r
dim(apps_cards)
```

    ## [1] 1631  202

``` r
dim(masterData)
```

    ## [1] 1684  249

``` r
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
df_redflag <- dplyr::filter(masterData, label.names_red.flag_ind == 1)
#df_withdrawn <- dplyr::filter(apps_cards, label.names_withdrawn_ind == 1)
dim(df_denied)
```

    ## [1]  12 202

``` r
dim(df_redflag)
```

    ## [1] 135 249

Visualization and further analysis for applications that were denied
--------------------------------------------------------------------

<br> There were 12 applications that were denied and 19 that were withdrawn. The analysis below shows the characteristics of the applications that were denied. Below are visualizations that show the budget, allergies, home owner, home pet policy, and experience breakdown for denied applications. <br>
<pre>Key takeaways:
- No allergies for the denied application
- Budget has no impact on denied application, approved applications were similar
- All household members agreed
- Majority did not enter the home pet policy and not everyone is the home owner
- Many of the denied applications had unfortunate prior experiences
</pre>
``` r
#budget, all household agree, allergies, homeowner, home pet policy (lot of NA), experience
dplyr::count(df_denied, budget_monthly_ranges)
```

| budget\_monthly\_ranges |    n|
|:------------------------|----:|
| 101-200                 |    2|
| 201-500                 |    1|
| 26-100                  |    8|
| 501-1000                |    1|

``` r
df_denied$budget_monthly_ranges <- factor(df_denied$budget_monthly_ranges,levels = c("26-100", "101-200", "201-500", "501-1000"))

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
  theme(axis.text.x = element_text(angle=50, vjust=0.5))

g2 <- ggplot(df_denied, aes(x=home_pet_policy)) + 
  geom_bar(aes(fill=animal_type), width=.5) +
  labs(fill = "Animal Type") +
  coord_cartesian(ylim=c(0, 10)) +
  theme(axis.text.x = element_text(angle=50, vjust=0.5))

theme_set(theme_cowplot(font_size=12))
plot_grid(g1, g2, align='h')
```

![](q4_denied_applications_files/figure-markdown_github/denied%20and%20withdrawn-4.png)

``` r
#home along average; varies too much
ggplot(df_denied, aes(x=home_alone_avg)) + 
  geom_bar(aes(fill=animal_type), width=.5) +
  ggtitle("Home Along Avg for Denied Applications") +
  labs(x = "Hours",                                                            
       y= "Count",
       fill = "Animal Type") +
  coord_cartesian(ylim=c(0, 15))
```

    ## Warning: Removed 3 rows containing non-finite values (stat_count).

![](q4_denied_applications_files/figure-markdown_github/denied%20and%20withdrawn-5.png)

``` r
#trying to get all the experiences to show in one chart
#df_exp <- dplyr::count(df_denied, experience) #frequency of experiences for all denied applications
#df_exp[order(df_exp$n, decreasing=TRUE),]
experience_summary <- df_denied %>%
  select(starts_with("experience_")) %>% 
  summarise_all(sum, na.rm = TRUE) %>%
  gather() %>%
  mutate(cleaned_col = str_replace(key, "experience_", ""),
         cleaned_col = str_replace(cleaned_col, "_ind", "")) %>%
  select(cleaned_col, value)

ggplot(experience_summary, aes(x= fct_reorder(cleaned_col, value, .desc=TRUE), y=value)) + 
  geom_bar(stat = "identity", fill="#78aac3") +
  theme(axis.text.x = element_text(angle=50, vjust=0.5)) +
  guides(fill=FALSE) +
  ggtitle("Prior Experiences for Denied Applications") +
  labs(x = "Experience", y= "Count")
```

![](q4_denied_applications_files/figure-markdown_github/denied%20and%20withdrawn-6.png)

Visualization and further analysis for applications that are red flagged
------------------------------------------------------------------------

``` r
#df_redflag$label_names

#For the applications that are red flgged, how many end up becoming adopted
dplyr::count(df_redflag, outcome_date) #129/133 did not result in an adoption or application is still in progress
```

| outcome\_date       |    n|
|:--------------------|----:|
| 2018-10-06 12:58:00 |    1|
| 2018-11-06 17:15:00 |    1|
| 2018-11-30 18:17:00 |    1|
| 2018-12-18 15:31:00 |    2|
| 2019-01-17 17:39:00 |    1|
| NA                  |  129|

``` r
dplyr::count(df_redflag, label.names_denied_ind) #only two of the red flagged applications were denied but some can still be in progress
```

|  label.names\_denied\_ind|    n|
|-------------------------:|----:|
|                         0|  133|
|                         1|    2|

``` r
dplyr::count(df_redflag, all_household_agree)
```

| all\_household\_agree |    n|
|:----------------------|----:|
| a-surprise            |    1|
| yes                   |  131|
| yes,a-surprise        |    3|

``` r
dplyr::count(df_redflag, reason_for_adoption)
```

| reason\_for\_adoption       |    n|
|:----------------------------|----:|
| NA                          |    2|
| gift                        |    5|
| mouser                      |    5|
| mouser,my-kids,myself       |    1|
| mouser,other                |    1|
| my-kids                     |    9|
| my-kids,gift                |    1|
| my-kids,mouser              |    2|
| my-kids,myself              |    3|
| my-kids,myself,mouser       |    1|
| my-kids,myself,other        |    1|
| my-kids,protection,myself   |    1|
| myself                      |   60|
| myself,mouser               |   11|
| myself,mouser,gift          |    1|
| myself,mouser,my-kids       |    1|
| myself,my-kids              |   17|
| myself,my-kids,mouser       |    7|
| myself,my-kids,mouser,gift  |    1|
| myself,my-kids,mouser,other |    1|
| other                       |    4|

``` r
#Time difference between when they were adopted and application submission date
```
