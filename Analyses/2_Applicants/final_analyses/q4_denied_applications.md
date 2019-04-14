PAWS Denied Applications Analysis
================
Veena
3/23/2019

-   [Load Packages](#load-packages)
-   [Load Data](#load-data)
-   [Explore Data](#explore-data)
-   [Visualization and further analysis for applications that were denied](#visualization-and-further-analysis-for-applications-that-were-denied)
-   [Visualization and further analysis for applications that are red flagged](#visualization-and-further-analysis-for-applications-that-are-red-flagged)
-   [Denied and Red Flagged Applications<span style="color:brown"> - Veena </span>](#denied-and-red-flagged-applications---veena)

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

master_apps_report <- readRDS("../masterapps_20190324.rds") #final version I will use in the report
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
#dplyr::count(df_redflag, outcome_date) #129/133 did not result in an adoption or application is still in progress

#dplyr::count(df_redflag, label.names_denied_ind) #only two of the red flagged applications were denied but some can still be in progress

#dplyr::count(df_redflag, all_household_agree)

#dplyr::count(df_redflag, reason_for_adoption)

#Time difference between when they were adopted and application submission date
df_redflag2 <- select(df_redflag, label_names, animal_type)

df_redflag3 <- separate(df_redflag2, 'label_names', paste("label_name", 1:6, sep="_"), sep=",", extra="drop")
```

    ## Warning: Expected 6 pieces. Missing pieces filled with `NA` in 132 rows [1,
    ## 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
dplyr::count(df_redflag, last_label) #only two of the red flagged applications were denied but some can still be in 
```

| last\_label             |    n|
|:------------------------|----:|
| adopted                 |    1|
| adopted elsewhere       |    3|
| checks                  |    2|
| declaw only             |    1|
| denied                  |    2|
| manager decision        |   11|
| need info               |   11|
| need proof of ownership |    1|
| need to see id          |    2|
| need vet info           |    5|
| not utd                 |    2|
| opa                     |    2|
| pet policy              |    3|
| questions               |   12|
| ready for review        |   18|
| ready to adopt          |    3|
| red flag                |   40|
| rescue check            |    5|
| vet                     |    9|
| vet check in process    |    1|
| withdrawn               |    1|

``` r
ggplot(df_redflag, aes(x=last_label)) + 
  geom_bar(aes(fill=animal_type), width=.5) +
  ggtitle("Last Updated Label for Red Flagged Applications") +
  labs(x = "Last Updated Label",                                                            
       y= "Count",
       fill = "Animal Type") +
  coord_cartesian(ylim=c(0, 45)) +
  theme(axis.text.x = element_text(angle=80, vjust=0.5))
```

![](q4_denied_applications_files/figure-markdown_github/red%20flag%20visualization-1.png)

Denied and Red Flagged Applications<span style="color:brown"> - Veena </span>
-----------------------------------------------------------------------------

<br> We further investigated the characteristics of applications that were denied or red flagged. There were 12 applications that were denied, 19 that were withdrawn, and 133 that were red flagged. <br><br> **Denied Applications**<br> Below are visualizations that illustrate the applicants' characteristics (e.g. allergies, budget, home pet policy, etc.). We only have data for 12 denied applications so the analysis is limited. In the future when we have more data, we could compare the denied applications to the adopted ones.<br><br>

Key takeaways:<br> \* No known allergies for the applicants \* Budget had no impact (same budget range for approved applications) \* All household members agreed to get a pet \* Majority of the applicants did not enter a home pet policy and not everyone is the home owner \* Many applicants had unfortunate incidents with prior pets (e.g. ran away, died in care)

![](q4_denied_applications_files/figure-markdown_github/denied%20applications-1.png)![](q4_denied_applications_files/figure-markdown_github/denied%20applications-2.png)![](q4_denied_applications_files/figure-markdown_github/denied%20applications-3.png)![](q4_denied_applications_files/figure-markdown_github/denied%20applications-4.png)![](q4_denied_applications_files/figure-markdown_github/denied%20applications-5.png)![](q4_denied_applications_files/figure-markdown_github/denied%20applications-6.png) <br>**Red Flagged Applications**<br>

There were 133 applications that were red flagged.<br>129 of the 133 have not yet resulted in an adoption or are still being procesed. Two of the applications that were flagged were denied but that does not mean that the rest are going to result in adoption. Since the data set for the applications is from the end of 2018, many of the applications are still in progress. We do not have the final status of all the applications so we cannot conclude what happened to the red flagged applications. As a further project, I think it would be interesting to track the final status of the applications that were red flagged.

Below is a visualization that shows the last updated status for applications that were red flagged. After being flagged, the applications were sent to the manager to make a decision or the applicant was requested to provide more information (e.g. in many cases the applicant was required to provide more information about the vet).

![](q4_denied_applications_files/figure-markdown_github/red%20flag-1.png)
