PAWS Application Timeline Analysis
================
Kate Connolly
3/24/2019

-   [Libraries](#libraries)
-   [Load Data](#load-data)
-   [Adoption Timeline Analysis](#adoption-timeline-analysis)
-   [Checklist Timeline Analysis](#checklist-timeline-analysis)

Libraries
---------

``` r
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)
library(data.table)
library(formattable)
library(knitr)
```

Load Data
---------

``` r
masterapps_20190324 <- readRDS("/Users/connolk/Downloads/masterapps_20190324.rds")
```

<br />

Adoption Timeline Analysis
--------------------------

``` r
# add adoption_time column for the difference between the date_submitted & outcome_date
masterapps_20190324 <- masterapps_20190324 %>%
  mutate(adoption_time = difftime(outcome_date, date_submitted, units = "days"),
         adoption_time = round(as.numeric(adoption_time), 2))
```

<br />

**Timeline by Animal**

``` r
# boxplot of adoption_time by animal
masterapps_20190324 %>%
  filter(!adoption_time < 0) %>%                                                        # remove negative values in adoption_time column
  
  ggplot(aes(x = animal_type, y = adoption_time, fill = animal_type)) +                 # break out checklist_item by cat & dog
  geom_boxplot(alpha = 0.4, outlier.alpha = 0.1) +                                      # make outliers and boxes more transparent
  scale_y_continuous(breaks = seq(0, 140, by=20)) +                                     # set y axis tick intervals at 20
  theme_light() + 
  ggtitle("Adoption Timeline by Animal") +                                              # set plot title
  labs(x = NULL,                                                                        # set plot labels
       y= "days between app submission & adoption",
       fill = "animal type") +
  theme(plot.title = element_text(hjust = 0.5,                                          # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),        
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 5, l = 0)),      # x axis title formatting (padding)
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5))) +    # y axis title formatting (padding)
  coord_flip()                                                                          # flip the x and y axes
```

![](q1_timeline_files/figure-markdown_github/adoption%20animal%20boxplot-1.png)

<br />

``` r
# related table 

# isolate the data that is related to animal type
kable(
      masterapps_20190324 %>%
        drop_na(outcome_sitename) %>%                                   # drop any results with no outcome_site
        filter(!adoption_time < 0) %>%                                  # remove negative values in adoption_time column
        group_by(animal_type) %>%                                       # before calculations, group data by outcome site
        summarize(mean(adoption_time),                                  # calculate mean, use summarize to collapse each site into single-row summary 
                  median(adoption_time)) %>%                            # calculate median, use summarize to collapse each site into single-row summary 
        rename("animal type" = "animal_type",                           # rename the df columns to be more readable 
               "mean adoption time" = "mean(adoption_time)",
               "median adoption time" = "median(adoption_time)")
)
```

| animal type |  mean adoption time|  median adoption time|
|:------------|-------------------:|---------------------:|
| cat         |            24.18434|                 18.80|
| dog         |            17.21027|                  7.86|

<br />

Can see that, in general, cat applications typically take longer than dog applications.

The boxplot also illuminates that many cat applications take significantly longer than dog ones—the upper quartile of cat applications take between about 35 days to 70 days, compared to about 18 days to 40 days for dogs.

<br /> <br />

**Timeline by Animal & Site**

I assumed that the outcome\_sitename (or the location from where animal left PAWS) would get at the "PAWS location" aspect in question 1. I'm not totally sure that the outcome site is where most of the application process occurred, but it's what I'm using for "location" in this analysis.

Here are some visualizations that I tossed together.

``` r
# boxplot of adoption_time by animal & adoption location
masterapps_20190324 %>%
  drop_na(outcome_sitename) %>%                                                         # one id with no adoption site, drop that id
  filter(!adoption_time < 0) %>%                                                        # remove negative values in adoption_time column
  
  ggplot(aes(x = outcome_sitename, y = adoption_time, fill = animal_type)) +
  geom_boxplot(alpha = 0.4, outlier.alpha = 0.1) +                                      # break out checklist_item by cat & dog
  scale_y_continuous(breaks = seq(0, 200, by=20)) +                                     # set y axis tick intervals at 2
  theme_light() + 
  ggtitle("Day Count Distribution by Checklist Item") +                                 # set plot title
  labs(x = "adoption site",                                                             # set plot labels
       y= "days between app submission & adoption",
       fill = "animal type") +
  theme(plot.title = element_text(hjust = 0.5,                                          # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),        
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 5, l = 0)),      # x axis title formatting (padding)
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5))) +    # y axis title formatting (padding)
  coord_flip()                                                                          # flip the x and y axes
```

![](q1_timeline_files/figure-markdown_github/adoption%20animal%20&%20site%20boxplot-1.png)

<br />

``` r
# related table 

# isolate the data that is related to outcome site
kable(
      masterapps_20190324 %>%
        drop_na(outcome_sitename) %>%                                   # one id with no adoption site, drop that id
        filter(!adoption_time < 0) %>%                                  # remove negative values in adoption_time column
        group_by(outcome_sitename) %>%                                  # before calculations, group data by outcome site
        summarize(mean(adoption_time),                                  # calculate mean, use summarize to collapse each site into single-row summary 
                  median(adoption_time)) %>%                            # calculate median, use summarize to collapse each site into single-row summary 
        rename("outcome site" = "outcome_sitename",                     # rename the df columns to be more readable 
               "mean adoption time" = "mean(adoption_time)",
               "median adoption time" = "median(adoption_time)")
)
```

| outcome site           |  mean adoption time|  median adoption time|
|:-----------------------|-------------------:|---------------------:|
| Grant Avenue           |            20.41333|                 8.770|
| Grays Ferry Avenue     |            15.94632|                12.710|
| PAC                    |            14.08069|                 7.760|
| PAWS Foster Program    |            27.77663|                24.120|
| PAWS Offsite Adoptions |            27.06545|                24.335|

``` r
# related table 

# isolate the data that is related to both outcome site & animal type
site_animal_df <- masterapps_20190324 %>%
  drop_na(outcome_sitename) %>%                                   # one id with no adoption site, drop that id
  filter(!adoption_time < 0) %>%                                  # remove negative values in adoption_time column
  group_by(outcome_sitename, animal_type) %>%                     # before calculations, group data by outcome site
  summarize(mean = mean(adoption_time),                           # calculate mean, use summarize to collapse each site into single-row summary 
            median = median(adoption_time)) %>%                   # calculate median, use summarize to collapse each site into single-row summary 
  mutate_at(vars(mean, median), funs(round(., 2))) %>%            # round the calcs to 2 decimal places
  rename("animal" = "animal_type",                                # rename the df columns to be more readable 
         "outcome site" = "outcome_sitename",                     # can't utilize underscores or formatting gets weird
         "mean adoption time" = "mean",
         "median adoption time" = "median") 
```

``` r
formattable(site_animal_df, align =c("l","l","c","c"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `mean adoption time`= color_tile("#FFEFDB", "#FF8000"),
  `median adoption time`= color_tile("#FFEFDB", "#FF8000")))
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
outcome site
</th>
<th style="text-align:left;">
animal
</th>
<th style="text-align:center;">
mean adoption time
</th>
<th style="text-align:center;">
median adoption time
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Grant Avenue
</td>
<td style="text-align:left;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffb468">18.93</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffcd98">10.25</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
Grant Avenue
</td>
<td style="text-align:left;">
dog
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8e1d">26.21</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe4c5">5.98</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
Grays Ferry Avenue
</td>
<td style="text-align:left;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffefdb">7.84</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffdab2">7.84</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
Grays Ferry Avenue
</td>
<td style="text-align:left;">
dog
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffc282">16.40</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffc07e">12.77</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
PAC
</td>
<td style="text-align:left;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffc78d">15.36</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffd9b0">7.96</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
PAC
</td>
<td style="text-align:left;">
dog
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe9d1">8.80</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffefdb">3.93</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
PAWS Foster Program
</td>
<td style="text-align:left;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8000">29.02</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8000">24.88</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
PAWS Foster Program
</td>
<td style="text-align:left;">
dog
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffc283">16.32</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffc488">11.86</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
PAWS Offsite Adoptions
</td>
<td style="text-align:left;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8912">27.19</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8001">24.78</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
PAWS Offsite Adoptions
</td>
<td style="text-align:left;">
dog
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffa64c">21.64</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff9121">21.64</span>
</td>
</tr>
</tbody>
</table>
<br />

``` r
# rename site_animaL_df column names to include underscores
site_animal_df <- site_animal_df %>%
  rename("animal_type" = "animal",                                # couldn't use underscores earlier due to formatting of table, need them now to do other anlysis
         "outcome_site" = "outcome site",                     
         "mean_adoption_time" = "mean adoption time",
         "median_adoption_time" = "median adoption time") 
```

``` r
# heatmap plot of median adoption time by animal & adoption site
site_animal_df %>%
  ggplot(aes(animal_type, outcome_site)) + 
  geom_tile(aes(fill = median_adoption_time),                                            # set tiles to be median adoption time
            color = "white") + 
  scale_fill_gradient(low = "aliceblue",                                                     # set tile gradient colors
                      high = "steelblue") +
  theme_light() +
  labs(x = NULL,                                                                         # set plot labels
       y = NULL) +
  ggtitle("Median Adoption Time Heatmap") +                                              # set plot title
  scale_x_discrete(expand = c(0, 0)) +                                                   # visual editing, used to expand tiles to entire plot area on both axes
  scale_y_discrete(expand = c(0, 0)) +                                
  theme(legend.position = "none",                                                        # remove legend
        axis.ticks.x = element_blank(),                                                  # remove tick marks on both axies
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5,                                           # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)))
```

![](q1_timeline_files/figure-markdown_github/outcome%20site%20&%20animal%20heatmap-1.png)

<br />

Overall, median adoption times were higher at PAWS Foster Program & PAWS Offsite Adoptions locations. This is especially true for cat applications at those locations.

Based on median values, here are the fastest & slowest time-to-adoption sites:

-   **Cats**
    -   Slowest: PAWS Foster Program
    -   Fastest: Grays Ferry Avenue
-   **Dogs**
    -   Slowest: PAWS Foster Program
    -   Fastest: PAC

<br />

``` r
# frequency polygon to compare distribution by adoption site 
masterapps_20190324 %>%
  drop_na(outcome_sitename) %>%                                                         # one id with no adoption site, drop that id
  filter(!adoption_time < 0) %>%                                                        # remove negative values in adoption_time column
  
  ggplot(aes(adoption_time, color = outcome_sitename)) +
  geom_freqpoly(bins = 15) +
  theme_light() +
  labs(x = "days between app submission & adoption",                                    # set plot labels
       y = "count of applications") + 
  ggtitle("Days Until Adoption Distribution by Adoption Site") +                        # set plot title
  theme(plot.title = element_text(hjust = 0.5, face="bold"),                            # title formatting (center, bold)
        legend.title = element_blank(),                                                 # leave legend title blank
        panel.grid.minor.y = element_blank(),        
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 5, l = 0)),      # x axis title formatting (padding)
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5)))      # y axis title formatting (padding)
```

![](q1_timeline_files/figure-markdown_github/frequency%20poly%20site-1.png)

<br /> <br />

Checklist Timeline Analysis
---------------------------

I isolated the data to just applications that resulted in adoption.

``` r
# days distribution boxplot, by checklist_item & animal
masterapps_20190324 %>%
  filter(outcome_type == "Adoption") %>%
  gather(checklist_item, value, checklist_ACCT:checklist_VET) %>%                        # flatten checklist rows into one column (called "checklist_item") and corresponsding values into one column (called "values")

  ggplot(aes(x = checklist_item, y = value, fill = animal_type)) +
  geom_boxplot(alpha = 0.4, outlier.alpha = 0.1) +                                       # break out checklist_item by cat & dog
  theme_light() + 
  ggtitle("Day Count Distribution by Checklist Item") +
  labs(x = "checklist item",                                                             # set plot labels
       y= "days from last checklist item",
       fill = "animal type") +
  theme(plot.title = element_text(hjust = 0.5,                                           # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),        
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 5, l = 0)),       # x axis title formatting (padding)
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5))) +     # y axis title formatting (padding)
  coord_flip()                                                                           # flip the x and y axes
```

![](q1_timeline_files/figure-markdown_github/checklist%20days%20boxplot%20outliers-1.png)

<br />

The outliers make it a little difficult to analyze this plot, so I made it again with fewer outliers.

<br />

``` r
#  REMOVED SOME OUTLIERS; days distribution boxplot, by checklist_item & animal
masterapps_20190324 %>%
  filter(outcome_type == "Adoption") %>%
  gather(checklist_item, value, checklist_ACCT:checklist_VET) %>%                       # flatten checklist rows into one column (called "checklist_item") and corresponsding values into one column (called "values")
  
  ggplot(aes(x = checklist_item, y = value)) +
  geom_boxplot(aes(fill = animal_type), alpha = 0.4, outlier.alpha = 0.1) +             # break out checklist_item by cat & dog
  scale_y_continuous(breaks = seq(0, 20, by=2),                                         # set y axis tick intervals at 2
                     limits=c(0, 20)) +                                                 # set y limit to 16 to "remove" highest outliers & see plots better 
  theme_light() + 
  ggtitle("Day Count Distribution by Checklist Item (Fewer Outliers)") +
  labs(x = "checklist item",
       y= "days from last checklist item",
       fill = "animal type") +
  theme(plot.title = element_text(hjust = 0.5,                                          # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),        
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 5, l = 0)),      # x axis title formatting (padding)
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5))) +    # y axis title formatting (padding)
  coord_flip()                                                                          # flip the x and y axes
```

![](q1_timeline_files/figure-markdown_github/checklist%20days%20boxplot-1.png)

<br />

Interesting to note that there was only 1 checklist\_ACCT item, and only cat applications involved checklist\_SPCA items. It's also interesting that I even had to take measures to remove some of the outliers—there are many of them. I wonder what potential labels may lead to certain ones taking longer to complete than others. I didn't get to explore that here, but I think that would be a valuable follow-up analysis. Becasue while the median time for checklist items to be completed is quite low for each, there are certainly enough outliers to wonder what has delayed those applications.

I also thought it might be more valuable to visualize this information in another way.

<br />

``` r
# isolate the data that is related to checklist items for adoptions
checklist_calcs <- masterapps_20190324 %>%
  filter(outcome_type == "Adoption") %>%
  gather(checklist_item, value, checklist_ACCT:checklist_VET) %>%
  drop_na(value) %>%
  group_by(checklist_item) %>%
  summarize(mean = mean(value),                                         # calculate mean, use summarize to collapse each site into single-row summary 
            median = median(value)) %>%                                 # calculate median, use summarize to collapse each site into single-row summary 
  mutate_at(vars(mean, median), funs(round(., 2))) %>%                  # round calcs to 2 decimal places
  rename("mean days from last item" = "mean",                           # rename the df columns to be more readable
         "median days from last item" = "median") 

# get the count of each checklist item occurrence
checklist_count <- masterapps_20190324 %>%
  filter(outcome_type == "Adoption") %>%
  gather(checklist_item, value, checklist_ACCT:checklist_VET) %>%
  drop_na(value) %>%
  group_by(checklist_item) %>%
  count(checklist_item)

# combine the calculations with the n for each checklist_item
checklist_df <- merge(checklist_count, checklist_calcs, by = "checklist_item", all.x = TRUE) %>%
  mutate(item_percent = percent(n/453, 1)) %>%                                                           # calculate the percent of applications that had each item checked off
  rename("percent of cards with item checked" = "item_percent",                                          # rename the df columns to be more readable
         "checklist item" = "checklist_item")
```

``` r
# put the table into formattable
formattable(checklist_df, align =c("l","c","c","c"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
  `mean days from last item`= color_tile("#FFEFDB", "#FF8000"),
  `median days from last item`= color_tile("#FFEFDB", "#FF8000")))
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
checklist item
</th>
<th style="text-align:center;">
n
</th>
<th style="text-align:center;">
mean days from last item
</th>
<th style="text-align:center;">
median days from last item
</th>
<th style="text-align:left;">
percent of cards with item checked
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
checklist\_ACCT
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8000">10.89</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8000">10.89</span>
</td>
<td style="text-align:left;">
0.2%
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_CHQ
</td>
<td style="text-align:center;">
432
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffeeda">2.11</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffeeda">0.97</span>
</td>
<td style="text-align:left;">
95.4%
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_LL
</td>
<td style="text-align:center;">
433
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe8cf">2.56</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffeed9">1.03</span>
</td>
<td style="text-align:left;">
95.6%
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_PP
</td>
<td style="text-align:center;">
433
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffecd6">2.26</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffeed9">1.03</span>
</td>
<td style="text-align:left;">
95.6%
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_SPCA
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffbc77">6.07</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffb56a">6.07</span>
</td>
<td style="text-align:left;">
0.4%
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_TR
</td>
<td style="text-align:center;">
435
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffefdb">2.08</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffefdb">0.95</span>
</td>
<td style="text-align:left;">
96.0%
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_VET
</td>
<td style="text-align:center;">
425
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffdfbd">3.28</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe5c8">1.80</span>
</td>
<td style="text-align:left;">
93.8%
</td>
</tr>
</tbody>
</table>
<br />

With this table we can see the summary calcs for each checklist item a little more clearly. I also added an "n" column with the number of applications that had this item checked off (had a day since last item value present). This is helpful in understanding how the n size may affect mean/median values.

One main takeaway is that checklist\_ACCT & checklist\_SPCA are often not checked off; they are checked in less than 1% of adoption applications. These items correspond to:

-   *checklist\_ACCT*: Check with ACCT (Animal Care and Control Team)
-   *checklist\_SPCA*: Check with SPCA (Society for the Prevention of Cruelty to Animals)

Why are these items typically left unchecked? Are they not always necessary for an application (it would seem not)? Our very very small n suggests that these items take longer than the others—is that why they are not required items? Are there other components of an application, like red flags or animal information, that would lead to this item needing to be completed? I would assume so, based on the organizations that these item's involve.

Can also see in the visualization below that those two checklist items were only completed at the PAC site. Why is that? In any case, it's unfrotunately too difficult to extrpolate on these two checklist items since they occurred so infrequently.

Another point to note is that no checklist item was featured in 100% of adoption applications.

``` r
# heatmap plot of mean adoption time by animal & adoption site

# first isolate the data that is related to checklist items for adoptions
masterapps_20190324 %>%
  filter(outcome_type == "Adoption") %>%
  gather(checklist_item, value, checklist_ACCT:checklist_VET) %>%
  drop_na(value) %>%
  group_by(checklist_item, outcome_sitename) %>%
  summarize(mean(value),
            median(value)) %>%
  rename("mean_days_from_last_item" = "mean(value)",                                     # rename the df columns to be more readable
         "median_days_from_last_item" = "median(value)") %>%
  drop_na(outcome_sitename) %>%                                                          
  
  ggplot(aes(outcome_sitename, checklist_item)) + 
  geom_tile(aes(fill = median_days_from_last_item),                                      # set tiles to be mean adoption time
            color = "white") + 
  scale_fill_gradient(low = "aliceblue",                                                 # set tile gradient colors
                      high = "steelblue") +
  theme_light() +
  labs(x = NULL,                                                                         # set plot labels
       y = NULL) +
  ggtitle("Median Days to Checklist Item Heatmap (by Outcome Site)") +                   # set plot title
  scale_x_discrete(expand = c(0, 0)) +                                                   # visual editing, used to expand tiles to entire plot area on both axes
  scale_y_discrete(expand = c(0, 0)) +                                
  theme(legend.position = "none",                                                        # remove legend
        axis.ticks.x = element_blank(),                                                  # remove tick marks on both axies
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),                                              # remove plot gridlines
        axis.text.x = element_text(angle = 25,                                           # rotate axis labels so they're more readable
                                   margin = margin(t = 25, r = 0, b = 0, l = 0)),    
        plot.title = element_text(hjust = 0.5,                                           # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)))
```

![](q1_timeline_files/figure-markdown_github/checklist%20heatmap-1.png)

<br />

Otherwise, most application items take between one and two days (median). There isn't too much noticeable distinction between the site or the animal, but it does generally take *slightly* longer for cat applications to have items checked off then for dog ones. This could contribute to cat applications taking longer to end in adoption. Checklist\_VET was the item with the greatest differnce between cats and dogs.

<br />

``` r
# isolate the data that is related to checklist items for adoptions
checklist_animal_calcs <- masterapps_20190324 %>%
  filter(outcome_type == "Adoption") %>%
  gather(checklist_item, value, checklist_ACCT:checklist_VET) %>%       # flatten checklist rows into one column (called "checklist_item") and corresponsding values into one column (called "values")
  drop_na(value) %>%
  group_by(checklist_item, animal_type) %>%
  summarize(mean = mean(value),                                         # calculate mean, use summarize to collapse each site into single-row summary 
            median = median(value)) %>%                                 # calculate median, use summarize to collapse each site into single-row summary 
  mutate_at(vars(mean, median), funs(round(., 2))) %>%                  # round calcs to 2 decimal places
  rename("mean days from last item" = "mean",                           # rename the df columns to be more readable
         "median days from last item" = "median",
         "checlist item" = "checklist_item",
         "animal type" = "animal_type") 
```

``` r
# put the table into formattable
formattable(checklist_animal_calcs, align =c("l","c","c","c"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
  `mean days from last item`= color_tile("#FFEFDB", "#FF8000"),
  `median days from last item`= color_tile("#FFEFDB", "#FF8000")))
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
checlist item
</th>
<th style="text-align:center;">
animal type
</th>
<th style="text-align:center;">
mean days from last item
</th>
<th style="text-align:center;">
median days from last item
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
checklist\_ACCT
</td>
<td style="text-align:center;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8000">10.89</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ff8000">10.89</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_CHQ
</td>
<td style="text-align:center;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe6ca">2.22</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffebd4">1.00</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_CHQ
</td>
<td style="text-align:center;">
dog
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffeeda">1.56</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffeed9">0.80</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_LL
</td>
<td style="text-align:center;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe0be">2.74</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffebd3">1.08</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_LL
</td>
<td style="text-align:center;">
dog
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffedd7">1.68</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffedd8">0.83</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_PP
</td>
<td style="text-align:center;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe5c7">2.36</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffebd4">1.03</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_PP
</td>
<td style="text-align:center;">
dog
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffecd5">1.78</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffedd8">0.84</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_SPCA
</td>
<td style="text-align:center;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffb970">6.07</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffb467">6.07</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_TR
</td>
<td style="text-align:center;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe7cb">2.20</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffebd4">1.00</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_TR
</td>
<td style="text-align:center;">
dog
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffefdb">1.53</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffefdb">0.72</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_VET
</td>
<td style="text-align:center;">
cat
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffd7ad">3.48</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe2c3">1.83</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
checklist\_VET
</td>
<td style="text-align:center;">
dog
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe5c9">2.29</span>
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffead1">1.17</span>
</td>
</tr>
</tbody>
</table>
