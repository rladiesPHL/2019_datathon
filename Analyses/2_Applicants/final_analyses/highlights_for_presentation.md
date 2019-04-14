Highlights for presentation
================
Amy Goodwin Davies

-   [Data Visualizations](#data-visualizations)
    -   [Petpoint Visualizations](#petpoint-visualizations)
    -   [Cards Visualizations](#cards-visualizations)
    -   [Apps Visualizations](#apps-visualizations)
    -   [Actions Visualizations](#actions-visualizations)
-   [Adoption timeline (from Kate's code)](#adoption-timeline-from-kates-code)
-   [Speedy adoptions](#speedy-adoptions)

``` r
library(tidyverse)
library(lubridate)
library(formattable)
```

    ## Warning: package 'formattable' was built under R version 3.5.3

-   ggplot theme for presentation (increased font size)

``` r
theme_pres <- function () { 
  theme_bw(base_size = 32) %+replace% 
    theme(
      axis.title.x = element_text(face="bold", margin = margin(5, 0, 5, 0)),
      axis.title.y  = element_text(face="bold", angle=90, margin = margin(0, 5, 0, 5)),
      axis.text.y  = element_text(hjust = 0.95),
      plot.title = element_text(face="bold", margin = margin(5, 0, 5, 0)),
      legend.text = element_text(margin = margin(5, 0, 5, 0)),
      legend.title = element_text(face="bold", margin = margin(5, 0, 5, 0)),
      strip.text.x = element_text(margin = margin(10, 0, 10, 0)),
      strip.text.y = element_text(angle=90, margin = margin(0, 10, 0, 10))
    )
}
```

-   load Data

``` r
master_apps <- read_rds("Analyses/2_Applicants/masterapps_20190324.rds")
actions <- read_rds("Analyses/2_Applicants/processed_actions.rds")
cards <- read_rds("Analyses/2_Applicants/processed_cards_with_indicators.rds")
petpoint <- read_rds("Analyses/2_Applicants/processed_petpoint_with_indicators.rds")
```

Data Visualizations
-------------------

### Petpoint Visualizations

``` r
#plot the distribution of the processing time - number of days between intake and outcome 
petpoint_processing_time <- petpoint %>% ggplot(mapping=aes(x=process_time,fill=animal_type)) +
  geom_histogram(breaks=seq(20,300,by=20)) +
  labs(title="Histogram of Processing Time in Days\n(Intake to Outcome)") +
  theme_pres() +
  facet_wrap(~ animal_type, scales = "free_y") +
  guides(fill = FALSE)
ggsave(plot=petpoint_processing_time,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/petpoint_processing_time.png",
       width = 12, height = 8, dpi=300, device = "png")

#plot the process time periods categories
petpoint_processing_periods <- petpoint %>%  ggplot(mapping=aes(x=process_time_periods,fill=animal_type)) +
   geom_bar() +
    labs(title = "Petpoint\nprocessing times") +
  theme_pres() + 
  theme(legend.position = "bottom") +
  ylim(0,900) +
  coord_flip()
ggsave(plot=petpoint_processing_periods,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/petpoint_processing_periods.png",
       width = 12, height = 8, dpi=300, device = "png")
```

    ## Warning: Removed 1 rows containing missing values (geom_bar).

### Cards Visualizations

``` r
#Distribution of lastLabel 
cards_last_label_plot <- cards %>% filter(!is.na(label_names)) %>%
  group_by(last_label) %>% 
  tally() %>% 
  ungroup() %>% 
  top_n(15) %>% 
  ggplot(aes(x = fct_reorder(last_label, n), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), hjust = -0.25, position = "identity", size = 7) +
  guides(fill=FALSE) +
  ylim(0,1600) +
  coord_flip() +
  theme_pres() +
  ggtitle("Top 15 last labels")
```

    ## Selecting by n

``` r
ggsave(plot=cards_last_label_plot,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/cards_last_label_plot.png",
       width = 12, height = 8, dpi=300, device = "png")

label_names_summary <- cards %>%
  filter(!is.na(label_names)) %>% 
  select(starts_with("label.names")) %>% 
  summarise_all(sum, na.rm = TRUE) %>%
   gather(num_labels, "count") %>% 
  rename(label_names = num_labels) %>% 
  mutate(short_label = str_replace(label_names, "label.names_", ""),
         short_label = str_replace(short_label, "_ind", "")) %>% 
  select(short_label, count) %>% 
  top_n(10)
```

    ## Selecting by count

### Apps Visualizations

``` r
count_animal_plot <- master_apps %>% 
  select(animal_type) %>% 
  group_by(animal_type) %>% 
  tally() %>% 
  ungroup() %>% 
  ggplot(aes(x = animal_type, y = n, fill = animal_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.25, position = "identity", size = 7) +
  theme_pres() +
  guides(fill=FALSE)
ggsave(plot=count_animal_plot,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/count_animal_plot.png",
       width = 12, height = 8, dpi=300, device = "png")

apps_budget_monthly <- master_apps %>%
  filter(budget_monthly < 2500) %>% 
  ggplot(aes(budget_monthly, fill = animal_type)) +
  geom_histogram(binwidth = 50) +
  theme_pres() +
  guides(fill = FALSE) +
  facet_wrap(~ animal_type)
ggsave(plot=apps_budget_monthly,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/apps_budget_monthly.png",
       width = 12, height = 8, dpi=300, device = "png")

apps_budget_emergency <- master_apps %>%
  filter(budget_emergency < 5000) %>% 
  ggplot(aes(budget_emergency, fill = animal_type)) +
  geom_histogram(binwidth = 50) +
  theme_pres() +
  guides(fill = FALSE) +
  facet_wrap(~ animal_type)
ggsave(plot=apps_budget_emergency,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/apps_budget_emergency.png",
       width = 12, height = 8, dpi=300, device = "png")

apps_home_alone_avg <- master_apps %>%
  ggplot(aes(home_alone_avg, fill = animal_type)) +
  geom_histogram(binwidth = 1) +
  theme_pres() +
  guides(fill = FALSE) +
  facet_wrap(~ animal_type)
ggsave(plot=apps_home_alone_avg,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/apps_home_alone_avg.png",
       width = 12, height = 8, dpi=300, device = "png")
```

    ## Warning: Removed 417 rows containing non-finite values (stat_bin).

``` r
apps_home_alone_max <- master_apps %>%
  ggplot(aes(home_alone_max, fill = animal_type)) +
  geom_histogram(binwidth = 1) +
  theme_pres() +
  guides(fill = FALSE) +
  facet_wrap(~ animal_type)
ggsave(plot=apps_home_alone_max,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/apps_home_alone_max.png",
       width = 12, height = 8, dpi=300, device = "png")
```

    ## Warning: Removed 435 rows containing non-finite values (stat_bin).

### Actions Visualizations

Adoption timeline (from Kate's code)
====================================

``` r
# add adoption_time column for the difference between the date_submitted & outcome_date
master_apps <- master_apps %>%
  mutate(adoption_time = difftime(outcome_date, date_submitted, units = "days"),
         adoption_time = round(as.numeric(adoption_time), 2))
```

``` r
# isolate the data that is related to both outcome site & animal type
site_animal_df <- master_apps %>%
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

# rename site_animaL_df column names to include underscores
site_animal_df <- site_animal_df %>%
  rename("animal_type" = "animal",                                # couldn't use underscores earlier due to formatting of table, need them now to do other anlysis
         "outcome_site" = "outcome site",                     
         "mean_adoption_time" = "mean adoption time",
         "median_adoption_time" = "median adoption time") 
```

``` r
# boxplot of adoption_time by animal
master_apps <- master_apps %>%
  mutate(adoption_time = difftime(outcome_date, date_submitted, units = "days"),
         adoption_time = round(as.numeric(adoption_time), 2))
adoption_time_by_animal <- master_apps %>%
  filter(!adoption_time < 0) %>%                                                    # remove negative values in adoption_time column
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
  coord_flip()                +
  theme_pres() +
  guides(fill = FALSE)
ggsave(plot=adoption_time_by_animal,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/adoption_time_by_animal.png",
       width = 12, height = 8, dpi=300, device = "png")
```

``` r
adoption_timeline <- master_apps %>%
  filter(!adoption_time < 0) %>%                                                        # remove negative values in adoption_time column
 ggplot(aes(x = adoption_time, fill = animal_type)) +                 # break out checklist_item by cat & dog
  geom_density(alpha = 0.5) +                                    # set y axis tick intervals at 20
  ggtitle("Adoption timeline") +                                              # set plot title
  labs(x = NULL,                                                                        # set plot labels
       y= "Days") +
  theme_pres() +
  theme(legend.position = "bottom")
ggsave(plot=adoption_timeline,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/adoption_timeline.png",
       width = 12, height = 8, dpi=300, device = "png")
```

``` r
# isolate the data that is related to both outcome site & animal type
site_animal_df <- master_apps %>%
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

site_animal_df <- site_animal_df %>%
  rename("animal_type" = "animal",                                # couldn't use underscores earlier due to formatting of table, need them now to do other anlysis
         "outcome_site" = "outcome site",                     
         "mean_adoption_time" = "mean adoption time",
         "median_adoption_time" = "median adoption time") 

# heatmap plot of median adoption time by animal & adoption site
timeline_heatmap <- site_animal_df %>%
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
                                  margin = margin(t = 0, r = 0, b = 25, l = 0))) +
  coord_flip() +
  theme_pres() +
  theme(legend.position = "FALSE",
        axis.text.x = element_text(size = 20))
ggsave(plot=timeline_heatmap,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/timeline_heatmap.png",
       width = 16, height = 8, dpi=300, device = "png")
```

``` r
#  REMOVED SOME OUTLIERS; days distribution boxplot, by checklist_item & animal
checklist_item <- master_apps %>%
  filter(outcome_type == "Adoption") %>%
  gather(checklist_item, value, checklist_ACCT:checklist_VET) %>%                       # flatten checklist rows into one column (called "checklist_item") and corresponsding values into one column (called "values")
  ggplot(aes(x = checklist_item, y = value)) +
  geom_boxplot(aes(fill = animal_type), alpha = 0.4, outlier.alpha = 0.1) +             # break out checklist_item by cat & dog
  scale_y_continuous(breaks = seq(0, 20, by=2),                                         # set y axis tick intervals at 2
                     limits=c(0, 20)) +                                                 # set y limit to 16 to "remove" highest outliers & see plots better 
  theme_light() + 
  ggtitle("Day Count Distribution by\nChecklist Item (Fewer Outliers)") +
  labs(x = "checklist item",
       y= "days from last checklist item",
       fill = "animal type") +
  theme(plot.title = element_text(hjust = 0.5,                                          # title formatting (center, bold, padding)
                                  line = 15, 
                                  face = "bold", 
                                  margin = margin(t = 0, r = 0, b = 25, l = 0)),        
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 5, l = 0)),      # x axis title formatting (padding)
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5))) +    # y axis title formatting (padding)
  coord_flip()  +
  theme_pres() +
  theme(legend.position = "bottom")
ggsave(plot=checklist_item,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/checklist_item.png",
       width = 12, height = 8, dpi=300, device = "png")
```

    ## Warning: Removed 1037 rows containing non-finite values (stat_boxplot).

``` r
# isolate the data that is related to checklist items for adoptions
checklist_calcs <- master_apps %>%
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
checklist_count <- master_apps %>%
  filter(outcome_type == "Adoption") %>%
  gather(checklist_item, value, checklist_ACCT:checklist_VET) %>%
  drop_na(value) %>%
  group_by(checklist_item) %>%
  count(checklist_item)

# combine the calculations with the n for each checklist_item
checklist_df <- merge(checklist_count, checklist_calcs, by = "checklist_item", all.x = TRUE) %>%
  mutate(item_percent = percent(n/453, 1)) %>% # calculate the percent of applications that had each item checked off
  arrange(desc(`median days from last item`)) %>% 
  rename("percent of cards with item checked" = "item_percent",                                          # rename the df columns to be more readable
         "checklist item" = "checklist_item")

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
</tbody>
</table>
Speedy adoptions
================

``` r
gender_switch <- master_apps %>% 
  filter(!adoption_time < 0,
         gender != "U") %>% 
  mutate(is_speedy = case_when(adoption_time < 31 ~ "speedy",
                               !adoption_time < 31 ~ "not speedy")) %>% 
  group_by(is_speedy, gender, animal_type) %>% 
  tally() %>% 
  ungroup %>% 
  ggplot(aes(gender, n, fill = animal_type )) +
  geom_bar(stat = "identity") +
  ylim(0,175) +
  facet_grid(animal_type ~ is_speedy) +
  geom_text(aes(label = n), vjust = -0.25, position = "identity", size = 7) +
  theme_pres() +
  guides(fill = FALSE)
ggsave(plot=gender_switch,
       filename="Analyses/2_Applicants/final_analyses/presentation_plots/gender_switch.png",
       width = 12, height = 8, dpi=300, device = "png")
```
