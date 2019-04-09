PAWS App-Trajectory Report
================
Ramaa Nathan
4/5/2019

-   [Executive Summary <span style="color:brown"> - Amy</span> (will wait until report is complete)](#executive-summary---amy-will-wait-until-report-is-complete)
-   [Feature Engineering <span style="color:brown"> - Amy</span>](#feature-engineering---amy)
    -   [Data Cleaning](#data-cleaning)
    -   [Data Conversion](#data-conversion)
    -   [Spreading/Creating to Indicator Variables](#spreadingcreating-to-indicator-variables)
    -   [Merging data](#merging-data)
-   [Analysis of Time in Processing Applications](#analysis-of-time-in-processing-applications)
    -   [How Animal & Outcome Site Influence Appiclation Timelines](#how-animal-outcome-site-influence-appiclation-timelines)
    -   [How Animal & Outcome Site Influence Application Checklist Items](#how-animal-outcome-site-influence-application-checklist-items)
-   [Application Characteristics](#application-characteristics)
    -   [Affecting Adoption <span style="color:brown"> - Ramaa </span>](#affecting-adoption---ramaa)
    -   [Affecting Decline](#affecting-decline)
-   [Data Issues affecting Analyses <span style="color:brown"> - Brendan </span>](#data-issues-affecting-analyses---brendan)
    -   [Missing Data](#missing-data)
    -   [Unlimited Responses and Response Validation](#unlimited-responses-and-response-validation)
    -   [Recommendations for Collecting Clean Data](#recommendations-for-collecting-clean-data)
-   [Important Features for Prediction <span style="color:brown"> - Ramaa</span>](#important-features-for-prediction---ramaa)
-   [Conclusions and Recommendations](#conclusions-and-recommendations)
    -   [Recommendations to analyze the data frequently to check for improvements in processing application](#recommendations-to-analyze-the-data-frequently-to-check-for-improvements-in-processing-application)
    -   [Suggest](#suggest)
        -   [How findings can be translated into concrete actions](#how-findings-can-be-translated-into-concrete-actions)
        -   [How data collection strategy should be improved](#how-data-collection-strategy-should-be-improved)
        -   [How data analysis might be done on a regular basis](#how-data-analysis-might-be-done-on-a-regular-basis)

Executive Summary <span style="color:brown"> - Amy</span> (will wait until report is complete)
----------------------------------------------------------------------------------------------

Feature Engineering <span style="color:brown"> - Amy</span>
-----------------------------------------------------------

### Data Cleaning

### Data Conversion

### Spreading/Creating to Indicator Variables

### Merging data

Analysis of Time in Processing Applications
-------------------------------------------

### How Animal & Outcome Site Influence Appiclation Timelines

Application timelines were measured by taking the difference between the time an application was submitted and the time that application resulted in an adoption. Only applications that resulted in adoption were assessed; applications that were denied were not included in the analysis. This is a potential area of further investigation.

In general, cat applications typically take longer than dog applications. The chart below shows that the median adoption timeline for **cats** is approximately **19** days (vertical black line inside red box), while **dog** applications average about **8** to result in an adoption (vertical black line inside blue box).

<img src="PAWS_App_Trajectory_Report_files/figure-markdown_github/kc_timeline_boxplot_animal-1.png" style="display: block; margin: auto;" />

The chart also illuminates that for longer-than-average application timelines, animal type may influence just *how much longer* those above-average timelines are. Of the longer-than-usual applications, cat ones took between 35 days and 70 days compared to about 18 days to 40 days for dogs.

The outcome site for an adoption also influences the timeline of an application. It's important to note that this analysis does not consider all the potential locations that an animal spent its time during the application process; it is strictly based on the animal's outcome site.

<img src="PAWS_App_Trajectory_Report_files/figure-markdown_github/kc_heatmap_site-1.png" style="display: block; margin: auto;" />

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
outcome\_sitename
</th>
<th style="text-align:left;">
animal\_type
</th>
<th style="text-align:center;">
n
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
74
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">10</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
dog
</td>
<td style="text-align:center;">
19
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">6</span>
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
2
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">8</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
dog
</td>
<td style="text-align:center;">
18
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">13</span>
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
70
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">8</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
dog
</td>
<td style="text-align:center;">
17
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">4</span>
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
187
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">25</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
dog
</td>
<td style="text-align:center;">
20
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">12</span>
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
44
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">25</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
dog
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">22</span>
</td>
</tr>
</tbody>
</table>
From the heatmap and table above, it's clear that overall average adoption times were higher at PAWS Foster Program & PAWS Offsite Adoptions locations. This is especially true for cat applications at those places

Based on median values, here are the fastest & slowest time-to-adoption sites:

-   **Cats**
    -   Slowest: PAWS Foster Program
    -   Fastest: Grays Ferry Avenue
-   **Dogs**
    -   Slowest: PAWS Foster Program
    -   Fastest: PAC

Only one site had a higher median adoption time for dogs than for cats—Grays Ferry Avenue. This site also had the fewest cat adoptions, though (n=2). It's also important to note the small n size for dog apps at PAWS Offsite Adoptions (n=1).

### How Animal & Outcome Site Influence Application Checklist Items

<img src="PAWS_App_Trajectory_Report_files/figure-markdown_github/checklist_boxplot-1.png" style="display: block; margin: auto;" />

Most application items took between one and two days (median) to complete. While the animal type and outcome site didn't significantly impact the individual item times, cat applications generally exhibited slightly longer times between checklist items. Cat applications averaged about **1.2** days between checklist item, compared to **0.9** for dogs (excluding SPCA & ACCT items). The VET checklist item had the greatest difference between cats and dogs, and also was the item that took the longest (besides SPCA & ACCT items). This distinction between animals, while modest, could contribute to longer submission-to-adoption times for cat applications.

The chart above removed significant outliers, but further inspection of these outliers could be valuable. Understanding what causes certain application steps to take longer could help to streamline parts of the checklist process.

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
median days from last item
</th>
<th style="text-align:center;">
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
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">10.89</span>
</td>
<td style="text-align:center;">
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
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">6.07</span>
</td>
<td style="text-align:center;">
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
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">1.80</span>
</td>
<td style="text-align:center;">
93.8%
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
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">0.97</span>
</td>
<td style="text-align:center;">
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
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">1.03</span>
</td>
<td style="text-align:center;">
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
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">1.03</span>
</td>
<td style="text-align:center;">
95.6%
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
<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">0.95</span>
</td>
<td style="text-align:center;">
96.0%
</td>
</tr>
</tbody>
</table>
The table above shows the exceptions to the average checklist times. The ACCT and SPCA checklist items took considerably longer to complete than other items, but they also were present in less than 1% of applications. This low sample limits any sound conclusions, but does present an area for potential further exploration. It may be valuable to assess if other components of an application—like red flags or particular animal information—lead to this item being more mandatory. But more data would be needed for this analysis.

Application Characteristics
---------------------------

### Affecting Adoption <span style="color:brown"> - Ramaa </span>

We analysed the the different factors of the applications that ended with a successful adoption.

![](PAWS_App_Trajectory_Report_files/figure-markdown_github/Adoption-1.png)

When applicants requested a specific type of animal, 30% of applications resulted in an adoption vs only 22% of the applications resultd in an adoption. This seems surprising as we would expect an applicant who is not specific about the type of animal to be able to adopt easily.

![](PAWS_App_Trajectory_Report_files/figure-markdown_github/unnamed-chunk-1-1.png)

Most of the applicants who adopted a pet had allocated a monthly budget of less than $500.

![](PAWS_App_Trajectory_Report_files/figure-markdown_github/unnamed-chunk-2-1.png)

Applicants who expected to leave the animal alone at home for longer hours chose to adopt a cat. The largest number of applicants expected the animal to be alone for 8 hours, which would be typical of an applicant who works full time.

![](PAWS_App_Trajectory_Report_files/figure-markdown_github/unnamed-chunk-3-1.png)

Singles overwhelmingly seem to prefer to adopt a pet.

![](PAWS_App_Trajectory_Report_files/figure-markdown_github/unnamed-chunk-4-1.png)

Again, families with no children at home seem to be the largest number of applicants. This correlates with mostly singles wanting to adopt.

![](PAWS_App_Trajectory_Report_files/figure-markdown_github/unnamed-chunk-5-1.png)

Interestingly, more number of applicants who were able to successfully adopt had less expereince in each of the types of experiences.

![](PAWS_App_Trajectory_Report_files/figure-markdown_github/unnamed-chunk-6-1.png)

Not surprisingly, the highest number of succesful adoptions were associated with a home policy that allowed pets.

![](PAWS_App_Trajectory_Report_files/figure-markdown_github/unnamed-chunk-7-1.png)

The main reason that people would return a pet in the future seem to be if the pet sheds or if they moved too far away. Of these, more number of people who would return if the pet sheds did not adopt and of hte ones who adopted, they mainly adopted a cat.

![](PAWS_App_Trajectory_Report_files/figure-markdown_github/unnamed-chunk-8-1.png)

Most of the people who adopted a pet had no allergies.

### Affecting Decline

Data Issues affecting Analyses <span style="color:brown"> - Brendan </span>
---------------------------------------------------------------------------

### Missing Data

Overall we were able to achieve some insights given the application data. However, we were at times limited due to missing data in the applications data set. Below is a plot that shows counts of `NA`'s in each column of the data set.

![](PAWS_App_Trajectory_Report_files/figure-markdown_github/unnamed-chunk-9-1.png)

The question with the most missing data is one regarding the home pet policy. This seems like an important question, especially for renters, and a non-response here may require manual follow up by PAWS staff. Making this a required question could save some time in the future.

### Unlimited Responses and Response Validation

Like many of the other teams, we ran into several challenges as a result of questions having a wide range of possible responses and illogical answers. For example, the 12 different responses below are for the Allergy question:

| Response                      | Count |
|-------------------------------|-------|
| no-allergies                  | 1,694 |
| mildly-allergic               | 130   |
| not-sure                      | 38    |
| not-sure,no-allergies         | 16    |
| very-allergic                 | 10    |
| no-allergies,mildly-allergic  | 5     |
| no-allergies,not-sure         | 5     |
| mildly-allergic,no-allergies  | 3     |
| mildly-allergic,very-allergic | 3     |
| mildly-allergic,not-sure      | 1     |
| very-allergic,mildly-allergic | 1     |
| very-allergic,no-allergies    | 1     |

In one case the responses conflict with each other: "very-allergic,no-allergies". This make grouping the data after the fact almost impossible because its not clear if this applicant has allergies or not. This is one example, but there were some other cases where this problem occurred as well, such a for the questions relating to Experience and Where the Pet Will be Kept.

For the monthly budget question, there were several negative numbers and some extremely large, strange values (i.e $150,159.00). Utilizing some kind of response validation logic (i.e.only allow positive values) and limiting the range of responses to a reasonable size given the question (in this case maybe between 200 and 1,000) would also make future analysis much more efficient.

### Recommendations for Collecting Clean Data

One of the most important recommendations moving forward would be to redesign the application to enforce standardized, limited and logical responses. Allowing only a single response combined with a limited response set would make analysis much easier in the future. Doing so will save PAWS staff time when reviewing applications *and* make future analyses easier and can lead to better insights.

Important Features for Prediction <span style="color:brown"> - Ramaa</span>
---------------------------------------------------------------------------

Conclusions and Recommendations
-------------------------------

### Recommendations to analyze the data frequently to check for improvements in processing application

### Suggest

#### How findings can be translated into concrete actions

#### How data collection strategy should be improved

#### How data analysis might be done on a regular basis
