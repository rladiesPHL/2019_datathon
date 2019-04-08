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
    -   [How long does each application take?](#how-long-does-each-application-take)
    -   [Heatmap](#heatmap)
-   [Application Characteristics](#application-characteristics)
    -   [Affecting Adoption <span style="color:brown"> - Ramaa </span>](#affecting-adoption---ramaa)
        -   [Frequency of processing applications - is it daily checks or processing applications in bulk every few days? (new analysis -- to check)](#frequency-of-processing-applications---is-it-daily-checks-or-processing-applications-in-bulk-every-few-days-new-analysis----to-check)
        -   [Budget, allergies, number of adults, number of children….](#budget-allergies-number-of-adults-number-of-children.)
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

### How long does each application take?

### Heatmap

Application Characteristics
---------------------------

### Affecting Adoption <span style="color:brown"> - Ramaa </span>

#### Frequency of processing applications - is it daily checks or processing applications in bulk every few days? (new analysis -- to check)

#### Budget, allergies, number of adults, number of children….

### Affecting Decline

Data Issues affecting Analyses <span style="color:brown"> - Brendan </span>
---------------------------------------------------------------------------

### Missing Data

Overall we were able to achieve some insights given the application data. However, we were at times limited due to missing data in the applications data set. Below is a plot that shows counts of `NA`'s in each column of the dataset.

![](https://github.com/rladiesPHL/2019_datathon/raw/paws-app-trajectory-q2/Analyses/2_Applicants/final_analyses/presentation_plots/NA_Count.png)

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

In one case the responses conflict with each other: "very-allergic,no-allergies". This make grouping the data after the fact almost impossible because its not clearif this applicant has allergies or not. This is one example, but there were some other cases where this problem occured as well, such a for the questions relating to Experience and Where the Pet Will be Kept.

For the monthly budget question, there were several negative numbers and some extremely large, strange values (i.e $150,159.00). Utilizaing some kind of response validation logic (i.e.only allow positive values) and limiting the range of responses to a reasonable size given the question (in this case maybe between 200 and 1,000) would also make future analysis much more efficient.

### Recommendations for Collecting Clean Data

One of the most imporant recommendations moving forward would be to redesign the application to enforce standardized, limited and logical responses. Allowing only a single response combined with a limited response set would make analysis much easier in the future. Doing so will save PAWS staff time when rev¶iewing applciations *and* make future analyses easier and can lead to better insights.

Important Features for Prediction <span style="color:brown"> - Ramaa</span>
---------------------------------------------------------------------------

Conclusions and Recommendations
-------------------------------

### Recommendations to analyze the data frequently to check for improvements in processing application

### Suggest

#### How findings can be translated into concrete actions

#### How data collection strategy should be improved

#### How data analysis might be done on a regular basis
