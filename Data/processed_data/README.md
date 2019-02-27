## Contents
Each of the datasets were originally separated into `cats_` and `dogs_`. The files within this folder have been combined into `all_` files. Other than unioning the files, the content of the data remains unchanged. 

* `all_apps` - the combination of `cat_apps` and `dog_apps`
* `all_actons` - the combination of `cat_actions` and `dog_actions`
* `all_cards` - the combination of `cat_cards` and `dog_cards`

<br>
The `actions` dataset originally had a line for each checklist action. A new csv was created called `actions_wide` that pivots the data into one row per `id`. The new dataset uses the minimum date as the start date and calculates the distance in days to the last entry for each checklist item. The day of week was also included to see if any delays exist due to the weekend.  

In this example: 

|animal_type | date_start | checklist_ACCT | checklist_CHQ | checklist_LL | checklist_PP | checklist_SPCA | checklist_TR | checklist_VET | wday_start|
|----|---------------------|------|------|-------|------|------|-----|-------|----|
|cat | 2018-10-09 17:58:49 | 10.9 | 5.18 | 10.05 | 5.18 | 10.9 | 5.2 | 18.96 | Tue|

The first action began on Tuesday 10/9/2018. The time it took to complete the ACCT checklist item was 10.9 days, the time for CHQ was 5.9, and the time to check with the vet was 18.96 days.

<br>

The R file to create these datasets is under `combine_data.R`
