---
title: "apps_data_vd"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r cars}
library(readr)
dataCleaned <- read.csv("Analyses/2_Applicants/amygood/output/apps_clean.csv")

#frequency of different home_owner types
dplyr::count(dataCleaned, home_owner)
```

```{r cars}
#frequency of different allergies types
dplyr::count(dataCleaned, allergies)
```


```{r cars}
#frequency of different ideal_adoption_timeline types
dplyr::count(dataCleaned, ideal_adoption_timeline)
```


```{r cars}
#frequency of different reason_for_adoption types
dplyr::count(dataCleaned, reason_for_adoption)
```

```{r cars}
#frequency of different all_household_agree types
dplyr::count(dataCleaned, all_household_agree)
```