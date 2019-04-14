MergedDataAnalysis-Adoption
================
Ramaa Nathan
3/21/2019

-   [Check the labels of the entries that correspond to adoption for any irregular patterns](#check-the-labels-of-the-entries-that-correspond-to-adoption-for-any-irregular-patterns)
-   [Verify the animal\_type column - a problem was detected here](#verify-the-animal_type-column---a-problem-was-detected-here)
-   [Plots for adopted](#plots-for-adopted)
    -   [Apply Random Forests to check for important varaibles](#apply-random-forests-to-check-for-important-varaibles)
    -   [Impute the values](#impute-the-values)
    -   [Apply random forests](#apply-random-forests)

``` r
master <- readRDS("../masterapps_20190324.rds")
dim(master)
```

    ## [1] 1684  249

``` r
#colnames(master)
#outcome_type contains the final status
#are there any NA?
anyNA(master$outcome_type)
```

    ## [1] TRUE

``` r
#What are the unique labels?
unique(master$outcome_type)
```

    ## [1] "Adoption" NA

``` r
# All are only adoption

#Find column names without ind
#master %>% select(-ends_with("_ind")) %>% colnames()
```

#### Check the labels of the entries that correspond to adoption for any irregular patterns

``` r
#master %>% select(-contains("_ind")) %>% str()
#master %>% select(contains("_ind")) %>% str()

unique(master$last_label)
```

    ##  [1] "adopted"                     NA                           
    ##  [3] "ready for review"            "need to see id"             
    ##  [5] "red flag"                    "denied"                     
    ##  [7] "not utd"                     "reviewed with handouts only"
    ##  [9] "vet"                         "need vet info"              
    ## [11] "need roommates vet info"     "questions"                  
    ## [13] "ready to adopt"              "pet policy"                 
    ## [15] "vet check in process"        "adoption follow up"         
    ## [17] "approved"                    "withdrawn"                  
    ## [19] "checks"                      "need info"                  
    ## [21] "opa"                         "adopted elsewhere"          
    ## [23] "manager decision"            "returned"                   
    ## [25] "rescue check"                "need proof of ownership"    
    ## [27] "declaw only"                 "approved with limitation"   
    ## [29] "need written ll permission"  "landlord"                   
    ## [31] "dog meet"                    "unsure foster or adopt"

``` r
#master %>% select(trello_id, outcome_type,contains("label"))

#check if any of the labels do not contain adopted
master %>% filter(outcome_type=="Adoption") %>% filter(!str_detect(label_names,"adopted")) %>% select(trello_id, outcome_type,label_names) 
```

    ##                   trello_id outcome_type
    ## 1  5b6b639dd851035960ee724a     Adoption
    ## 2  5b6b639dd851035960ee724a     Adoption
    ## 3  5c0ad604f7f228172af0f148     Adoption
    ## 4  5c0ad5e7dd86162c0a1eb6d1     Adoption
    ## 5  5c0ad5e7dd86162c0a1eb6d1     Adoption
    ## 6  5bfee9a5693d880396d90210     Adoption
    ## 7  5bfee9a5693d880396d90210     Adoption
    ## 8  5bfe0a0c879b8689ee069946     Adoption
    ## 9  5bfae6e1bc78d9136683ece0     Adoption
    ## 10 5bf5c4c58a5a8563df72b7f5     Adoption
    ## 11 5bee098c4de3e93aedb7e9c4     Adoption
    ## 12 5bee098c4de3e93aedb7e9c4     Adoption
    ## 13 5bd1e88e057d1d25395c520a     Adoption
    ## 14 5bd1de82e8169f09bbddbc0c     Adoption
    ## 15 5af4aa7258bd82967684f43e     Adoption
    ## 16 5bcc2864c6cd64460cb9bf28     Adoption
    ## 17 5bc89bf68c66b98bf5280cdc     Adoption
    ## 18 5bc4d1ba255ef371eac51557     Adoption
    ## 19 5bc158935d42f06bd0acef6a     Adoption
    ## 20 5bbe58a03ca2933299f4865e     Adoption
    ## 21 5bb93f37d1c14c86a9e2bd68     Adoption
    ## 22 5bb771ca00c1b70a28aa3446     Adoption
    ## 23 5bafdcc35f4e222c53059c32     Adoption
    ## 24 5ba3006791e7c979a4795c06     Adoption
    ## 25 5b8ac714b5dbd5263293a353     Adoption
    ## 26 5b8ac714b5dbd5263293a353     Adoption
    ## 27 5bd8b23c4ce1e35d92411f5b     Adoption
    ## 28 5bd9ce9a76fcfd0758376d96     Adoption
    ##                                              label_names
    ## 1                                       ready for review
    ## 2                                       ready for review
    ## 3                  approved, reviewed with handouts only
    ## 4                               ready to adopt, red flag
    ## 5                               ready to adopt, red flag
    ## 6            ready to adopt, reviewed with handouts only
    ## 7            ready to adopt, reviewed with handouts only
    ## 8                                         ready to adopt
    ## 9            reviewed with handouts only, ready to adopt
    ## 10                                        ready to adopt
    ## 11                                        ready to adopt
    ## 12                                        ready to adopt
    ## 13                                        ready to adopt
    ## 14           ready to adopt, reviewed with handouts only
    ## 15                              returned, ready to adopt
    ## 16                                                   vet
    ## 17           ready to adopt, reviewed with handouts only
    ## 18                                        ready to adopt
    ## 19                                        ready to adopt
    ## 20                                        ready to adopt
    ## 21           ready to adopt, reviewed with handouts only
    ## 22                                        ready to adopt
    ## 23           ready to adopt, reviewed with handouts only
    ## 24                                        ready to adopt
    ## 25           ready to adopt, reviewed with handouts only
    ## 26           ready to adopt, reviewed with handouts only
    ## 27           ready to adopt, reviewed with handouts only
    ## 28 ready to adopt, reviewed with handouts only, dog meet

``` r
## 28 entries found!

#check if any of the labels do not contain "adopt" in any form
master %>% filter(outcome_type=="Adoption") %>% filter(!str_detect(label_names,"adopt")) %>% select(trello_id, outcome_type,label_names) 
```

    ##                  trello_id outcome_type
    ## 1 5b6b639dd851035960ee724a     Adoption
    ## 2 5b6b639dd851035960ee724a     Adoption
    ## 3 5c0ad604f7f228172af0f148     Adoption
    ## 4 5bcc2864c6cd64460cb9bf28     Adoption
    ##                             label_names
    ## 1                      ready for review
    ## 2                      ready for review
    ## 3 approved, reviewed with handouts only
    ## 4                                   vet

``` r
# there are four entries with no labels associated with label

#check if there are no labels at all
master  %>% filter(outcome_type=="Adoption") %>% filter(num_labels == 0) %>% select(trello_id,animal_type,label_names)
```

    ##                  trello_id animal_type label_names
    ## 1 5830b6fd9b5eb5693b943550         cat        <NA>
    ## 2 582a54bcc7f743105f7b1e64         cat        <NA>
    ## 3 5bbab2bade6f6875fb736265         cat        <NA>
    ## 4 5bbab2bade6f6875fb736265         cat        <NA>
    ## 5 5babb1c7b635cc3245d30b26         cat        <NA>
    ## 6 5b9d277d9aebec80e0f888ee         cat        <NA>

``` r
# there are six total entries and fice unique entries with no labels

#master  %>% filter(outcome_type=="Adoption") %>% filter(num_labels == 0) %>%  select(trello_id,label_names,date_submitted,outcome_date,starts_with("checklist")) 
#master  %>% filter(outcome_type=="Adoption") %>% select(trello_id,starts_with("checklist")) 
```

#### Verify the animal\_type column - a problem was detected here

``` r
#check why we have animal.x and animal.y
master %>% select(starts_with("animal_type")) %>% table()
```

    ## .
    ##  cat  dog 
    ## 1057  627

### Plots for adopted

``` r
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=animal_type,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Species") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=specific_animal,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Specific Animal") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=new_age_group,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Age Group of Animal") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=budget_monthly_ranges,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Monthly budgets") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-4.png)

``` r
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=budget_emergency_ranges,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Emergency budgets") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-5.png)

``` r
### Home alone avg
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=home_alone_avg,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Home Alone Average") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

    ## Warning: Removed 98 rows containing non-finite values (stat_count).

    ## Warning: Removed 98 rows containing non-finite values (stat_count).

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-6.png)

``` r
### Home alone max
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=home_alone_max,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Home Alone Maximum") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

    ## Warning: Removed 118 rows containing non-finite values (stat_count).

    ## Warning: Removed 118 rows containing non-finite values (stat_count).

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-7.png)

``` r
### Adults at home
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=adults_in_home,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Number of Adults in Home") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

    ## Warning: Removed 2 rows containing non-finite values (stat_count).

    ## Warning: Removed 2 rows containing non-finite values (stat_count).

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-8.png)

``` r
### CHildren at home
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=children_in_home,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Number of Children in Home") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

    ## Warning: Removed 2 rows containing non-finite values (stat_count).

    ## Warning: Removed 2 rows containing non-finite values (stat_count).

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-9.png)

``` r
### Experience
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=experience,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Experience with Pets") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-10.png)

``` r
### Home Pet Policy
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=home_pet_policy,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Home Pet Policy") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-11.png)

``` r
### Pet Return
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=return_pet,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Pet Return") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-12.png)

``` r
### Allergies
master %>% filter(outcome_type=="Adoption") %>%
  ggplot(mapping=aes(x=allergies,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Allergies") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](RNathan_Merged_Adopt_files/figure-markdown_github/unnamed-chunk-3-13.png)

#### Apply Random Forests to check for important varaibles

``` r
master_mod <- master %>%
  mutate(adoption=ifelse((!is.na(outcome_type) & outcome_type=="Adoption"),1,0)) %>%
  select(-c(trello_id,reason_for_adoption,all_household_agree,allergies,home_owner, all_household_agree,allergies,
            experience, budget_monthly, budget_emergency,pet_kept,exercise,needs,return_pet),
         -starts_with("budget."),
         -starts_with("home.alone"),
         -starts_with("checklist"),  #the type of check may not affect final adoption
         -starts_with("label"),  #labels are not reliable
         -starts_with("new.age"))

## find columns with NA
master_mod %>% skim() %>% 
  filter(stat == "missing") %>% filter(value > 0) %>% 
  kable(caption="Missing Entries in master_mod")
```

    ## Warning in min.default(structure(c(NA_real_, NA_real_, NA_real_,
    ## NA_real_, : no non-missing arguments to min; returning Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_,
    ## NA_real_, : no non-missing arguments to max; returning -Inf

    ## Warning in min.default(structure(c(NA_real_, NA_real_, NA_real_,
    ## NA_real_, : no non-missing arguments to min; returning Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_,
    ## NA_real_, : no non-missing arguments to max; returning -Inf

| variable                                                      | type      | stat    | level |  value| formatted |
|:--------------------------------------------------------------|:----------|:--------|:------|------:|:----------|
| adults\_in\_home                                              | numeric   | missing | .all  |      5| 5         |
| children\_in\_home                                            | integer   | missing | .all  |      9| 9         |
| home\_pet\_policy                                             | factor    | missing | .all  |    998| 998       |
| home\_alone\_avg                                              | numeric   | missing | .all  |    417| 417       |
| home\_alone\_max                                              | numeric   | missing | .all  |    435| 435       |
| how\_heard                                                    | character | missing | .all  |    799| 799       |
| City                                                          | factor    | missing | .all  |      1| 1         |
| ZIP                                                           | character | missing | .all  |      5| 5         |
| reason.for.adoption\_gift\_ind                                | numeric   | missing | .all  |     36| 36        |
| reason.for.adoption\_mouser\_ind                              | numeric   | missing | .all  |     36| 36        |
| reason.for.adoption\_my.kids\_ind                             | numeric   | missing | .all  |     36| 36        |
| reason.for.adoption\_myself\_ind                              | numeric   | missing | .all  |     36| 36        |
| reason.for.adoption\_other\_ind                               | numeric   | missing | .all  |     36| 36        |
| reason.for.adoption\_protection\_ind                          | numeric   | missing | .all  |     36| 36        |
| home.owner\_company\_ind                                      | numeric   | missing | .all  |     20| 20        |
| home.owner\_family.friend\_ind                                | numeric   | missing | .all  |     20| 20        |
| home.owner\_family.member.or.friend\_ind                      | numeric   | missing | .all  |     20| 20        |
| home.owner\_landlord\_ind                                     | numeric   | missing | .all  |     20| 20        |
| home.owner\_myself\_ind                                       | numeric   | missing | .all  |     20| 20        |
| home.pet.policy\_no.but.pets.allowed\_ind                     | numeric   | missing | .all  |    995| 995       |
| home.pet.policy\_not.applicable\_ind                          | numeric   | missing | .all  |    995| 995       |
| home.pet.policy\_not.yet\_ind                                 | numeric   | missing | .all  |    995| 995       |
| home.pet.policy\_yes\_ind                                     | numeric   | missing | .all  |    995| 995       |
| home.pet.policy\_yes.with.pet.policy\_ind                     | numeric   | missing | .all  |    995| 995       |
| pet.kept\_crate\_ind                                          | numeric   | missing | .all  |     24| 24        |
| pet.kept\_inside.only\_ind                                    | numeric   | missing | .all  |     24| 24        |
| pet.kept\_inside.outside\_ind                                 | numeric   | missing | .all  |     24| 24        |
| pet.kept\_inside.with.yard.access\_ind                        | numeric   | missing | .all  |     24| 24        |
| pet.kept\_leash.harness\_ind                                  | numeric   | missing | .all  |     24| 24        |
| pet.kept\_other\_ind                                          | numeric   | missing | .all  |     24| 24        |
| pet.kept\_outside.only\_ind                                   | numeric   | missing | .all  |     24| 24        |
| pet.kept\_supervised.in.my.yard\_ind                          | numeric   | missing | .all  |     24| 24        |
| pet.kept\_unsupervised.access.to.my.yard.doggie.door.etc\_ind | numeric   | missing | .all  |     24| 24        |
| exercise\_another.pet\_ind                                    | numeric   | missing | .all  |     37| 37        |
| exercise\_dog.parks\_ind                                      | numeric   | missing | .all  |     37| 37        |
| exercise\_jogging.together\_ind                               | numeric   | missing | .all  |     37| 37        |
| exercise\_not.much\_ind                                       | numeric   | missing | .all  |     37| 37        |
| exercise\_other.cats\_ind                                     | numeric   | missing | .all  |     37| 37        |
| exercise\_other.pets\_ind                                     | numeric   | missing | .all  |     37| 37        |
| exercise\_playing.in.my.yard\_ind                             | numeric   | missing | .all  |     37| 37        |
| exercise\_toy.mice\_ind                                       | numeric   | missing | .all  |     37| 37        |
| exercise\_walks.off.leash\_ind                                | numeric   | missing | .all  |     37| 37        |
| exercise\_walks.on.leash\_ind                                 | numeric   | missing | .all  |     37| 37        |
| exercise\_wand.toys\_ind                                      | numeric   | missing | .all  |     37| 37        |
| needs\_declaw\_ind                                            | numeric   | missing | .all  |     37| 37        |
| needs\_groom.myself\_ind                                      | numeric   | missing | .all  |     37| 37        |
| needs\_nail.clip\_ind                                         | numeric   | missing | .all  |     37| 37        |
| needs\_no.grooming\_ind                                       | numeric   | missing | .all  |     37| 37        |
| needs\_not.sure\_ind                                          | numeric   | missing | .all  |     37| 37        |
| needs\_other\_ind                                             | numeric   | missing | .all  |     37| 37        |
| needs\_professional.groomer\_ind                              | numeric   | missing | .all  |     37| 37        |
| needs\_scratching.post\_ind                                   | numeric   | missing | .all  |     37| 37        |
| date\_start                                                   | POSIXct   | missing | .all  |     55| 55        |
| wday\_start                                                   | factor    | missing | .all  |     55| 55        |
| dateLastActivity                                              | Date      | missing | .all  |   1684| 1684      |
| due                                                           | Date      | missing | .all  |   1684| 1684      |
| last\_label                                                   | character | missing | .all  |     25| 25        |
| num\_labels                                                   | numeric   | missing | .all  |     11| 11        |
| species                                                       | character | missing | .all  |   1231| 1231      |
| primary\_breed                                                | character | missing | .all  |   1231| 1231      |
| secondary\_breed                                              | character | missing | .all  |   1231| 1231      |
| markings                                                      | character | missing | .all  |   1683| 1683      |
| gender                                                        | character | missing | .all  |   1231| 1231      |
| altered                                                       | character | missing | .all  |   1231| 1231      |
| dob                                                           | Date      | missing | .all  |   1231| 1231      |
| age\_intake                                                   | integer   | missing | .all  |   1231| 1231      |
| intake\_asilomar                                              | character | missing | .all  |   1440| 1440      |
| intake\_condition                                             | character | missing | .all  |   1231| 1231      |
| intake\_date                                                  | POSIXct   | missing | .all  |   1231| 1231      |
| intake\_type                                                  | character | missing | .all  |   1232| 1232      |
| intake\_subtype                                               | character | missing | .all  |   1232| 1232      |
| intake\_reason                                                | character | missing | .all  |   1362| 1362      |
| intake\_sitename                                              | character | missing | .all  |   1231| 1231      |
| agency\_name                                                  | character | missing | .all  |   1402| 1402      |
| outcome\_asilomar                                             | character | missing | .all  |   1413| 1413      |
| release\_date                                                 | POSIXct   | missing | .all  |   1231| 1231      |
| outcome\_date                                                 | POSIXct   | missing | .all  |   1231| 1231      |
| outcome\_type                                                 | character | missing | .all  |   1231| 1231      |
| outcome\_subtype                                              | character | missing | .all  |   1232| 1232      |
| outcome\_sitename                                             | character | missing | .all  |   1232| 1232      |
| outcome\_city                                                 | character | missing | .all  |   1233| 1233      |
| outcome\_state                                                | character | missing | .all  |   1233| 1233      |
| outcome\_ZIP                                                  | character | missing | .all  |   1233| 1233      |
| new\_age\_group                                               | factor    | missing | .all  |   1231| 1231      |
| process\_time                                                 | numeric   | missing | .all  |   1231| 1231      |
| process\_time\_periods                                        | factor    | missing | .all  |   1231| 1231      |

#### Impute the values

All \_ind columns should have either a 1 or 0. There is some bug in the convert\_to\_ind funtion what was used during merge that is creating NA in the \_ind columns. Set them to 0.

``` r
#master_mod %>% select(ends_with("_ind")) %>% colnames() %>% str() #replace_na(0) %>% View()
```

#### Apply random forests

``` r
inTrain <- createDataPartition(y=master_mod$adoption, p=0.7, list=FALSE)
training <- master_mod[inTrain, ]
testing <- master_mod[-inTrain,]
dim(training)
```

    ## [1] 1179  129

``` r
dim(testing)
```

    ## [1] 505 129

``` r
table(training$adoption)
```

    ## 
    ##   0   1 
    ## 874 305

``` r
table(testing$adoption)
```

    ## 
    ##   0   1 
    ## 357 148

``` r
#rfFit <- train(adoption~.,data=training,method="rf",prox=TRUE,importance=TRUE)
#rffit
#importance(rffit)
```
