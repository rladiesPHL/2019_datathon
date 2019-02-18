Initial data exploration
================
Amy Goodwin Davies

-   [Actions](#actions)
-   [Cards](#cards)
-   [Apps](#apps)

Load packages

``` r
library(tidyverse)
library(lubridate)
library(data.table)
library(forcats)
```

Source helper function(s)

``` r
source("Analyses/2_Applicants/helper_functions.R")
```

Load data

``` r
cat_actions <- read_csv("Data/cat_actions.csv")
dog_actions <- read_csv("Data/dog_actions.csv")
cat_cards <- read_csv("Data/cat_cards.csv")
dog_cards <- read_csv("Data/dog_cards.csv")
cat_apps <- read_csv("Data/cat_apps.csv")
dog_apps <- read_csv("Data/dog_apps.csv")
```

Actions
-------

Create checklist\_names as a factor for each checklist name combination...

``` r
actions <- rbind(cat_actions, dog_actions)
actions$checklist_names <- ""
actions$checklist_names <- as.character(actions$checklist_names)
actions[actions$checklist_ACCT == TRUE,]$checklist_names <- 
  paste(actions[actions$checklist_ACCT == TRUE,]$checklist_names, "ACCT", sep = "/")
actions[actions$checklist_CHQ == TRUE,]$checklist_names <- 
  paste(actions[actions$checklist_CHQ == TRUE,]$checklist_names, "CHQ", sep = "/")
actions[actions$checklist_LL == TRUE,]$checklist_names <- 
  paste(actions[actions$checklist_LL == TRUE,]$checklist_names, "LL", sep = "/")
actions[actions$checklist_PP == TRUE,]$checklist_names <- 
  paste(actions[actions$checklist_PP == TRUE,]$checklist_names, "PP", sep = "/")
actions[actions$checklist_SPCA == TRUE,]$checklist_names <- 
  paste(actions[actions$checklist_SPCA == TRUE,]$checklist_names, "SPCA", sep = "/")
actions[actions$checklist_TR == TRUE,]$checklist_names <- 
  paste(actions[actions$checklist_TR == TRUE,]$checklist_names, "TR", sep = "/")
actions[actions$checklist_VET == TRUE,]$checklist_names <- 
  paste(actions[actions$checklist_VET == TRUE,]$checklist_names, "VET", sep = "/")
actions[actions$checklist_ACCT == FALSE &
          actions$checklist_CHQ == FALSE &
          actions$checklist_LL == FALSE &
          actions$checklist_PP == FALSE &
          actions$checklist_SPCA == FALSE &
          actions$checklist_TR == FALSE &
          actions$checklist_VET == FALSE,]$checklist_names <- "OTHER"
actions$checklist_names <- trimws(gsub("^/", " ", actions$checklist_names))
actions$checklist_names <- as.factor(actions$checklist_names)
summary(actions$checklist_names)
```

    ##      ACCT ACCT/SPCA       CHQ    CHQ/PP   CHQ/VET        LL     LL/PP 
    ##       152         1      5289         4         1      6728         5 
    ##     OTHER        PP      SPCA        TR       VET 
    ##      7029      6774       121      6753      6122

``` r
ggplot(actions, aes(x = checklist_names, fill = checklist_names)) +
  geom_bar(stat = "count") +
  geom_text(aes(label = ..count..), vjust = -0.25, stat = "count", position = "identity", size = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ animal_type) +
  guides(fill=FALSE)
```

![](amygood_data_exploration_files/figure-markdown_github/actions-1.png)

Make wide version of data (each row corresponds to unique data.card.id)...

-   Why is the sequence numbering so strange? Did I make a mistake?

``` r
actions_complete <- subset(actions, data.checkItem.state == "complete")
wide_actions <- dcast(actions_complete, formula = data.card.id + animal_type ~ checklist_seq_num, value.var = c("checklist_names"))
nrow(wide_actions) == length(unique(actions_complete$data.card.id))
```

    ## [1] TRUE

``` r
wide_actions <- wide_actions %>% mutate_if(is.character, as.factor)
summary(wide_actions)
```

    ##                    data.card.id  animal_type       1              2       
    ##  57acc2f1d4009ea56a2cb2be:   1   cat:4760    PP     : 116   VET    :  27  
    ##  57b0d0a3c473065ae66852c6:   1   dog:1862    TR     : 103   LL     :  13  
    ##  57bf545a4204dda43853983a:   1               VET    :  44   PP     :  12  
    ##  57ed4d88d27ca2801d342858:   1               CHQ    :  24   CHQ    :  10  
    ##  58027874ff46111221ad74ad:   1               LL     :  18   TR     :   9  
    ##  581ce61069899a9f8c04e021:   1               (Other):   6   (Other):   9  
    ##  (Other)                 :6616               NA's   :6311   NA's   :6542  
    ##        3              4           5           6           7       
    ##  VET    :  11   PP     :  10   ACCT:   1   CHQ :   5   ACCT:   1  
    ##  PP     :   9   VET    :   9   CHQ :   5   LL  :   8   CHQ :   2  
    ##  TR     :   9   LL     :   7   LL  :   8   PP  :   2   LL  :   2  
    ##  LL     :   4   TR     :   6   PP  :   3   TR  :   3   TR  :   2  
    ##  CHQ    :   3   CHQ    :   5   TR  :   7   VET :   6   VET :   5  
    ##  (Other):   7   (Other):   2   VET :   7   NA's:6598   NA's:6610  
    ##  NA's   :6579   NA's   :6583   NA's:6591                          
    ##     8           9           10          11             12      
    ##  CHQ :   3   LL  :   2   LL  :   1   CHQ :   1   PP     :2525  
    ##  SPCA:   1   PP  :   1   TR  :   1   NA's:6621   TR     :2403  
    ##  VET :   3   NA's:6619   NA's:6620               LL     : 596  
    ##  NA's:6615                                       VET    : 525  
    ##                                                  CHQ    : 481  
    ##                                                  (Other):  25  
    ##                                                  NA's   :  67  
    ##     13             14             15             16             17      
    ##  LL  :   1   TR     :2257   LL     :2058   LL     :1886   VET    :2288  
    ##  NA's:6621   PP     :1971   CHQ    :1684   VET    :1550   LL     : 720  
    ##              LL     : 946   TR     :1183   CHQ    :1433   CHQ    : 497  
    ##              CHQ    : 923   PP     : 882   PP     : 597   PP     : 317  
    ##              VET    : 322   VET    : 551   TR     : 454   TR     : 138  
    ##              (Other):  17   (Other):  21   (Other):  51   (Other):  98  
    ##              NA's   : 186   NA's   : 243   NA's   : 651   NA's   :2564  
    ##        18             19             20      
    ##  VET    : 292   VET    : 201   VET    :  50  
    ##  LL     : 144   LL     :  85   PP     :  15  
    ##  CHQ    : 100   PP     :  36   CHQ    :  13  
    ##  PP     :  72   CHQ    :  28   LL     :  13  
    ##  ACCT   :  35   TR     :  18   TR     :   8  
    ##  (Other): 101   (Other):  37   (Other):  16  
    ##  NA's   :5878   NA's   :6217   NA's   :6507

``` r
summary(wide_actions$`1`)
```

    ##  ACCT   CHQ    LL OTHER    PP  SPCA    TR   VET  NA's 
    ##     4    24    18     1   116     1   103    44  6311

``` r
summary(wide_actions$`2`)
```

    ##  ACCT   CHQ    LL OTHER    PP  SPCA    TR   VET  NA's 
    ##     2    10    13     2    12     5     9    27  6542

``` r
summary(wide_actions$`3`)
```

    ##  ACCT   CHQ    LL OTHER    PP  SPCA    TR   VET  NA's 
    ##     2     3     4     3     9     2     9    11  6579

``` r
summary(wide_actions$`4`)
```

    ##  ACCT   CHQ    LL OTHER    PP    TR   VET  NA's 
    ##     1     5     7     1    10     6     9  6583

``` r
summary(wide_actions$`5`)
```

    ## ACCT  CHQ   LL   PP   TR  VET NA's 
    ##    1    5    8    3    7    7 6591

``` r
summary(wide_actions$`6`)
```

    ##  CHQ   LL   PP   TR  VET NA's 
    ##    5    8    2    3    6 6598

``` r
summary(wide_actions$`7`)
```

    ## ACCT  CHQ   LL   TR  VET NA's 
    ##    1    2    2    2    5 6610

``` r
summary(wide_actions$`8`)
```

    ##  CHQ SPCA  VET NA's 
    ##    3    1    3 6615

``` r
summary(wide_actions$`9`)
```

    ##   LL   PP NA's 
    ##    2    1 6619

``` r
summary(wide_actions$`10`)
```

    ##   LL   TR NA's 
    ##    1    1 6620

``` r
summary(wide_actions$`11`)
```

    ##  CHQ NA's 
    ##    1 6621

``` r
summary(wide_actions$`12`)
```

    ##   ACCT    CHQ CHQ/PP     LL  OTHER     PP   SPCA     TR    VET   NA's 
    ##      6    481      1    596     13   2525      5   2403    525     67

``` r
summary(wide_actions$`13`)
```

    ##   LL NA's 
    ##    1 6621

``` r
summary(wide_actions$`14`)
```

    ##   ACCT    CHQ CHQ/PP     LL  OTHER     PP   SPCA     TR    VET   NA's 
    ##      4    923      3    946      8   1971      2   2257    322    186

``` r
summary(wide_actions$`15`)
```

    ##  ACCT   CHQ    LL OTHER    PP  SPCA    TR   VET  NA's 
    ##     7  1684  2058     9   882     5  1183   551   243

``` r
summary(wide_actions$`16`)
```

    ##  ACCT   CHQ    LL OTHER    PP  SPCA    TR   VET  NA's 
    ##    29  1433  1886    11   597    11   454  1550   651

Cards
-----

``` r
cards <- rbind(dog_cards, cat_cards)
head(cards)
```

    ## # A tibble: 6 x 6
    ##   id      dateLastActivity dueComplete due        animal_type label_names 
    ##   <chr>   <date>           <lgl>       <date>     <chr>       <chr>       
    ## 1 5a6b7b~ 2018-01-31       FALSE       2018-01-26 dog         ready for r~
    ## 2 5a6d20~ 2018-01-31       TRUE        2018-01-27 dog         ready for r~
    ## 3 5a6a4f~ 2018-01-31       TRUE        2018-01-25 dog         not utd     
    ## 4 5a6533~ 2018-01-31       TRUE        2018-01-21 dog         not utd     
    ## 5 5a673b~ 2018-01-30       FALSE       NA         dog         need info   
    ## 6 5a6d23~ 2018-01-30       FALSE       NA         dog         need vet in~

``` r
length(unique(cards$id)) == nrow(cards)
```

    ## [1] TRUE

Create boolean variables for each label name...

``` r
all_label_names_string <- do.call(paste, c(as.list(cards$label_names), sep = ","))
all_label_names <- unlist(strsplit(all_label_names_string, c(","))) 
label_name_list <- unique(trimws(all_label_names))
label_name_list <- label_name_list[label_name_list != ""]
label_name_list <- label_name_list[label_name_list != "NA"]
sort(label_name_list)
```

    ##  [1] "adopted"                      "adopted elsewhere"           
    ##  [3] "adoption follow up"           "approved"                    
    ##  [5] "approved with limitation"     "checks"                      
    ##  [7] "declaw only"                  "denied"                      
    ##  [9] "do not follow up"             "dog meet"                    
    ## [11] "foster to adopt"              "landlord"                    
    ## [13] "manager decision"             "need info"                   
    ## [15] "need proof of ownership"      "need roommates vet info"     
    ## [17] "need to see id"               "need vet info"               
    ## [19] "need written ll permission"   "needs app attached"          
    ## [21] "needs review before approval" "not s n"                     
    ## [23] "not utd"                      "opa"                         
    ## [25] "pet policy"                   "questions"                   
    ## [27] "ready for review"             "ready to adopt"              
    ## [29] "red flag"                     "rescue check"                
    ## [31] "returned"                     "reviewed with handouts only" 
    ## [33] "serial no show"               "unsure foster or adopt"      
    ## [35] "vet"                          "vet check in process"        
    ## [37] "withdrawn"

``` r
length(label_name_list)
```

    ## [1] 37

``` r
cards <- tidy_paws_labels(cards, "label_names", label_name_list) # see function in helper_functions.R (this is slow and can be improved)
summary(cards)
```

    ##       id            dateLastActivity     dueComplete    
    ##  Length:9989        Min.   :2017-12-01   Mode :logical  
    ##  Class :character   1st Qu.:2018-07-29   FALSE:7258     
    ##  Mode  :character   Median :2018-08-31   TRUE :2731     
    ##                     Mean   :2018-08-15                  
    ##                     3rd Qu.:2018-10-05                  
    ##                     Max.   :2019-01-22                  
    ##                                                         
    ##       due             animal_type        label_names       
    ##  Min.   :2016-08-11   Length:9989        Length:9989       
    ##  1st Qu.:2018-02-14   Class :character   Class :character  
    ##  Median :2018-07-19   Mode  :character   Mode  :character  
    ##  Mean   :2018-05-29                                        
    ##  3rd Qu.:2018-10-12                                        
    ##  Max.   :2019-01-26                                        
    ##  NA's   :2974                                              
    ##  ready_for_review  not_utd        need_info       need_vet_info  
    ##  Mode :logical    Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:8764       FALSE:9637      FALSE:9572      FALSE:9685     
    ##  TRUE :1225       TRUE :352       TRUE :417       TRUE :304      
    ##                                                                  
    ##                                                                  
    ##                                                                  
    ##                                                                  
    ##  questions       ready_to_adopt  withdrawn        adopted       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:9220      FALSE:7935      FALSE:9941      FALSE:8740     
    ##  TRUE :769       TRUE :2054      TRUE :48        TRUE :1249     
    ##                                                                 
    ##                                                                 
    ##                                                                 
    ##                                                                 
    ##   returned       pet_policy      adopted_elsewhere need_to_see_id 
    ##  Mode :logical   Mode :logical   Mode :logical     Mode :logical  
    ##  FALSE:9741      FALSE:9770      FALSE:9818        FALSE:9733     
    ##  TRUE :248       TRUE :219       TRUE :171         TRUE :256      
    ##                                                                   
    ##                                                                   
    ##                                                                   
    ##                                                                   
    ##    denied         red_flag        dog_meet       manager_decision
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical   
    ##  FALSE:9854      FALSE:9465      FALSE:9960      FALSE:9827      
    ##  TRUE :135       TRUE :524       TRUE :29        TRUE :162       
    ##                                                                  
    ##                                                                  
    ##                                                                  
    ##                                                                  
    ##  rescue_check    approved_with_limitation  approved      
    ##  Mode :logical   Mode :logical            Mode :logical  
    ##  FALSE:9910      FALSE:9933               FALSE:9827     
    ##  TRUE :79        TRUE :56                 TRUE :162      
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  reviewed_with_handouts_only    vet             opa         
    ##  Mode :logical               Mode :logical   Mode :logical  
    ##  FALSE:9057                  FALSE:9421      FALSE:9948     
    ##  TRUE :932                   TRUE :568       TRUE :41       
    ##                                                             
    ##                                                             
    ##                                                             
    ##                                                             
    ##    checks        unsure_foster_or_adopt adoption_follow_up
    ##  Mode :logical   Mode :logical          Mode :logical     
    ##  FALSE:9875      FALSE:9979             FALSE:9864        
    ##  TRUE :114       TRUE :10               TRUE :125         
    ##                                                           
    ##                                                           
    ##                                                           
    ##                                                           
    ##  needs_app_attached declaw_only     serial_no_show  foster_to_adopt
    ##  Mode :logical      Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:9988         FALSE:9896      FALSE:9986      FALSE:9983     
    ##  TRUE :1            TRUE :93        TRUE :3         TRUE :6        
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##  needs_review_before_approval do_not_follow_up need_written_ll_permission
    ##  Mode :logical                Mode :logical    Mode :logical             
    ##  FALSE:9987                   FALSE:9985       FALSE:9980                
    ##  TRUE :2                      TRUE :4          TRUE :9                   
    ##                                                                          
    ##                                                                          
    ##                                                                          
    ##                                                                          
    ##  need_proof_of_ownership  not_s_n         landlord      
    ##  Mode :logical           Mode :logical   Mode :logical  
    ##  FALSE:9977              FALSE:9984      FALSE:9980     
    ##  TRUE :12                TRUE :5         TRUE :9        
    ##                                                         
    ##                                                         
    ##                                                         
    ##                                                         
    ##  vet_check_in_process need_roommates_vet_info
    ##  Mode :logical        Mode :logical          
    ##  FALSE:9985           FALSE:9984             
    ##  TRUE :4              TRUE :5                
    ##                                              
    ##                                              
    ##                                              
    ## 

``` r
cards_labels <- cards[!names(cards) %in% c("id", "dateLastActivity", "dueComplete", "due", "animal_type", "label_names")]
cards_labels_summary <- as.data.frame(colSums(cards_labels))
cards_labels_summary$label <- rownames(cards_labels_summary)
colnames(cards_labels_summary)[1] <- "label_count"
ggplot(cards_labels_summary, aes(x = fct_reorder(label, label_count), y = label_count, fill = label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label_count), vjust = -0.25, position = "identity", size = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)
```

![](amygood_data_exploration_files/figure-markdown_github/cards_2-1.png)

Apps
----

``` r
apps <- rbind(cat_apps, dog_apps)
apps$outcome_trello_id <- as.factor(apps$outcome_trello_id)
apps_with_trello_id <- subset(apps, !is.na(apps$outcome_trello_id))
nrow(apps_with_trello_id)
```

    ## [1] 1631

``` r
length(unique(apps_with_trello_id$outcome_trello_id)) == nrow(apps_with_trello_id)
```

    ## [1] FALSE

``` r
length(unique(apps_with_trello_id$outcome_trello_id)) # some duplicates
```

    ## [1] 1593
