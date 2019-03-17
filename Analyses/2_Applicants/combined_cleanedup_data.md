combined\_dataset
================
Veena
3/16/2019

-   [Load Data](#load-data)
-   [Helper Functions](#helper-functions)
-   [Dataset Cleanup](#dataset-cleanup)
    -   [Apps Data](#apps-data)
    -   [Actions Dataset](#actions-dataset)
    -   [Petpoint Dataset](#petpoint-dataset)
    -   [Cards Dataset](#cards-dataset)
-   [Merge the 4 Datasets](#merge-the-4-datasets)

Load Data
---------

Helper Functions
----------------

convert\_to\_ind(): creates indicator for specified field in dataframe

``` r
#functions provided by Ramaa
# create separate dataframe with indicator columns - one for each option of the provided categorical field
convert_to_ind <- function(df, field){
    df %>% 
        mutate_(var = field) %>% 
        distinct(trello_id, var) %>% 
        unnest(split = str_split(str_trim(var), ",")) %>%
        select(-var) %>% 
        filter(!is.na(split)) %>% 
        mutate(n = 1,
               split = 
                   str_replace_all(split, "-", ".") %>% 
                   str_replace_all(., " ", ".") %>%
                   paste0(str_replace_all(field, "_", "."), 
                          "_", ., "_ind")) %>%
        distinct() %>% 
        spread(split, n, fill = 0)
}

clean_adoption_timeline <- function(x) {
  x %>% str_replace_all(.,"next-few-weeks","few-weeks")
}

clean_household_agree <- function(x) {
  x %>% str_replace_all(.,"it-s-a-surprise","a-surprise") %>% 
    str_replace_all(.,"yes,no","no,yes") %>%
    str_replace_all(.,"a-surprise,yes","yes,a-surprise")
}

clean_pet_policy <- function(x) {
  x %>% str_replace_all(.,"no-yet","not-yet") %>%
    str_replace_all(.,"havent-asked","not-yet") %>%
    str_replace_all(.,"n-a","not-applicable")
}

# clean the comma-separated fields
clean_experience <- function(x){
    x %>%
        str_replace_all(., "(grew-up-with)(-pet)", "\\1") %>% 
        #str_replace_all(., "(currently)(-pet)", "\\1") %>% 
        str_replace_all(., "(euthanized)[^,]+", "\\1") %>% 
        str_replace_all(., "had-pet-die", "pet-died-in-care") %>% 
        str_replace_all(., "[^,]*(lived-with-housemate|lived-wit-prev)[^,]*", "past-housemates-pet") %>% 
        str_replace_all(., "currently-pets[^,]*", "current-housemates-pet") %>% 
        str_replace_all(., "(never-lived-)[^,]+", "\\1with") %>% 
        str_replace_all(., "(given-)[^,]*shelter", "\\1to-shelter") %>% 
        str_replace_all(., "(given-)(pet-)?(to-another)", "\\1away") %>% 
        str_replace_all(., "(bred-sold)-a-pet", "bred-sold")
}

clean_budget <- function(x) {
  x %>% str_replace_all(.,"^-","") %>%
    parse_number(.)
}

clean_pet_kept <- function(x) {
  x %>% str_replace_all(.,"unsupervised-access-to-my-yard-9doggie-door-etc","unsupervised-access-to-my-yard-doggie-door-etc")
}

#function provided by Veena
clean_city <- function(colname) {
  colname %>% toupper(.) %>%
  gsub("[.]|[,]| PA$", "", .) %>%
  gsub("  ", " ", .) %>%
  gsub("MT ", "MOUNT ", .) %>%
  gsub("19010", "BRYN MAWR", .) %>%
  gsub("CHETSER", "CHESTER", .) %>%
  gsub("ROYERFORD", "ROYERSFORD", .) %>%
  gsub("NORTH WHALES", "NORTH WALES", .) %>%
  gsub("MONTGOMERY VALLAGE", "MONTGOMERY VILLAGE", .) %>%
  gsub("E LANSDOWNE", "EAST LANSDOWNE", .) %>%
  gsub("PHILLY|FILADELFIA|PHILIDELPHIA|PHIMADELPHIA|PHIALADELPHIA|PHIALDELPHIA|PHILDELPHIA", "PHILADELPHIA", .)
}
```

Dataset Cleanup
---------------

### Apps Data

The cat\_apps.csv and dog\_apps.csv have exactly same columns but there is a mismatch in their datatypes. <br><br>Steps to fix this: <br>1. Add animal\_type="cats" to cats.csv and animal\_type="dogs" to dogs.csv <br>2. cat\_apps.csv - convert adults\_in \_home from character type to integer type <br>3. dog\_apps.csv - convert zip code from integer type to character type so that we can categorize them

combined dog and cat apps data \[1906, 27\]

``` r
#convert adults_in_home to integer type
cat_apps <- cat_apps[,-1] %>% 
  distinct() %>%
  transform(adults_in_home = as.numeric(adults_in_home)) %>%
  mutate(animal_type="cats",
         ZIP=ifelse(str_length(ZIP)<5,str_c("0",ZIP),ZIP))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs
    ## introduced by coercion

``` r
#convert ZIP to character type and pad it correctly
dog_apps <- dog_apps[,-1] %>% 
  distinct() %>% 
  transform(ZIP = as.character(ZIP)) %>%
  mutate(animal_type="dogs",
         ZIP=ifelse(str_length(ZIP)<5,str_c("0",ZIP),ZIP))          

#combine cat and dog apps data
apps <- bind_rows(cat_apps,dog_apps) 
```

Apps data needs to be cleaned up: <br> \* Removed columns: STATEFP,COUNTYFP,TRACTCE,GEOID,NAME,NAMELSAD,MTFCC,FUNCSTAT,ALAND,AWATER,INTPTLAT,INTPTLON <br> \* Take absolute value of negative numbers <br> \* Any values &gt; 15 for adults\_in\_home and children\_in\_home are updated N/A <br> \* ideal\_adoption\_timeline: consolidated next-few-weeks to few-weeks <br> \* all\_household\_agree: consolidated using the function clean\_household\_agree <br> \* home\_owner and home\_pet\_policy: make factor <br> \* experience: cleaned using function above <br> \* City: cleaned to reduce same cities with different spellings

``` r
apps <- apps %>% 
  select(-c(STATEFP,COUNTYFP,TRACTCE,GEOID,NAME,NAMELSAD,MTFCC,FUNCSTAT,ALAND,AWATER,INTPTLAT,INTPTLON)) %>%
  rename(trello_id = outcome_trello_id) %>%
  mutate(date_submitted = mdy(date_submitted),
         ideal_adoption_timeline = clean_adoption_timeline(ideal_adoption_timeline),
         all_household_agree = clean_household_agree(all_household_agree),
         home_pet_policy = clean_pet_policy(home_pet_policy),
         home_pet_policy = as.factor(home_pet_policy),
         home_owner = as.factor(home_owner),
         experience = clean_experience(experience),
         pet_kept = clean_pet_kept(pet_kept),
         adults_in_home = abs(adults_in_home),
         adults_in_home = replace(adults_in_home, adults_in_home > 15,NA),
         children_in_home = abs(children_in_home), #remove negative numbers
         children_in_home = replace(children_in_home, children_in_home > 15,NA), #remove any numbers greater than 15
         home_alone_avg = parse_number(home_alone_avg),
         home_alone_max = parse_number(home_alone_max),
         budget_monthly = clean_budget(budget_monthly),
         budget_emergency = clean_budget(budget_emergency),
         budget_monthly_ranges = factor(case_when(budget_monthly <=25 ~ "Less than $25",
                                             budget_monthly <=100 ~ "$25-$100",
                                             budget_monthly <=200 ~ "$100-$200",
                                             budget_monthly <=500 ~ "$200-$500",
                                             budget_monthly <=1000 ~ "$500-$1000",
                                             budget_monthly <=5000 ~ "$1000-$5000",
                                             is.na(budget_monthly) ~ "NA",
                                             TRUE ~ ">$5000"),
                                   levels=c("<$25","$25-$100","$100-$200","$200-$500","$500-$1000","$1000-$5000",">$5000","NA"),
                                   ordered=T),
         budget_emergency_ranges = factor(case_when(budget_emergency <=25 ~ "Less than $25",
                                             budget_emergency  <=100 ~ "$25-$100",
                                             budget_emergency  <=200 ~ "$100-$200",
                                             budget_emergency  <=500 ~ "$200-$500",
                                             budget_emergency  <=1000 ~ "$500-$1000",
                                             budget_emergency  <=5000 ~ "$1000-$5000",
                                             is.na(budget_emergency ) ~ "NA",
                                             TRUE ~ ">$5000"),
                                   levels=c("<$25","$25-$100","$100-$200","$200-$500","$500-$1000","$1000-$5000",">$5000","NA"),
                                   ordered=T))
#Cleanup city column
apps$City = clean_city(apps$City)
apps$City = replace(apps$City, apps$City %in% c("Y"),NA)
apps$City = as.factor(apps$City)

#Make State factor
apps$State <- as.factor(apps$State)
         
#only extract zip codes with 5 values
apps$ZIP <- str_extract(apps$ZIP, "^.{5}")

apps_with_indicators <- apps %>%
   #distinct(trello_id) %>%
   left_join(convert_to_ind(apps,"reason_for_adoption")) %>%
   left_join(convert_to_ind(apps,"all_household_agree")) %>%
   left_join(convert_to_ind(apps,"allergies")) %>%
   left_join(convert_to_ind(apps,"home_owner")) %>%
   left_join(convert_to_ind(apps,"home_pet_policy")) %>%
   left_join(convert_to_ind(apps,"experience")) %>%
   left_join(convert_to_ind(apps,"budget_monthly_ranges")) %>%
   left_join(convert_to_ind(apps,"budget_emergency_ranges")) %>%
   left_join(convert_to_ind(apps,"home_alone_avg")) %>%
   left_join(convert_to_ind(apps,"home_alone_max")) %>%
   left_join(convert_to_ind(apps,"pet_kept")) %>%
   left_join(convert_to_ind(apps,"exercise")) %>%
   left_join(convert_to_ind(apps,"needs")) %>%
   left_join(convert_to_ind(apps,"return_pet"))
```

    ## Warning: mutate_() is deprecated. 
    ## Please use mutate() instead
    ## 
    ## The 'programming' vignette or the tidyeval book can help you
    ## to program with mutate() : https://tidyeval.tidyverse.org
    ## This warning is displayed once per session.

``` r
dim(apps)
```

    ## [1] 1906   27

``` r
dim(apps_with_indicators)
```

    ## [1] 1906  158

``` r
colnames(apps_with_indicators)
```

    ##   [1] "date_submitted"                                             
    ##   [2] "ideal_adoption_timeline"                                    
    ##   [3] "reason_for_adoption"                                        
    ##   [4] "specific_animal"                                            
    ##   [5] "adults_in_home"                                             
    ##   [6] "children_in_home"                                           
    ##   [7] "all_household_agree"                                        
    ##   [8] "allergies"                                                  
    ##   [9] "home_owner"                                                 
    ##  [10] "home_pet_policy"                                            
    ##  [11] "experience"                                                 
    ##  [12] "budget_monthly"                                             
    ##  [13] "budget_emergency"                                           
    ##  [14] "home_alone_avg"                                             
    ##  [15] "home_alone_max"                                             
    ##  [16] "pet_kept"                                                   
    ##  [17] "exercise"                                                   
    ##  [18] "needs"                                                      
    ##  [19] "return_pet"                                                 
    ##  [20] "how_heard"                                                  
    ##  [21] "trello_id"                                                  
    ##  [22] "City"                                                       
    ##  [23] "State"                                                      
    ##  [24] "ZIP"                                                        
    ##  [25] "animal_type"                                                
    ##  [26] "budget_monthly_ranges"                                      
    ##  [27] "budget_emergency_ranges"                                    
    ##  [28] "reason.for.adoption_gift_ind"                               
    ##  [29] "reason.for.adoption_mouser_ind"                             
    ##  [30] "reason.for.adoption_my.kids_ind"                            
    ##  [31] "reason.for.adoption_myself_ind"                             
    ##  [32] "reason.for.adoption_other_ind"                              
    ##  [33] "reason.for.adoption_protection_ind"                         
    ##  [34] "all.household.agree_a.surprise_ind"                         
    ##  [35] "all.household.agree_no_ind"                                 
    ##  [36] "all.household.agree_yes_ind"                                
    ##  [37] "allergies_mildly.allergic_ind"                              
    ##  [38] "allergies_no.allergies_ind"                                 
    ##  [39] "allergies_not.sure_ind"                                     
    ##  [40] "allergies_very.allergic_ind"                                
    ##  [41] "home.owner_company_ind"                                     
    ##  [42] "home.owner_family.friend_ind"                               
    ##  [43] "home.owner_family.member.or.friend_ind"                     
    ##  [44] "home.owner_landlord_ind"                                    
    ##  [45] "home.owner_myself_ind"                                      
    ##  [46] "home.pet.policy_no.but.pets.allowed_ind"                    
    ##  [47] "home.pet.policy_not.applicable_ind"                         
    ##  [48] "home.pet.policy_not.yet_ind"                                
    ##  [49] "home.pet.policy_yes_ind"                                    
    ##  [50] "home.pet.policy_yes.with.pet.policy_ind"                    
    ##  [51] "experience_bred.sold_ind"                                   
    ##  [52] "experience_current.housemates.pet_ind"                      
    ##  [53] "experience_currently.have.pet_ind"                          
    ##  [54] "experience_euthanized_ind"                                  
    ##  [55] "experience_given.away_ind"                                  
    ##  [56] "experience_given.to.shelter_ind"                            
    ##  [57] "experience_grew.up.with_ind"                                
    ##  [58] "experience_never.lived.with_ind"                            
    ##  [59] "experience_past.housemates.pet_ind"                         
    ##  [60] "experience_pet.died.in.care_ind"                            
    ##  [61] "experience_pet.ran.away_ind"                                
    ##  [62] "budget.monthly.ranges_>$5000_ind"                           
    ##  [63] "budget.monthly.ranges_$100.$200_ind"                        
    ##  [64] "budget.monthly.ranges_$1000.$5000_ind"                      
    ##  [65] "budget.monthly.ranges_$200.$500_ind"                        
    ##  [66] "budget.monthly.ranges_$25.$100_ind"                         
    ##  [67] "budget.monthly.ranges_$500.$1000_ind"                       
    ##  [68] "budget.emergency.ranges_>$5000_ind"                         
    ##  [69] "budget.emergency.ranges_$100.$200_ind"                      
    ##  [70] "budget.emergency.ranges_$1000.$5000_ind"                    
    ##  [71] "budget.emergency.ranges_$200.$500_ind"                      
    ##  [72] "budget.emergency.ranges_$25.$100_ind"                       
    ##  [73] "budget.emergency.ranges_$500.$1000_ind"                     
    ##  [74] "home.alone.avg_0_ind"                                       
    ##  [75] "home.alone.avg_1_ind"                                       
    ##  [76] "home.alone.avg_10_ind"                                      
    ##  [77] "home.alone.avg_11_ind"                                      
    ##  [78] "home.alone.avg_12_ind"                                      
    ##  [79] "home.alone.avg_13_ind"                                      
    ##  [80] "home.alone.avg_2_ind"                                       
    ##  [81] "home.alone.avg_24_ind"                                      
    ##  [82] "home.alone.avg_3_ind"                                       
    ##  [83] "home.alone.avg_4_ind"                                       
    ##  [84] "home.alone.avg_5_ind"                                       
    ##  [85] "home.alone.avg_6_ind"                                       
    ##  [86] "home.alone.avg_7_ind"                                       
    ##  [87] "home.alone.avg_8_ind"                                       
    ##  [88] "home.alone.avg_9_ind"                                       
    ##  [89] "home.alone.max_0_ind"                                       
    ##  [90] "home.alone.max_1_ind"                                       
    ##  [91] "home.alone.max_10_ind"                                      
    ##  [92] "home.alone.max_11_ind"                                      
    ##  [93] "home.alone.max_12_ind"                                      
    ##  [94] "home.alone.max_13_ind"                                      
    ##  [95] "home.alone.max_14_ind"                                      
    ##  [96] "home.alone.max_15_ind"                                      
    ##  [97] "home.alone.max_16_ind"                                      
    ##  [98] "home.alone.max_18_ind"                                      
    ##  [99] "home.alone.max_2_ind"                                       
    ## [100] "home.alone.max_20_ind"                                      
    ## [101] "home.alone.max_23_ind"                                      
    ## [102] "home.alone.max_24_ind"                                      
    ## [103] "home.alone.max_28_ind"                                      
    ## [104] "home.alone.max_3_ind"                                       
    ## [105] "home.alone.max_30_ind"                                      
    ## [106] "home.alone.max_36_ind"                                      
    ## [107] "home.alone.max_4_ind"                                       
    ## [108] "home.alone.max_48_ind"                                      
    ## [109] "home.alone.max_5_ind"                                       
    ## [110] "home.alone.max_6_ind"                                       
    ## [111] "home.alone.max_7_ind"                                       
    ## [112] "home.alone.max_8_ind"                                       
    ## [113] "home.alone.max_9_ind"                                       
    ## [114] "pet.kept_crate_ind"                                         
    ## [115] "pet.kept_inside.only_ind"                                   
    ## [116] "pet.kept_inside.outside_ind"                                
    ## [117] "pet.kept_inside.with.yard.access_ind"                       
    ## [118] "pet.kept_leash.harness_ind"                                 
    ## [119] "pet.kept_other_ind"                                         
    ## [120] "pet.kept_outside.only_ind"                                  
    ## [121] "pet.kept_supervised.in.my.yard_ind"                         
    ## [122] "pet.kept_unsupervised.access.to.my.yard.doggie.door.etc_ind"
    ## [123] "exercise_another.pet_ind"                                   
    ## [124] "exercise_dog.parks_ind"                                     
    ## [125] "exercise_jogging.together_ind"                              
    ## [126] "exercise_not.much_ind"                                      
    ## [127] "exercise_other.cats_ind"                                    
    ## [128] "exercise_other.pets_ind"                                    
    ## [129] "exercise_playing.in.my.yard_ind"                            
    ## [130] "exercise_toy.mice_ind"                                      
    ## [131] "exercise_walks.off.leash_ind"                               
    ## [132] "exercise_walks.on.leash_ind"                                
    ## [133] "exercise_wand.toys_ind"                                     
    ## [134] "needs_declaw_ind"                                           
    ## [135] "needs_groom.myself_ind"                                     
    ## [136] "needs_nail.clip_ind"                                        
    ## [137] "needs_no.grooming_ind"                                      
    ## [138] "needs_not.sure_ind"                                         
    ## [139] "needs_other_ind"                                            
    ## [140] "needs_professional.groomer_ind"                             
    ## [141] "needs_scratching.post_ind"                                  
    ## [142] "return.pet_allergies.appear_ind"                            
    ## [143] "return.pet_becomes.aggressive_ind"                          
    ## [144] "return.pet_destructive_ind"                                 
    ## [145] "return.pet_jumps.on.counters_ind"                           
    ## [146] "return.pet_jumps.on.furniture_ind"                          
    ## [147] "return.pet_litter.box.issues_ind"                           
    ## [148] "return.pet_moving.too.far_ind"                              
    ## [149] "return.pet_new.baby_ind"                                    
    ## [150] "return.pet_none_ind"                                        
    ## [151] "return.pet_not.allowed.new.living.space_ind"                
    ## [152] "return.pet_not.enough.time_ind"                             
    ## [153] "return.pet_not.housebroken_ind"                             
    ## [154] "return.pet_other_ind"                                       
    ## [155] "return.pet_pet.sheds_ind"                                   
    ## [156] "return.pet_scratches.furniture_ind"                         
    ## [157] "return.pet_too.playful_ind"                                 
    ## [158] "return.pet_vet.becomes.expensive_ind"

``` r
all(duplicated(apps_with_indicators) == FALSE) # check for duplicates
```

    ## [1] TRUE

``` r
#dput(get_unique_elements(apps_with_indicators, "experience")) #check unique values
#length(get_unique_elements(apps_with_indicators, "City"))
```

### Actions Dataset

``` r
#create one dataframe
actions <- bind_rows(unique(cat_actions),unique(dog_actions))

actions <-
    actions %>% distinct() %>% 
    #mutate (animal_type=factor(animal_type)) %>%
    rename(trello_id = data.card.id) %>%
    gather(item, result, checklist_ACCT:checklist_VET) %>%
    group_by(trello_id) %>% 
    mutate(date_start = min(date)) %>% 
    filter(result == TRUE) %>%
    group_by(trello_id, item) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(wait = difftime(date, date_start, units = "days"),
           wait = round(as.numeric(wait), 2)) %>%
    select(-c(date, data.checkItem.state, type, result)) %>%
    distinct() %>%
    spread(item, wait) %>% 
    mutate(wday_start = wday(date_start, label = TRUE, abbr = TRUE))
str(actions)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    6622 obs. of  11 variables:
    ##  $ trello_id     : chr  "57acc2f1d4009ea56a2cb2be" "57b0d0a3c473065ae66852c6" "57bf545a4204dda43853983a" "57ed4d88d27ca2801d342858" ...
    ##  $ animal_type   : chr  "dog" "cat" "cat" "cat" ...
    ##  $ date_start    : POSIXct, format: "2018-07-02 17:11:01" "2018-08-29 18:22:52" ...
    ##  $ checklist_ACCT: num  NA NA NA NA NA NA NA 1.13 0 NA ...
    ##  $ checklist_CHQ : num  0 0.02 0 0 NA 0 0 0 NA 0 ...
    ##  $ checklist_LL  : num  0 0 0 0 NA 0 0 NA NA NA ...
    ##  $ checklist_PP  : num  0 0.02 0 0 0 0 0 NA NA NA ...
    ##  $ checklist_SPCA: num  NA NA NA NA NA NA NA 0.13 1.75 NA ...
    ##  $ checklist_TR  : num  0 0 0 0 NA 0 0 NA NA NA ...
    ##  $ checklist_VET : num  0.01 0.94 0 NA NA NA 7.57 NA NA 0.32 ...
    ##  $ wday_start    : Ord.factor w/ 7 levels "Sun"<"Mon"<"Tue"<..: 2 4 3 1 1 1 6 6 5 2 ...

### Petpoint Dataset

``` r
petpoint <- petpoint[,-1] %>% #there are 77 duplicates
  distinct() %>%
  filter(animal_type != "Wildlife") %>%
  select(-c(age_group,STATEFP,COUNTYFP,TRACTCE,GEOID,NAME,NAMELSAD,MTFCC,FUNCSTAT,ALAND,AWATER,INTPTLAT,INTPTLON)) %>%
  rename(trello_id = outcome_trello_id) %>%
  mutate(dob=mdy(dob),
         intake_date=mdy_hm(intake_date,tz="America/New_York"),
         release_date=mdy_hm(release_date,tz="America/New_York"),
         outcome_date=mdy_hm(outcome_date,tz="America/New_York"),
         outcome_ZIP=as.character(outcome_ZIP),
         outcome_ZIP=ifelse(str_length(outcome_ZIP)<5,str_c("0",outcome_ZIP),outcome_ZIP),
         new_age_group = factor(case_when(age_intake<=1 ~ "<4 weeks",
                                   age_intake <= 3 ~ "4-12 weeks",
                                   age_intake <= 6 ~ "12weeks-6months",
                                   age_intake <= 12 ~ "6months-1year",
                                   age_intake <= 24 ~ "1-2years",
                                   age_intake <= 48 ~ "2-4years",
                                   age_intake <= 72 ~ "4-6years",
                                   age_intake <= 120 ~ "6-10years",
                                   is.na(age_intake) ~ "NA",
                                   TRUE ~ "older than 10years"),
                                levels=c("<4 weeks","4-12 weeks","12weeks-6months",
                                         "6months-1year","1-2years","2-4years",
                                         "4-6years","6-10years","older than 10years","NA"),
                                ordered=TRUE),
         process_time = (interval(intake_date,outcome_date) / ddays(1)),
         process_time_periods = cut(process_time,
                                    breaks=c(-Inf,1,3,5,10,30,90,180,Inf),
                                    labels=c("< 1day","2-3 days","4-5 days","6-10 days", "11-30 days", "31-90 days", "91-180days", ">180 days"))
         ) 

#Spread out the new_group data into different columns
  petpoint_with_indicators <- petpoint %>%
   #distinct(trello_id) %>%
   left_join(convert_to_ind(petpoint,"new_age_group"))
    
  str(petpoint)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    2940 obs. of  30 variables:
    ##  $ animal_type         : chr  "Cat" "Cat" "Cat" "Cat" ...
    ##  $ species             : chr  "Cat" "Cat" "Cat" "Cat" ...
    ##  $ primary_breed       : chr  "Domestic Shorthair" "Domestic Shorthair" "Domestic Shorthair" "Domestic Shorthair" ...
    ##  $ secondary_breed     : chr  "Mix" "Mix" "Mix" "Mix" ...
    ##  $ markings            : logi  NA NA NA NA NA NA ...
    ##  $ gender              : chr  "F" "F" "M" "M" ...
    ##  $ altered             : chr  "Yes" "Yes" "Yes" "Yes" ...
    ##  $ dob                 : Date, format: "2006-10-18" "2007-05-16" ...
    ##  $ age_intake          : num  138 139 141 130 174 125 140 126 121 101 ...
    ##  $ intake_asilomar     : chr  "Healthy" NA "Treatable-Rehabilitatable" NA ...
    ##  $ intake_condition    : chr  "Healthy" "Healthy" "Sick" "Healthy" ...
    ##  $ intake_date         : POSIXct, format: "2018-05-02 13:17:00" "2019-01-03 11:33:00" ...
    ##  $ intake_type         : chr  "Return" "Transfer In" "Transfer In" "Transfer In" ...
    ##  $ intake_subtype      : chr  "Returned Adoption" "Partner Transfer In" "Partner Transfer In" "Partner Transfer In" ...
    ##  $ intake_reason       : chr  "Divorce / Separation" "Rescue" "Rescue" "Rescue" ...
    ##  $ intake_sitename     : chr  "Grays Ferry Avenue" "Grays Ferry Avenue" "Grays Ferry Avenue" "Grays Ferry Avenue" ...
    ##  $ agency_name         : chr  NA "ACCT Philly" "ACCT Philly" "ACCT Philly" ...
    ##  $ outcome_asilomar    : chr  "Treatable-Manageable" NA NA "Healthy" ...
    ##  $ release_date        : POSIXct, format: "2018-05-14 11:23:00" "2019-01-18 16:05:00" ...
    ##  $ outcome_date        : POSIXct, format: "2018-05-14 11:23:00" "2019-01-18 16:05:00" ...
    ##  $ outcome_type        : chr  "Adoption" "Adoption" "Euthanasia" "Adoption" ...
    ##  $ outcome_subtype     : chr  "Grays Ferry Avenue" "Grays Ferry Avenue" "45 Non Treatable Behavior" "Kawaii Kitty Cafe" ...
    ##  $ outcome_sitename    : chr  "Grays Ferry Avenue" "Grays Ferry Avenue" "Grays Ferry Avenue" "PAWS Offsite Adoptions" ...
    ##  $ trello_id           : chr  "5af1852ab89bd9b467036638" "5c3a6fc172fbfa3471d005e4" NA "5be891e33813143141259a9c" ...
    ##  $ outcome_city        : chr  "MOORESTOWN" "PHILADELPHIA" NA "PHILADELPHIA" ...
    ##  $ outcome_state       : chr  "NJ" "PA" NA "PA" ...
    ##  $ outcome_ZIP         : chr  "08057" "19146" NA "19131" ...
    ##  $ new_age_group       : Ord.factor w/ 10 levels "<4 weeks"<"4-12 weeks"<..: 9 9 9 9 9 9 9 9 9 8 ...
    ##  $ process_time        : num  11.92 15.19 4.78 111.07 1 ...
    ##  $ process_time_periods: Factor w/ 8 levels "< 1day","2-3 days",..: 5 5 3 7 2 2 1 6 5 2 ...

### Cards Dataset

``` r
# Add a new column "animal_type" to each dataset
cat_cards <- cat_cards %>% mutate(animal_type="cats");
dog_cards <- dog_cards %>% mutate(animal_type="dogs");
#combine
cards <- bind_rows(cat_cards,dog_cards)

#dueComplete has been found to be unreliable - so remove it
cards <- cards %>% select(-dueComplete) %>%
  rename(trello_id = id) %>%
  mutate (last_label = sapply(cards$label_names, FUN=function(x)
            unlist(
              str_trim(
                tail(
                  str_split(x,",")[[1]],
                  1)))),
          num_labels = sapply(cards$label_names, FUN=function(x)
            ifelse(is.na(x),0,length(str_split(x,",")[[1]]))))
str(cards)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 9989 obs. of  7 variables:
    ##  $ trello_id       : chr  "5a6d054eea0086b8d596b209" "5a6e1189899dc1ca5caff197" "5a6e338962147c1a458086c2" "5a709fb3ced1c38cc2a08bbe" ...
    ##  $ dateLastActivity: Date, format: "2018-01-31" "2018-01-31" ...
    ##  $ due             : Date, format: "2018-01-27" "2018-01-28" ...
    ##  $ animal_type     : chr  "cats" "cats" "cats" "cats" ...
    ##  $ label_names     : chr  "ready to adopt" "vet" "ready for review" "ready for review, declaw only" ...
    ##  $ last_label      : Named chr  "ready to adopt" "vet" "ready for review" "declaw only" ...
    ##   ..- attr(*, "names")= chr  "ready to adopt" "vet" "ready for review" "ready for review, declaw only" ...
    ##  $ num_labels      : Named num  1 1 1 2 1 1 1 1 2 1 ...
    ##   ..- attr(*, "names")= chr  "ready to adopt" "vet" "ready for review" "ready for review, declaw only" ...

``` r
cards_with_indicators <- cards %>%
   #distinct(trello_id) %>%
   left_join(convert_to_ind(cards,"label_names"))

cards_with_indicators %>% sample_n(10) %>% select(contains("label"))
```

    ## # A tibble: 10 x 74
    ##    label_names last_label num_labels label.names_.ad… label.names_.ad…
    ##    <chr>       <chr>           <dbl>            <dbl>            <dbl>
    ##  1 adopted     adopted             1                0                0
    ##  2 <NA>        <NA>                0               NA               NA
    ##  3 ready to a… ready to …          1                0                0
    ##  4 reviewed w… ready to …          2                0                0
    ##  5 pet policy… ready for…          2                0                0
    ##  6 ready for … ready to …          2                0                0
    ##  7 <NA>        <NA>                0               NA               NA
    ##  8 ready to a… reviewed …          3                0                0
    ##  9 adopted     adopted             1                0                0
    ## 10 <NA>        <NA>                0               NA               NA
    ## # … with 69 more variables: label.names_.adoption.follow.up_ind <dbl>,
    ## #   label.names_.approved_ind <dbl>,
    ## #   label.names_.approved.with.limitation_ind <dbl>,
    ## #   label.names_.checks_ind <dbl>, label.names_.declaw.only_ind <dbl>,
    ## #   label.names_.denied_ind <dbl>,
    ## #   label.names_.do.not.follow.up_ind <dbl>,
    ## #   label.names_.dog.meet_ind <dbl>,
    ## #   label.names_.foster.to.adopt_ind <dbl>,
    ## #   label.names_.landlord_ind <dbl>,
    ## #   label.names_.manager.decision_ind <dbl>,
    ## #   label.names_.need.info_ind <dbl>,
    ## #   label.names_.need.proof.of.ownership_ind <dbl>,
    ## #   label.names_.need.roommates.vet.info_ind <dbl>,
    ## #   label.names_.need.to.see.id_ind <dbl>,
    ## #   label.names_.need.vet.info_ind <dbl>,
    ## #   label.names_.need.written.ll.permission_ind <dbl>,
    ## #   label.names_.needs.app.attached_ind <dbl>,
    ## #   label.names_.needs.review.before.approval_ind <dbl>,
    ## #   label.names_.not.s.n_ind <dbl>, label.names_.not.utd_ind <dbl>,
    ## #   label.names_.opa_ind <dbl>, label.names_.pet.policy_ind <dbl>,
    ## #   label.names_.questions_ind <dbl>,
    ## #   label.names_.ready.for.review_ind <dbl>,
    ## #   label.names_.ready.to.adopt_ind <dbl>,
    ## #   label.names_.red.flag_ind <dbl>, label.names_.rescue.check_ind <dbl>,
    ## #   label.names_.returned_ind <dbl>,
    ## #   label.names_.reviewed.with.handouts.only_ind <dbl>,
    ## #   label.names_.serial.no.show_ind <dbl>,
    ## #   label.names_.unsure.foster.or.adopt_ind <dbl>,
    ## #   label.names_.vet_ind <dbl>,
    ## #   label.names_.vet.check.in.process_ind <dbl>,
    ## #   label.names_.withdrawn_ind <dbl>, label.names_adopted_ind <dbl>,
    ## #   label.names_adopted.elsewhere_ind <dbl>,
    ## #   label.names_adoption.follow.up_ind <dbl>,
    ## #   label.names_approved_ind <dbl>,
    ## #   label.names_approved.with.limitation_ind <dbl>,
    ## #   label.names_checks_ind <dbl>, label.names_declaw.only_ind <dbl>,
    ## #   label.names_denied_ind <dbl>, label.names_foster.to.adopt_ind <dbl>,
    ## #   label.names_landlord_ind <dbl>,
    ## #   label.names_manager.decision_ind <dbl>,
    ## #   label.names_need.info_ind <dbl>,
    ## #   label.names_need.proof.of.ownership_ind <dbl>,
    ## #   label.names_need.roommates.vet.info_ind <dbl>,
    ## #   label.names_need.to.see.id_ind <dbl>,
    ## #   label.names_need.vet.info_ind <dbl>,
    ## #   label.names_need.written.ll.permission_ind <dbl>,
    ## #   label.names_needs.review.before.approval_ind <dbl>,
    ## #   label.names_not.s.n_ind <dbl>, label.names_not.utd_ind <dbl>,
    ## #   label.names_opa_ind <dbl>, label.names_pet.policy_ind <dbl>,
    ## #   label.names_questions_ind <dbl>,
    ## #   label.names_ready.for.review_ind <dbl>,
    ## #   label.names_ready.to.adopt_ind <dbl>, label.names_red.flag_ind <dbl>,
    ## #   label.names_rescue.check_ind <dbl>, label.names_returned_ind <dbl>,
    ## #   label.names_reviewed.with.handouts.only_ind <dbl>,
    ## #   label.names_serial.no.show_ind <dbl>,
    ## #   label.names_unsure.foster.or.adopt_ind <dbl>,
    ## #   label.names_vet_ind <dbl>, label.names_vet.check.in.process_ind <dbl>,
    ## #   label.names_withdrawn_ind <dbl>

Merge the 4 Datasets
--------------------

``` r
combine_data <- apps_with_indicators %>%
  left_join(actions) %>%
  left_join(petpoint_with_indicators) %>%
  left_join(cards_with_indicators)            
```

    ## Joining, by = c("trello_id", "animal_type")
    ## Joining, by = c("trello_id", "animal_type")
    ## Joining, by = c("trello_id", "animal_type")

``` r
dim(combine_data)
```

    ## [1] 1906  281

``` r
colnames(combine_data)
```

    ##   [1] "date_submitted"                                             
    ##   [2] "ideal_adoption_timeline"                                    
    ##   [3] "reason_for_adoption"                                        
    ##   [4] "specific_animal"                                            
    ##   [5] "adults_in_home"                                             
    ##   [6] "children_in_home"                                           
    ##   [7] "all_household_agree"                                        
    ##   [8] "allergies"                                                  
    ##   [9] "home_owner"                                                 
    ##  [10] "home_pet_policy"                                            
    ##  [11] "experience"                                                 
    ##  [12] "budget_monthly"                                             
    ##  [13] "budget_emergency"                                           
    ##  [14] "home_alone_avg"                                             
    ##  [15] "home_alone_max"                                             
    ##  [16] "pet_kept"                                                   
    ##  [17] "exercise"                                                   
    ##  [18] "needs"                                                      
    ##  [19] "return_pet"                                                 
    ##  [20] "how_heard"                                                  
    ##  [21] "trello_id"                                                  
    ##  [22] "City"                                                       
    ##  [23] "State"                                                      
    ##  [24] "ZIP"                                                        
    ##  [25] "animal_type"                                                
    ##  [26] "budget_monthly_ranges"                                      
    ##  [27] "budget_emergency_ranges"                                    
    ##  [28] "reason.for.adoption_gift_ind"                               
    ##  [29] "reason.for.adoption_mouser_ind"                             
    ##  [30] "reason.for.adoption_my.kids_ind"                            
    ##  [31] "reason.for.adoption_myself_ind"                             
    ##  [32] "reason.for.adoption_other_ind"                              
    ##  [33] "reason.for.adoption_protection_ind"                         
    ##  [34] "all.household.agree_a.surprise_ind"                         
    ##  [35] "all.household.agree_no_ind"                                 
    ##  [36] "all.household.agree_yes_ind"                                
    ##  [37] "allergies_mildly.allergic_ind"                              
    ##  [38] "allergies_no.allergies_ind"                                 
    ##  [39] "allergies_not.sure_ind"                                     
    ##  [40] "allergies_very.allergic_ind"                                
    ##  [41] "home.owner_company_ind"                                     
    ##  [42] "home.owner_family.friend_ind"                               
    ##  [43] "home.owner_family.member.or.friend_ind"                     
    ##  [44] "home.owner_landlord_ind"                                    
    ##  [45] "home.owner_myself_ind"                                      
    ##  [46] "home.pet.policy_no.but.pets.allowed_ind"                    
    ##  [47] "home.pet.policy_not.applicable_ind"                         
    ##  [48] "home.pet.policy_not.yet_ind"                                
    ##  [49] "home.pet.policy_yes_ind"                                    
    ##  [50] "home.pet.policy_yes.with.pet.policy_ind"                    
    ##  [51] "experience_bred.sold_ind"                                   
    ##  [52] "experience_current.housemates.pet_ind"                      
    ##  [53] "experience_currently.have.pet_ind"                          
    ##  [54] "experience_euthanized_ind"                                  
    ##  [55] "experience_given.away_ind"                                  
    ##  [56] "experience_given.to.shelter_ind"                            
    ##  [57] "experience_grew.up.with_ind"                                
    ##  [58] "experience_never.lived.with_ind"                            
    ##  [59] "experience_past.housemates.pet_ind"                         
    ##  [60] "experience_pet.died.in.care_ind"                            
    ##  [61] "experience_pet.ran.away_ind"                                
    ##  [62] "budget.monthly.ranges_>$5000_ind"                           
    ##  [63] "budget.monthly.ranges_$100.$200_ind"                        
    ##  [64] "budget.monthly.ranges_$1000.$5000_ind"                      
    ##  [65] "budget.monthly.ranges_$200.$500_ind"                        
    ##  [66] "budget.monthly.ranges_$25.$100_ind"                         
    ##  [67] "budget.monthly.ranges_$500.$1000_ind"                       
    ##  [68] "budget.emergency.ranges_>$5000_ind"                         
    ##  [69] "budget.emergency.ranges_$100.$200_ind"                      
    ##  [70] "budget.emergency.ranges_$1000.$5000_ind"                    
    ##  [71] "budget.emergency.ranges_$200.$500_ind"                      
    ##  [72] "budget.emergency.ranges_$25.$100_ind"                       
    ##  [73] "budget.emergency.ranges_$500.$1000_ind"                     
    ##  [74] "home.alone.avg_0_ind"                                       
    ##  [75] "home.alone.avg_1_ind"                                       
    ##  [76] "home.alone.avg_10_ind"                                      
    ##  [77] "home.alone.avg_11_ind"                                      
    ##  [78] "home.alone.avg_12_ind"                                      
    ##  [79] "home.alone.avg_13_ind"                                      
    ##  [80] "home.alone.avg_2_ind"                                       
    ##  [81] "home.alone.avg_24_ind"                                      
    ##  [82] "home.alone.avg_3_ind"                                       
    ##  [83] "home.alone.avg_4_ind"                                       
    ##  [84] "home.alone.avg_5_ind"                                       
    ##  [85] "home.alone.avg_6_ind"                                       
    ##  [86] "home.alone.avg_7_ind"                                       
    ##  [87] "home.alone.avg_8_ind"                                       
    ##  [88] "home.alone.avg_9_ind"                                       
    ##  [89] "home.alone.max_0_ind"                                       
    ##  [90] "home.alone.max_1_ind"                                       
    ##  [91] "home.alone.max_10_ind"                                      
    ##  [92] "home.alone.max_11_ind"                                      
    ##  [93] "home.alone.max_12_ind"                                      
    ##  [94] "home.alone.max_13_ind"                                      
    ##  [95] "home.alone.max_14_ind"                                      
    ##  [96] "home.alone.max_15_ind"                                      
    ##  [97] "home.alone.max_16_ind"                                      
    ##  [98] "home.alone.max_18_ind"                                      
    ##  [99] "home.alone.max_2_ind"                                       
    ## [100] "home.alone.max_20_ind"                                      
    ## [101] "home.alone.max_23_ind"                                      
    ## [102] "home.alone.max_24_ind"                                      
    ## [103] "home.alone.max_28_ind"                                      
    ## [104] "home.alone.max_3_ind"                                       
    ## [105] "home.alone.max_30_ind"                                      
    ## [106] "home.alone.max_36_ind"                                      
    ## [107] "home.alone.max_4_ind"                                       
    ## [108] "home.alone.max_48_ind"                                      
    ## [109] "home.alone.max_5_ind"                                       
    ## [110] "home.alone.max_6_ind"                                       
    ## [111] "home.alone.max_7_ind"                                       
    ## [112] "home.alone.max_8_ind"                                       
    ## [113] "home.alone.max_9_ind"                                       
    ## [114] "pet.kept_crate_ind"                                         
    ## [115] "pet.kept_inside.only_ind"                                   
    ## [116] "pet.kept_inside.outside_ind"                                
    ## [117] "pet.kept_inside.with.yard.access_ind"                       
    ## [118] "pet.kept_leash.harness_ind"                                 
    ## [119] "pet.kept_other_ind"                                         
    ## [120] "pet.kept_outside.only_ind"                                  
    ## [121] "pet.kept_supervised.in.my.yard_ind"                         
    ## [122] "pet.kept_unsupervised.access.to.my.yard.doggie.door.etc_ind"
    ## [123] "exercise_another.pet_ind"                                   
    ## [124] "exercise_dog.parks_ind"                                     
    ## [125] "exercise_jogging.together_ind"                              
    ## [126] "exercise_not.much_ind"                                      
    ## [127] "exercise_other.cats_ind"                                    
    ## [128] "exercise_other.pets_ind"                                    
    ## [129] "exercise_playing.in.my.yard_ind"                            
    ## [130] "exercise_toy.mice_ind"                                      
    ## [131] "exercise_walks.off.leash_ind"                               
    ## [132] "exercise_walks.on.leash_ind"                                
    ## [133] "exercise_wand.toys_ind"                                     
    ## [134] "needs_declaw_ind"                                           
    ## [135] "needs_groom.myself_ind"                                     
    ## [136] "needs_nail.clip_ind"                                        
    ## [137] "needs_no.grooming_ind"                                      
    ## [138] "needs_not.sure_ind"                                         
    ## [139] "needs_other_ind"                                            
    ## [140] "needs_professional.groomer_ind"                             
    ## [141] "needs_scratching.post_ind"                                  
    ## [142] "return.pet_allergies.appear_ind"                            
    ## [143] "return.pet_becomes.aggressive_ind"                          
    ## [144] "return.pet_destructive_ind"                                 
    ## [145] "return.pet_jumps.on.counters_ind"                           
    ## [146] "return.pet_jumps.on.furniture_ind"                          
    ## [147] "return.pet_litter.box.issues_ind"                           
    ## [148] "return.pet_moving.too.far_ind"                              
    ## [149] "return.pet_new.baby_ind"                                    
    ## [150] "return.pet_none_ind"                                        
    ## [151] "return.pet_not.allowed.new.living.space_ind"                
    ## [152] "return.pet_not.enough.time_ind"                             
    ## [153] "return.pet_not.housebroken_ind"                             
    ## [154] "return.pet_other_ind"                                       
    ## [155] "return.pet_pet.sheds_ind"                                   
    ## [156] "return.pet_scratches.furniture_ind"                         
    ## [157] "return.pet_too.playful_ind"                                 
    ## [158] "return.pet_vet.becomes.expensive_ind"                       
    ## [159] "date_start"                                                 
    ## [160] "checklist_ACCT"                                             
    ## [161] "checklist_CHQ"                                              
    ## [162] "checklist_LL"                                               
    ## [163] "checklist_PP"                                               
    ## [164] "checklist_SPCA"                                             
    ## [165] "checklist_TR"                                               
    ## [166] "checklist_VET"                                              
    ## [167] "wday_start"                                                 
    ## [168] "species"                                                    
    ## [169] "primary_breed"                                              
    ## [170] "secondary_breed"                                            
    ## [171] "markings"                                                   
    ## [172] "gender"                                                     
    ## [173] "altered"                                                    
    ## [174] "dob"                                                        
    ## [175] "age_intake"                                                 
    ## [176] "intake_asilomar"                                            
    ## [177] "intake_condition"                                           
    ## [178] "intake_date"                                                
    ## [179] "intake_type"                                                
    ## [180] "intake_subtype"                                             
    ## [181] "intake_reason"                                              
    ## [182] "intake_sitename"                                            
    ## [183] "agency_name"                                                
    ## [184] "outcome_asilomar"                                           
    ## [185] "release_date"                                               
    ## [186] "outcome_date"                                               
    ## [187] "outcome_type"                                               
    ## [188] "outcome_subtype"                                            
    ## [189] "outcome_sitename"                                           
    ## [190] "outcome_city"                                               
    ## [191] "outcome_state"                                              
    ## [192] "outcome_ZIP"                                                
    ## [193] "new_age_group"                                              
    ## [194] "process_time"                                               
    ## [195] "process_time_periods"                                       
    ## [196] "new.age.group_<4.weeks_ind"                                 
    ## [197] "new.age.group_1.2years_ind"                                 
    ## [198] "new.age.group_12weeks.6months_ind"                          
    ## [199] "new.age.group_2.4years_ind"                                 
    ## [200] "new.age.group_4.12.weeks_ind"                               
    ## [201] "new.age.group_4.6years_ind"                                 
    ## [202] "new.age.group_6.10years_ind"                                
    ## [203] "new.age.group_6months.1year_ind"                            
    ## [204] "new.age.group_NA_ind"                                       
    ## [205] "new.age.group_older.than.10years_ind"                       
    ## [206] "dateLastActivity"                                           
    ## [207] "due"                                                        
    ## [208] "label_names"                                                
    ## [209] "last_label"                                                 
    ## [210] "num_labels"                                                 
    ## [211] "label.names_.adopted_ind"                                   
    ## [212] "label.names_.adopted.elsewhere_ind"                         
    ## [213] "label.names_.adoption.follow.up_ind"                        
    ## [214] "label.names_.approved_ind"                                  
    ## [215] "label.names_.approved.with.limitation_ind"                  
    ## [216] "label.names_.checks_ind"                                    
    ## [217] "label.names_.declaw.only_ind"                               
    ## [218] "label.names_.denied_ind"                                    
    ## [219] "label.names_.do.not.follow.up_ind"                          
    ## [220] "label.names_.dog.meet_ind"                                  
    ## [221] "label.names_.foster.to.adopt_ind"                           
    ## [222] "label.names_.landlord_ind"                                  
    ## [223] "label.names_.manager.decision_ind"                          
    ## [224] "label.names_.need.info_ind"                                 
    ## [225] "label.names_.need.proof.of.ownership_ind"                   
    ## [226] "label.names_.need.roommates.vet.info_ind"                   
    ## [227] "label.names_.need.to.see.id_ind"                            
    ## [228] "label.names_.need.vet.info_ind"                             
    ## [229] "label.names_.need.written.ll.permission_ind"                
    ## [230] "label.names_.needs.app.attached_ind"                        
    ## [231] "label.names_.needs.review.before.approval_ind"              
    ## [232] "label.names_.not.s.n_ind"                                   
    ## [233] "label.names_.not.utd_ind"                                   
    ## [234] "label.names_.opa_ind"                                       
    ## [235] "label.names_.pet.policy_ind"                                
    ## [236] "label.names_.questions_ind"                                 
    ## [237] "label.names_.ready.for.review_ind"                          
    ## [238] "label.names_.ready.to.adopt_ind"                            
    ## [239] "label.names_.red.flag_ind"                                  
    ## [240] "label.names_.rescue.check_ind"                              
    ## [241] "label.names_.returned_ind"                                  
    ## [242] "label.names_.reviewed.with.handouts.only_ind"               
    ## [243] "label.names_.serial.no.show_ind"                            
    ## [244] "label.names_.unsure.foster.or.adopt_ind"                    
    ## [245] "label.names_.vet_ind"                                       
    ## [246] "label.names_.vet.check.in.process_ind"                      
    ## [247] "label.names_.withdrawn_ind"                                 
    ## [248] "label.names_adopted_ind"                                    
    ## [249] "label.names_adopted.elsewhere_ind"                          
    ## [250] "label.names_adoption.follow.up_ind"                         
    ## [251] "label.names_approved_ind"                                   
    ## [252] "label.names_approved.with.limitation_ind"                   
    ## [253] "label.names_checks_ind"                                     
    ## [254] "label.names_declaw.only_ind"                                
    ## [255] "label.names_denied_ind"                                     
    ## [256] "label.names_foster.to.adopt_ind"                            
    ## [257] "label.names_landlord_ind"                                   
    ## [258] "label.names_manager.decision_ind"                           
    ## [259] "label.names_need.info_ind"                                  
    ## [260] "label.names_need.proof.of.ownership_ind"                    
    ## [261] "label.names_need.roommates.vet.info_ind"                    
    ## [262] "label.names_need.to.see.id_ind"                             
    ## [263] "label.names_need.vet.info_ind"                              
    ## [264] "label.names_need.written.ll.permission_ind"                 
    ## [265] "label.names_needs.review.before.approval_ind"               
    ## [266] "label.names_not.s.n_ind"                                    
    ## [267] "label.names_not.utd_ind"                                    
    ## [268] "label.names_opa_ind"                                        
    ## [269] "label.names_pet.policy_ind"                                 
    ## [270] "label.names_questions_ind"                                  
    ## [271] "label.names_ready.for.review_ind"                           
    ## [272] "label.names_ready.to.adopt_ind"                             
    ## [273] "label.names_red.flag_ind"                                   
    ## [274] "label.names_rescue.check_ind"                               
    ## [275] "label.names_returned_ind"                                   
    ## [276] "label.names_reviewed.with.handouts.only_ind"                
    ## [277] "label.names_serial.no.show_ind"                             
    ## [278] "label.names_unsure.foster.or.adopt_ind"                     
    ## [279] "label.names_vet_ind"                                        
    ## [280] "label.names_vet.check.in.process_ind"                       
    ## [281] "label.names_withdrawn_ind"

``` r
#write.csv(combine_data, "combine_data.csv", row.names = FALSE)
```
