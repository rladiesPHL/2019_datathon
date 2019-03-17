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

``` r
# Turn off scientific notation ----
options(scipen=999) #digits = 2
```

Load Data
---------

Helper Functions
----------------

convert\_to\_ind(): creates indicator for specified field in dataframe

``` r
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

clean_pet_kept <- function(x) {
  x %>% str_replace_all(.,"unsupervised-access-to-my-yard-9doggie-door-etc","unsupervised-access-to-my-yard-doggie-door-etc")
}

#budget cleanup functions
clean_budget <- function(x) {
  x %>% str_replace_all(.,"^-","") %>%
    parse_number(.) %>%
    gsub("[$]|[(]|[)]|[,]", "", .) %>% 
    as.numeric()
}

create_budget_range <- function(x) {
    case_when( x <= 25 ~ "<25",
               x <= 100 ~ "26-100", 
               x <= 200 ~ "101-200",
               x <= 500 ~ "201-500",
               x <= 1000 ~ "501-1000",
               x <= 5000 ~ "1001-5000",
               is.na(x) ~ "NA",
               TRUE ~ ">5000")
}

#useful functions for QA
get_unique_elements <- function(df, colname) {
  elements_string <- do.call(paste, c(as.list(df[colname]), sep = ","))
  elements_list <- unique(trimws(unlist(strsplit(elements_string, c(",")))))
  unique_elements <- elements_list[!elements_list %in% c("","NA")]
  return(unique_elements)
}

get_elements_summary <- function(output_df, colname, new_colnames) {
  subset_df <- output_df[names(output_df) %in% new_colnames]
  elements_summary <- subset_df %>%
    summarise_all(sum, na.rm = TRUE) %>%
    gather(!!colname, "count")
  return(elements_summary)
}

#city cleanup
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

Apps data needs to be cleaned up:
<ul>
        <li> __Removed columns__: STATEFP,COUNTYFP,TRACTCE,GEOID,NAME,NAMELSAD,MTFCC,FUNCSTAT,ALAND,AWATER,INTPTLAT,INTPTLON </li>          <li> Take absolute value of negative numbers </li>
        <li> Any values > 15 for __adults_in_home__ and __children_in_home__ are updated N/A </li>
        <li> __ideal_adoption_timeline__: consolidated next-few-weeks to few-weeks </li>
        <li> __all_household_agree__: consolidated using the function clean_household_agree </li>
        <li> __home_owner__ and __home_pet_policy__: make factor </li>
        <li> __experience__: cleaned using function above </li>
        <li> __City__: cleaned to reduce same cities with different spellings </li>
        <li> __budget_monthly__ and __budget_emergency__: </li>

<br> 1. Cleaned up the syntax (e.g. () and $) <br> 2. Updated to take the absolute values (e.g. -3000 is no 3000) <br> 3. Categorized into buckets (e.g. 100-200)
</ul>
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
         budget_monthly_ranges = as.factor(create_budget_range(budget_monthly)),
         budget_emergency_ranges = as.factor(create_budget_range(budget_emergency)))
         
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

    ## [1] 1906  160

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
    ##  [62] "budget.monthly.ranges_<25_ind"                              
    ##  [63] "budget.monthly.ranges_>5000_ind"                            
    ##  [64] "budget.monthly.ranges_1001.5000_ind"                        
    ##  [65] "budget.monthly.ranges_101.200_ind"                          
    ##  [66] "budget.monthly.ranges_201.500_ind"                          
    ##  [67] "budget.monthly.ranges_26.100_ind"                           
    ##  [68] "budget.monthly.ranges_501.1000_ind"                         
    ##  [69] "budget.emergency.ranges_<25_ind"                            
    ##  [70] "budget.emergency.ranges_>5000_ind"                          
    ##  [71] "budget.emergency.ranges_1001.5000_ind"                      
    ##  [72] "budget.emergency.ranges_101.200_ind"                        
    ##  [73] "budget.emergency.ranges_201.500_ind"                        
    ##  [74] "budget.emergency.ranges_26.100_ind"                         
    ##  [75] "budget.emergency.ranges_501.1000_ind"                       
    ##  [76] "home.alone.avg_0_ind"                                       
    ##  [77] "home.alone.avg_1_ind"                                       
    ##  [78] "home.alone.avg_10_ind"                                      
    ##  [79] "home.alone.avg_11_ind"                                      
    ##  [80] "home.alone.avg_12_ind"                                      
    ##  [81] "home.alone.avg_13_ind"                                      
    ##  [82] "home.alone.avg_2_ind"                                       
    ##  [83] "home.alone.avg_24_ind"                                      
    ##  [84] "home.alone.avg_3_ind"                                       
    ##  [85] "home.alone.avg_4_ind"                                       
    ##  [86] "home.alone.avg_5_ind"                                       
    ##  [87] "home.alone.avg_6_ind"                                       
    ##  [88] "home.alone.avg_7_ind"                                       
    ##  [89] "home.alone.avg_8_ind"                                       
    ##  [90] "home.alone.avg_9_ind"                                       
    ##  [91] "home.alone.max_0_ind"                                       
    ##  [92] "home.alone.max_1_ind"                                       
    ##  [93] "home.alone.max_10_ind"                                      
    ##  [94] "home.alone.max_11_ind"                                      
    ##  [95] "home.alone.max_12_ind"                                      
    ##  [96] "home.alone.max_13_ind"                                      
    ##  [97] "home.alone.max_14_ind"                                      
    ##  [98] "home.alone.max_15_ind"                                      
    ##  [99] "home.alone.max_16_ind"                                      
    ## [100] "home.alone.max_18_ind"                                      
    ## [101] "home.alone.max_2_ind"                                       
    ## [102] "home.alone.max_20_ind"                                      
    ## [103] "home.alone.max_23_ind"                                      
    ## [104] "home.alone.max_24_ind"                                      
    ## [105] "home.alone.max_28_ind"                                      
    ## [106] "home.alone.max_3_ind"                                       
    ## [107] "home.alone.max_30_ind"                                      
    ## [108] "home.alone.max_36_ind"                                      
    ## [109] "home.alone.max_4_ind"                                       
    ## [110] "home.alone.max_48_ind"                                      
    ## [111] "home.alone.max_5_ind"                                       
    ## [112] "home.alone.max_6_ind"                                       
    ## [113] "home.alone.max_7_ind"                                       
    ## [114] "home.alone.max_8_ind"                                       
    ## [115] "home.alone.max_9_ind"                                       
    ## [116] "pet.kept_crate_ind"                                         
    ## [117] "pet.kept_inside.only_ind"                                   
    ## [118] "pet.kept_inside.outside_ind"                                
    ## [119] "pet.kept_inside.with.yard.access_ind"                       
    ## [120] "pet.kept_leash.harness_ind"                                 
    ## [121] "pet.kept_other_ind"                                         
    ## [122] "pet.kept_outside.only_ind"                                  
    ## [123] "pet.kept_supervised.in.my.yard_ind"                         
    ## [124] "pet.kept_unsupervised.access.to.my.yard.doggie.door.etc_ind"
    ## [125] "exercise_another.pet_ind"                                   
    ## [126] "exercise_dog.parks_ind"                                     
    ## [127] "exercise_jogging.together_ind"                              
    ## [128] "exercise_not.much_ind"                                      
    ## [129] "exercise_other.cats_ind"                                    
    ## [130] "exercise_other.pets_ind"                                    
    ## [131] "exercise_playing.in.my.yard_ind"                            
    ## [132] "exercise_toy.mice_ind"                                      
    ## [133] "exercise_walks.off.leash_ind"                               
    ## [134] "exercise_walks.on.leash_ind"                                
    ## [135] "exercise_wand.toys_ind"                                     
    ## [136] "needs_declaw_ind"                                           
    ## [137] "needs_groom.myself_ind"                                     
    ## [138] "needs_nail.clip_ind"                                        
    ## [139] "needs_no.grooming_ind"                                      
    ## [140] "needs_not.sure_ind"                                         
    ## [141] "needs_other_ind"                                            
    ## [142] "needs_professional.groomer_ind"                             
    ## [143] "needs_scratching.post_ind"                                  
    ## [144] "return.pet_allergies.appear_ind"                            
    ## [145] "return.pet_becomes.aggressive_ind"                          
    ## [146] "return.pet_destructive_ind"                                 
    ## [147] "return.pet_jumps.on.counters_ind"                           
    ## [148] "return.pet_jumps.on.furniture_ind"                          
    ## [149] "return.pet_litter.box.issues_ind"                           
    ## [150] "return.pet_moving.too.far_ind"                              
    ## [151] "return.pet_new.baby_ind"                                    
    ## [152] "return.pet_none_ind"                                        
    ## [153] "return.pet_not.allowed.new.living.space_ind"                
    ## [154] "return.pet_not.enough.time_ind"                             
    ## [155] "return.pet_not.housebroken_ind"                             
    ## [156] "return.pet_other_ind"                                       
    ## [157] "return.pet_pet.sheds_ind"                                   
    ## [158] "return.pet_scratches.furniture_ind"                         
    ## [159] "return.pet_too.playful_ind"                                 
    ## [160] "return.pet_vet.becomes.expensive_ind"

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

| label\_names                                | last\_label             |  num\_labels|  label.names\_.adopted\_ind|  label.names\_.adopted.elsewhere\_ind|  label.names\_.adoption.follow.up\_ind|  label.names\_.approved\_ind|  label.names\_.approved.with.limitation\_ind|  label.names\_.checks\_ind|  label.names\_.declaw.only\_ind|  label.names\_.denied\_ind|  label.names\_.do.not.follow.up\_ind|  label.names\_.dog.meet\_ind|  label.names\_.foster.to.adopt\_ind|  label.names\_.landlord\_ind|  label.names\_.manager.decision\_ind|  label.names\_.need.info\_ind|  label.names\_.need.proof.of.ownership\_ind|  label.names\_.need.roommates.vet.info\_ind|  label.names\_.need.to.see.id\_ind|  label.names\_.need.vet.info\_ind|  label.names\_.need.written.ll.permission\_ind|  label.names\_.needs.app.attached\_ind|  label.names\_.needs.review.before.approval\_ind|  label.names\_.not.s.n\_ind|  label.names\_.not.utd\_ind|  label.names\_.opa\_ind|  label.names\_.pet.policy\_ind|  label.names\_.questions\_ind|  label.names\_.ready.for.review\_ind|  label.names\_.ready.to.adopt\_ind|  label.names\_.red.flag\_ind|  label.names\_.rescue.check\_ind|  label.names\_.returned\_ind|  label.names\_.reviewed.with.handouts.only\_ind|  label.names\_.serial.no.show\_ind|  label.names\_.unsure.foster.or.adopt\_ind|  label.names\_.vet\_ind|  label.names\_.vet.check.in.process\_ind|  label.names\_.withdrawn\_ind|  label.names\_adopted\_ind|  label.names\_adopted.elsewhere\_ind|  label.names\_adoption.follow.up\_ind|  label.names\_approved\_ind|  label.names\_approved.with.limitation\_ind|  label.names\_checks\_ind|  label.names\_declaw.only\_ind|  label.names\_denied\_ind|  label.names\_foster.to.adopt\_ind|  label.names\_landlord\_ind|  label.names\_manager.decision\_ind|  label.names\_need.info\_ind|  label.names\_need.proof.of.ownership\_ind|  label.names\_need.roommates.vet.info\_ind|  label.names\_need.to.see.id\_ind|  label.names\_need.vet.info\_ind|  label.names\_need.written.ll.permission\_ind|  label.names\_needs.review.before.approval\_ind|  label.names\_not.s.n\_ind|  label.names\_not.utd\_ind|  label.names\_opa\_ind|  label.names\_pet.policy\_ind|  label.names\_questions\_ind|  label.names\_ready.for.review\_ind|  label.names\_ready.to.adopt\_ind|  label.names\_red.flag\_ind|  label.names\_rescue.check\_ind|  label.names\_returned\_ind|  label.names\_reviewed.with.handouts.only\_ind|  label.names\_serial.no.show\_ind|  label.names\_unsure.foster.or.adopt\_ind|  label.names\_vet\_ind|  label.names\_vet.check.in.process\_ind|  label.names\_withdrawn\_ind|
|:--------------------------------------------|:------------------------|------------:|---------------------------:|-------------------------------------:|--------------------------------------:|----------------------------:|--------------------------------------------:|--------------------------:|-------------------------------:|--------------------------:|------------------------------------:|----------------------------:|-----------------------------------:|----------------------------:|------------------------------------:|-----------------------------:|-------------------------------------------:|-------------------------------------------:|----------------------------------:|---------------------------------:|----------------------------------------------:|--------------------------------------:|------------------------------------------------:|---------------------------:|---------------------------:|-----------------------:|------------------------------:|-----------------------------:|------------------------------------:|----------------------------------:|----------------------------:|--------------------------------:|----------------------------:|-----------------------------------------------:|----------------------------------:|------------------------------------------:|-----------------------:|----------------------------------------:|-----------------------------:|--------------------------:|------------------------------------:|-------------------------------------:|---------------------------:|-------------------------------------------:|-------------------------:|------------------------------:|-------------------------:|----------------------------------:|---------------------------:|-----------------------------------:|----------------------------:|------------------------------------------:|------------------------------------------:|---------------------------------:|--------------------------------:|---------------------------------------------:|-----------------------------------------------:|--------------------------:|--------------------------:|----------------------:|-----------------------------:|----------------------------:|-----------------------------------:|---------------------------------:|---------------------------:|-------------------------------:|---------------------------:|----------------------------------------------:|---------------------------------:|-----------------------------------------:|----------------------:|---------------------------------------:|----------------------------:|
| NA                                          | NA                      |            0|                          NA|                                    NA|                                     NA|                           NA|                                           NA|                         NA|                              NA|                         NA|                                   NA|                           NA|                                  NA|                           NA|                                   NA|                            NA|                                          NA|                                          NA|                                 NA|                                NA|                                             NA|                                     NA|                                               NA|                          NA|                          NA|                      NA|                             NA|                            NA|                                   NA|                                 NA|                           NA|                               NA|                           NA|                                              NA|                                 NA|                                         NA|                      NA|                                       NA|                            NA|                         NA|                                   NA|                                    NA|                          NA|                                          NA|                        NA|                             NA|                        NA|                                 NA|                          NA|                                  NA|                           NA|                                         NA|                                         NA|                                NA|                               NA|                                            NA|                                              NA|                         NA|                         NA|                     NA|                            NA|                           NA|                                  NA|                                NA|                          NA|                              NA|                          NA|                                             NA|                                NA|                                        NA|                     NA|                                      NA|                           NA|
| NA                                          | NA                      |            0|                          NA|                                    NA|                                     NA|                           NA|                                           NA|                         NA|                              NA|                         NA|                                   NA|                           NA|                                  NA|                           NA|                                   NA|                            NA|                                          NA|                                          NA|                                 NA|                                NA|                                             NA|                                     NA|                                               NA|                          NA|                          NA|                      NA|                             NA|                            NA|                                   NA|                                 NA|                           NA|                               NA|                           NA|                                              NA|                                 NA|                                         NA|                      NA|                                       NA|                            NA|                         NA|                                   NA|                                    NA|                          NA|                                          NA|                        NA|                             NA|                        NA|                                 NA|                          NA|                                  NA|                           NA|                                         NA|                                         NA|                                NA|                               NA|                                            NA|                                              NA|                         NA|                         NA|                     NA|                            NA|                           NA|                                  NA|                                NA|                          NA|                              NA|                          NA|                                             NA|                                NA|                                        NA|                     NA|                                      NA|                           NA|
| reviewed with handouts only, ready to adopt | ready to adopt          |            2|                           0|                                     0|                                      0|                            0|                                            0|                          0|                               0|                          0|                                    0|                            0|                                   0|                            0|                                    0|                             0|                                           0|                                           0|                                  0|                                 0|                                              0|                                      0|                                                0|                           0|                           0|                       0|                              0|                             0|                                    0|                                  1|                            0|                                0|                            0|                                               0|                                  0|                                          0|                       0|                                        0|                             0|                          0|                                    0|                                     0|                           0|                                           0|                         0|                              0|                         0|                                  0|                           0|                                   0|                            0|                                          0|                                          0|                                 0|                                0|                                             0|                                               0|                          0|                          0|                      0|                             0|                            0|                                   0|                                 0|                           0|                               0|                           0|                                              1|                                 0|                                         0|                      0|                                       0|                            0|
| need info, vet, questions                   | questions               |            3|                           0|                                     0|                                      0|                            0|                                            0|                          0|                               0|                          0|                                    0|                            0|                                   0|                            0|                                    0|                             0|                                           0|                                           0|                                  0|                                 0|                                              0|                                      0|                                                0|                           0|                           0|                       0|                              0|                             1|                                    0|                                  0|                            0|                                0|                            0|                                               0|                                  0|                                          0|                       1|                                        0|                             0|                          0|                                    0|                                     0|                           0|                                           0|                         0|                              0|                         0|                                  0|                           0|                                   0|                            1|                                          0|                                          0|                                 0|                                0|                                             0|                                               0|                          0|                          0|                      0|                             0|                            0|                                   0|                                 0|                           0|                               0|                           0|                                              0|                                 0|                                         0|                      0|                                       0|                            0|
| NA                                          | NA                      |            0|                          NA|                                    NA|                                     NA|                           NA|                                           NA|                         NA|                              NA|                         NA|                                   NA|                           NA|                                  NA|                           NA|                                   NA|                            NA|                                          NA|                                          NA|                                 NA|                                NA|                                             NA|                                     NA|                                               NA|                          NA|                          NA|                      NA|                             NA|                            NA|                                   NA|                                 NA|                           NA|                               NA|                           NA|                                              NA|                                 NA|                                         NA|                      NA|                                       NA|                            NA|                         NA|                                   NA|                                    NA|                          NA|                                          NA|                        NA|                             NA|                        NA|                                 NA|                          NA|                                  NA|                           NA|                                         NA|                                         NA|                                NA|                               NA|                                            NA|                                              NA|                         NA|                         NA|                     NA|                            NA|                           NA|                                  NA|                                NA|                          NA|                              NA|                          NA|                                             NA|                                NA|                                        NA|                     NA|                                      NA|                           NA|
| need proof of ownership                     | need proof of ownership |            1|                           0|                                     0|                                      0|                            0|                                            0|                          0|                               0|                          0|                                    0|                            0|                                   0|                            0|                                    0|                             0|                                           0|                                           0|                                  0|                                 0|                                              0|                                      0|                                                0|                           0|                           0|                       0|                              0|                             0|                                    0|                                  0|                            0|                                0|                            0|                                               0|                                  0|                                          0|                       0|                                        0|                             0|                          0|                                    0|                                     0|                           0|                                           0|                         0|                              0|                         0|                                  0|                           0|                                   0|                            0|                                          1|                                          0|                                 0|                                0|                                             0|                                               0|                          0|                          0|                      0|                             0|                            0|                                   0|                                 0|                           0|                               0|                           0|                                              0|                                 0|                                         0|                      0|                                       0|                            0|
| NA                                          | NA                      |            0|                          NA|                                    NA|                                     NA|                           NA|                                           NA|                         NA|                              NA|                         NA|                                   NA|                           NA|                                  NA|                           NA|                                   NA|                            NA|                                          NA|                                          NA|                                 NA|                                NA|                                             NA|                                     NA|                                               NA|                          NA|                          NA|                      NA|                             NA|                            NA|                                   NA|                                 NA|                           NA|                               NA|                           NA|                                              NA|                                 NA|                                         NA|                      NA|                                       NA|                            NA|                         NA|                                   NA|                                    NA|                          NA|                                          NA|                        NA|                             NA|                        NA|                                 NA|                          NA|                                  NA|                           NA|                                         NA|                                         NA|                                NA|                               NA|                                            NA|                                              NA|                         NA|                         NA|                     NA|                            NA|                           NA|                                  NA|                                NA|                          NA|                              NA|                          NA|                                             NA|                                NA|                                        NA|                     NA|                                      NA|                           NA|
| NA                                          | NA                      |            0|                          NA|                                    NA|                                     NA|                           NA|                                           NA|                         NA|                              NA|                         NA|                                   NA|                           NA|                                  NA|                           NA|                                   NA|                            NA|                                          NA|                                          NA|                                 NA|                                NA|                                             NA|                                     NA|                                               NA|                          NA|                          NA|                      NA|                             NA|                            NA|                                   NA|                                 NA|                           NA|                               NA|                           NA|                                              NA|                                 NA|                                         NA|                      NA|                                       NA|                            NA|                         NA|                                   NA|                                    NA|                          NA|                                          NA|                        NA|                             NA|                        NA|                                 NA|                          NA|                                  NA|                           NA|                                         NA|                                         NA|                                NA|                               NA|                                            NA|                                              NA|                         NA|                         NA|                     NA|                            NA|                           NA|                                  NA|                                NA|                          NA|                              NA|                          NA|                                             NA|                                NA|                                        NA|                     NA|                                      NA|                           NA|
| not utd                                     | not utd                 |            1|                           0|                                     0|                                      0|                            0|                                            0|                          0|                               0|                          0|                                    0|                            0|                                   0|                            0|                                    0|                             0|                                           0|                                           0|                                  0|                                 0|                                              0|                                      0|                                                0|                           0|                           0|                       0|                              0|                             0|                                    0|                                  0|                            0|                                0|                            0|                                               0|                                  0|                                          0|                       0|                                        0|                             0|                          0|                                    0|                                     0|                           0|                                           0|                         0|                              0|                         0|                                  0|                           0|                                   0|                            0|                                          0|                                          0|                                 0|                                0|                                             0|                                               0|                          0|                          1|                      0|                             0|                            0|                                   0|                                 0|                           0|                               0|                           0|                                              0|                                 0|                                         0|                      0|                                       0|                            0|
| NA                                          | NA                      |            0|                          NA|                                    NA|                                     NA|                           NA|                                           NA|                         NA|                              NA|                         NA|                                   NA|                           NA|                                  NA|                           NA|                                   NA|                            NA|                                          NA|                                          NA|                                 NA|                                NA|                                             NA|                                     NA|                                               NA|                          NA|                          NA|                      NA|                             NA|                            NA|                                   NA|                                 NA|                           NA|                               NA|                           NA|                                              NA|                                 NA|                                         NA|                      NA|                                       NA|                            NA|                         NA|                                   NA|                                    NA|                          NA|                                          NA|                        NA|                             NA|                        NA|                                 NA|                          NA|                                  NA|                           NA|                                         NA|                                         NA|                                NA|                               NA|                                            NA|                                              NA|                         NA|                         NA|                     NA|                            NA|                           NA|                                  NA|                                NA|                          NA|                              NA|                          NA|                                             NA|                                NA|                                        NA|                     NA|                                      NA|                           NA|

Merge the 4 Datasets
--------------------

``` r
combine_data <- apps_with_indicators %>%
  left_join(actions) %>%
  left_join(petpoint_with_indicators) %>%
  left_join(cards_with_indicators)            

dim(combine_data)
```

    ## [1] 1906  283

``` r
#colnames(combine_data)
write.csv(combine_data, "Analyses/2_Applicants/combine_data.csv", row.names = FALSE)
```
