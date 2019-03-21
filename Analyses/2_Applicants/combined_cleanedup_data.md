combined\_dataset
================
Veena (some edits from Amy and Ramaa)
3/17/2019

-   [Load Data](#load-data)
-   [Helper Functions](#helper-functions)
-   [Dataset Cleanup](#dataset-cleanup)
    -   [Apps Data](#apps-data)
    -   [Actions Dataset](#actions-dataset)
    -   [Petpoint Dataset](#petpoint-dataset)
    -   [Cards Dataset](#cards-dataset)
-   [Merge the 4 Datasets](#merge-the-4-datasets)
-   [Data Visualizations](#data-visualizations)
    -   [Petpoint Visualizations](#petpoint-visualizations)
    -   [Cards Visualizations](#cards-visualizations)
    -   [Apps Visualizations](#apps-visualizations)
    -   [Actions Visualizations](#actions-visualizations)

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
        distinct(trello_id, animal_type, var) %>% 
        unnest(split = str_split(str_trim(var), ",")) %>%
        select(-var) %>% 
        filter(!is.na(split)) %>% 
        mutate(split = str_trim(split)) %>%
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
petpoint <- petpoint %>% 
  select(-X1) %>% # there are 77 duplicates
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
    ##  $ markings            : chr  NA NA NA NA ...
    ##  $ gender              : chr  "F" "F" "M" "M" ...
    ##  $ altered             : chr  "Yes" "Yes" "Yes" "Yes" ...
    ##  $ dob                 : Date, format: "2006-10-18" "2007-05-16" ...
    ##  $ age_intake          : int  138 139 141 130 174 125 140 126 121 101 ...
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

``` r
# Some duplicates remain
all(duplicated(petpoint) == FALSE)  
```

    ## [1] FALSE

``` r
petpoint[duplicated(petpoint),]
```

| animal\_type | species | primary\_breed       | secondary\_breed | markings | gender | altered | dob        |  age\_intake| intake\_asilomar          | intake\_condition | intake\_date        | intake\_type | intake\_subtype     | intake\_reason | intake\_sitename   | agency\_name                 | outcome\_asilomar         | release\_date       | outcome\_date       | outcome\_type | outcome\_subtype  | outcome\_sitename      | trello\_id               | outcome\_city | outcome\_state | outcome\_ZIP | new\_age\_group |  process\_time| process\_time\_periods |
|:-------------|:--------|:---------------------|:-----------------|:---------|:-------|:--------|:-----------|------------:|:--------------------------|:------------------|:--------------------|:-------------|:--------------------|:---------------|:-------------------|:-----------------------------|:--------------------------|:--------------------|:--------------------|:--------------|:------------------|:-----------------------|:-------------------------|:--------------|:---------------|:-------------|:----------------|--------------:|:-----------------------|
| Cat          | Cat     | Domestic Medium Hair | Mix              | NA       | F      | Yes     | 2014-03-18 |           48| Treatable-Rehabilitatable | Healthy           | 2018-03-23 14:30:00 | Transfer In  | Partner Transfer In | Rescue         | Grays Ferry Avenue | Camden County Animal Shelter | Treatable-Rehabilitatable | 2018-04-06 13:48:00 | 2018-04-06 13:48:00 | Adoption      | Kawaii Kitty Cafe | PAWS Offsite Adoptions | 5abd1fc3553a150daabdca1b | BENSALEM      | PA             | 19020        | 2-4years        |       13.97083| 11-30 days             |
| Cat          | Cat     | Domestic Shorthair   | Mix              | NA       | F      | Yes     | 2018-07-21 |            1| Healthy                   | Under 7 Weeks     | 2018-08-27 16:03:00 | Transfer In  | Partner Transfer In | Rescue         | Grays Ferry Avenue | ACCT Philly                  | Healthy                   | 2018-11-05 16:37:00 | 2018-11-05 16:37:00 | Adoption      | PAC               | PAC                    | 5bd0fef0fbda7d61758333dc | PHILADELPHIA  | PA             | 19143        | &lt;4 weeks     |       70.06528| 31-90 days             |

``` r
petpoint[petpoint$trello_id %in% c("5abd1fc3553a150daabdca1b", "5bd0fef0fbda7d61758333dc"),]
```

| animal\_type | species | primary\_breed       | secondary\_breed | markings | gender | altered | dob        |  age\_intake| intake\_asilomar          | intake\_condition | intake\_date        | intake\_type | intake\_subtype     | intake\_reason | intake\_sitename   | agency\_name                 | outcome\_asilomar         | release\_date       | outcome\_date       | outcome\_type | outcome\_subtype  | outcome\_sitename      | trello\_id               | outcome\_city | outcome\_state | outcome\_ZIP | new\_age\_group |  process\_time| process\_time\_periods |
|:-------------|:--------|:---------------------|:-----------------|:---------|:-------|:--------|:-----------|------------:|:--------------------------|:------------------|:--------------------|:-------------|:--------------------|:---------------|:-------------------|:-----------------------------|:--------------------------|:--------------------|:--------------------|:--------------|:------------------|:-----------------------|:-------------------------|:--------------|:---------------|:-------------|:----------------|--------------:|:-----------------------|
| Cat          | Cat     | Domestic Medium Hair | Mix              | NA       | F      | Yes     | 2014-03-18 |           48| Treatable-Rehabilitatable | Healthy           | 2018-03-23 14:30:00 | Transfer In  | Partner Transfer In | Rescue         | Grays Ferry Avenue | Camden County Animal Shelter | Treatable-Rehabilitatable | 2018-04-06 13:48:00 | 2018-04-06 13:48:00 | Adoption      | Kawaii Kitty Cafe | PAWS Offsite Adoptions | 5abd1fc3553a150daabdca1b | BENSALEM      | PA             | 19020        | 2-4years        |       13.97083| 11-30 days             |
| Cat          | Cat     | Domestic Medium Hair | Mix              | NA       | F      | Yes     | 2014-03-18 |           48| Treatable-Rehabilitatable | Healthy           | 2018-03-23 14:30:00 | Transfer In  | Partner Transfer In | Rescue         | Grays Ferry Avenue | Camden County Animal Shelter | Treatable-Rehabilitatable | 2018-04-06 13:48:00 | 2018-04-06 13:48:00 | Adoption      | Kawaii Kitty Cafe | PAWS Offsite Adoptions | 5abd1fc3553a150daabdca1b | BENSALEM      | PA             | 19020        | 2-4years        |       13.97083| 11-30 days             |
| Cat          | Cat     | Domestic Shorthair   | Mix              | NA       | F      | Yes     | 2018-07-21 |            1| Healthy                   | Under 7 Weeks     | 2018-08-27 16:03:00 | Transfer In  | Partner Transfer In | Rescue         | Grays Ferry Avenue | ACCT Philly                  | Healthy                   | 2018-11-05 16:37:00 | 2018-11-05 16:37:00 | Adoption      | PAC               | PAC                    | 5bd0fef0fbda7d61758333dc | PHILADELPHIA  | PA             | 19143        | &lt;4 weeks     |       70.06528| 31-90 days             |
| Cat          | Cat     | Domestic Shorthair   | Mix              | NA       | F      | Yes     | 2018-07-21 |            1| Healthy                   | Under 7 Weeks     | 2018-08-27 16:03:00 | Transfer In  | Partner Transfer In | Rescue         | Grays Ferry Avenue | ACCT Philly                  | Healthy                   | 2018-11-05 16:37:00 | 2018-11-05 16:37:00 | Adoption      | PAC               | PAC                    | 5bd0fef0fbda7d61758333dc | PHILADELPHIA  | PA             | 19143        | &lt;4 weeks     |       70.06528| 31-90 days             |

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

# convert dateLastActivity & due from character to Date
cards <- cards %>%
  mutate(dateLastActivity = mdy(dateLastActivity)) %>%
  mutate(due = mdy(due))
```

    ## Warning: All formats failed to parse. No formats found.

    ## Warning: All formats failed to parse. No formats found.

``` r
str(cards)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    9989 obs. of  7 variables:
    ##  $ trello_id       : chr  "5a6d054eea0086b8d596b209" "5a6e1189899dc1ca5caff197" "5a6e338962147c1a458086c2" "5a709fb3ced1c38cc2a08bbe" ...
    ##  $ dateLastActivity: Date, format: NA NA ...
    ##  $ due             : Date, format: NA NA ...
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

| label\_names                                | last\_label       |  num\_labels|  label.names\_adopted\_ind|  label.names\_adopted.elsewhere\_ind|  label.names\_adoption.follow.up\_ind|  label.names\_approved\_ind|  label.names\_approved.with.limitation\_ind|  label.names\_checks\_ind|  label.names\_declaw.only\_ind|  label.names\_denied\_ind|  label.names\_do.not.follow.up\_ind|  label.names\_dog.meet\_ind|  label.names\_foster.to.adopt\_ind|  label.names\_landlord\_ind|  label.names\_manager.decision\_ind|  label.names\_need.info\_ind|  label.names\_need.proof.of.ownership\_ind|  label.names\_need.roommates.vet.info\_ind|  label.names\_need.to.see.id\_ind|  label.names\_need.vet.info\_ind|  label.names\_need.written.ll.permission\_ind|  label.names\_needs.app.attached\_ind|  label.names\_needs.review.before.approval\_ind|  label.names\_not.s.n\_ind|  label.names\_not.utd\_ind|  label.names\_opa\_ind|  label.names\_pet.policy\_ind|  label.names\_questions\_ind|  label.names\_ready.for.review\_ind|  label.names\_ready.to.adopt\_ind|  label.names\_red.flag\_ind|  label.names\_rescue.check\_ind|  label.names\_returned\_ind|  label.names\_reviewed.with.handouts.only\_ind|  label.names\_serial.no.show\_ind|  label.names\_unsure.foster.or.adopt\_ind|  label.names\_vet\_ind|  label.names\_vet.check.in.process\_ind|  label.names\_withdrawn\_ind|
|:--------------------------------------------|:------------------|------------:|--------------------------:|------------------------------------:|-------------------------------------:|---------------------------:|-------------------------------------------:|-------------------------:|------------------------------:|-------------------------:|-----------------------------------:|---------------------------:|----------------------------------:|---------------------------:|-----------------------------------:|----------------------------:|------------------------------------------:|------------------------------------------:|---------------------------------:|--------------------------------:|---------------------------------------------:|-------------------------------------:|-----------------------------------------------:|--------------------------:|--------------------------:|----------------------:|-----------------------------:|----------------------------:|-----------------------------------:|---------------------------------:|---------------------------:|-------------------------------:|---------------------------:|----------------------------------------------:|---------------------------------:|-----------------------------------------:|----------------------:|---------------------------------------:|----------------------------:|
| NA                                          | NA                |            0|                         NA|                                   NA|                                    NA|                          NA|                                          NA|                        NA|                             NA|                        NA|                                  NA|                          NA|                                 NA|                          NA|                                  NA|                           NA|                                         NA|                                         NA|                                NA|                               NA|                                            NA|                                    NA|                                              NA|                         NA|                         NA|                     NA|                            NA|                           NA|                                  NA|                                NA|                          NA|                              NA|                          NA|                                             NA|                                NA|                                        NA|                     NA|                                      NA|                           NA|
| ready to adopt                              | ready to adopt    |            1|                          0|                                    0|                                     0|                           0|                                           0|                         0|                              0|                         0|                                   0|                           0|                                  0|                           0|                                   0|                            0|                                          0|                                          0|                                 0|                                0|                                             0|                                     0|                                               0|                          0|                          0|                      0|                             0|                            0|                                   0|                                 1|                           0|                               0|                           0|                                              0|                                 0|                                         0|                      0|                                       0|                            0|
| need info, need vet info, adopted elsewhere | adopted elsewhere |            3|                          0|                                    1|                                     0|                           0|                                           0|                         0|                              0|                         0|                                   0|                           0|                                  0|                           0|                                   0|                            1|                                          0|                                          0|                                 0|                                1|                                             0|                                     0|                                               0|                          0|                          0|                      0|                             0|                            0|                                   0|                                 0|                           0|                               0|                           0|                                              0|                                 0|                                         0|                      0|                                       0|                            0|
| ready to adopt                              | ready to adopt    |            1|                          0|                                    0|                                     0|                           0|                                           0|                         0|                              0|                         0|                                   0|                           0|                                  0|                           0|                                   0|                            0|                                          0|                                          0|                                 0|                                0|                                             0|                                     0|                                               0|                          0|                          0|                      0|                             0|                            0|                                   0|                                 1|                           0|                               0|                           0|                                              0|                                 0|                                         0|                      0|                                       0|                            0|
| NA                                          | NA                |            0|                         NA|                                   NA|                                    NA|                          NA|                                          NA|                        NA|                             NA|                        NA|                                  NA|                          NA|                                 NA|                          NA|                                  NA|                           NA|                                         NA|                                         NA|                                NA|                               NA|                                            NA|                                    NA|                                              NA|                         NA|                         NA|                     NA|                            NA|                           NA|                                  NA|                                NA|                          NA|                              NA|                          NA|                                             NA|                                NA|                                        NA|                     NA|                                      NA|                           NA|
| NA                                          | NA                |            0|                         NA|                                   NA|                                    NA|                          NA|                                          NA|                        NA|                             NA|                        NA|                                  NA|                          NA|                                 NA|                          NA|                                  NA|                           NA|                                         NA|                                         NA|                                NA|                               NA|                                            NA|                                    NA|                                              NA|                         NA|                         NA|                     NA|                            NA|                           NA|                                  NA|                                NA|                          NA|                              NA|                          NA|                                             NA|                                NA|                                        NA|                     NA|                                      NA|                           NA|
| approved                                    | approved          |            1|                          0|                                    0|                                     0|                           1|                                           0|                         0|                              0|                         0|                                   0|                           0|                                  0|                           0|                                   0|                            0|                                          0|                                          0|                                 0|                                0|                                             0|                                     0|                                               0|                          0|                          0|                      0|                             0|                            0|                                   0|                                 0|                           0|                               0|                           0|                                              0|                                 0|                                         0|                      0|                                       0|                            0|
| reviewed with handouts only, adopted        | adopted           |            2|                          1|                                    0|                                     0|                           0|                                           0|                         0|                              0|                         0|                                   0|                           0|                                  0|                           0|                                   0|                            0|                                          0|                                          0|                                 0|                                0|                                             0|                                     0|                                               0|                          0|                          0|                      0|                             0|                            0|                                   0|                                 0|                           0|                               0|                           0|                                              1|                                 0|                                         0|                      0|                                       0|                            0|
| adopted                                     | adopted           |            1|                          1|                                    0|                                     0|                           0|                                           0|                         0|                              0|                         0|                                   0|                           0|                                  0|                           0|                                   0|                            0|                                          0|                                          0|                                 0|                                0|                                             0|                                     0|                                               0|                          0|                          0|                      0|                             0|                            0|                                   0|                                 0|                           0|                               0|                           0|                                              0|                                 0|                                         0|                      0|                                       0|                            0|
| ready for review                            | ready for review  |            1|                          0|                                    0|                                     0|                           0|                                           0|                         0|                              0|                         0|                                   0|                           0|                                  0|                           0|                                   0|                            0|                                          0|                                          0|                                 0|                                0|                                             0|                                     0|                                               0|                          0|                          0|                      0|                             0|                            0|                                   1|                                 0|                           0|                               0|                           0|                                              0|                                 0|                                         0|                      0|                                       0|                            0|

Merge the 4 Datasets
--------------------

``` r
master_apps <- apps_with_indicators %>%
 filter(!is.na(trello_id)) %>%
 inner_join(petpoint_with_indicators,by = "trello_id") %>%
 left_join(actions,by = "trello_id") %>%
 left_join(cards_with_indicators, by = "trello_id")          

dim(master_apps)
```

    ## [1] 456 252

``` r
summary(master_apps)
```

    ##  date_submitted       ideal_adoption_timeline reason_for_adoption
    ##  Min.   :2018-08-31   Length:456              Length:456         
    ##  1st Qu.:2018-09-24   Class :character        Class :character   
    ##  Median :2018-10-24   Mode  :character        Mode  :character   
    ##  Mean   :2018-10-22                                              
    ##  3rd Qu.:2018-11-21                                              
    ##  Max.   :2018-12-31                                              
    ##                                                                  
    ##  specific_animal adults_in_home   children_in_home all_household_agree
    ##  Mode :logical   Min.   : 0.000   Min.   :0.0000   Length:456         
    ##  FALSE:163       1st Qu.: 0.000   1st Qu.:0.0000   Class :character   
    ##  TRUE :293       Median : 1.000   Median :0.0000   Mode  :character   
    ##                  Mean   : 1.042   Mean   :0.3326                      
    ##                  3rd Qu.: 1.000   3rd Qu.:0.0000                      
    ##                  Max.   :11.000   Max.   :4.0000                      
    ##                  NA's   :2        NA's   :2                           
    ##   allergies                           home_owner 
    ##  Length:456         company                :192  
    ##  Class :character   family-friend          : 11  
    ##  Mode  :character   family-member-or-friend: 35  
    ##                     landlord               : 29  
    ##                     myself                 :181  
    ##                     NA's                   :  8  
    ##                                                  
    ##             home_pet_policy  experience        budget_monthly   
    ##  no-but-pets-allowed:  6    Length:456         Min.   :     25  
    ##  not-applicable     :  6    Class :character   1st Qu.:    100  
    ##  not-yet            : 37    Mode  :character   Median :    200  
    ##  yes                :157                       Mean   :   2466  
    ##  yes-with-pet-policy: 18                       3rd Qu.:    300  
    ##  NA's               :232                       Max.   :1000000  
    ##                                                                 
    ##  budget_emergency  home_alone_avg   home_alone_max     pet_kept        
    ##  Min.   :      1   Min.   : 0.000   Min.   : 1.000   Length:456        
    ##  1st Qu.:    500   1st Qu.: 4.000   1st Qu.: 6.000   Class :character  
    ##  Median :   1000   Median : 6.000   Median : 8.000   Mode  :character  
    ##  Mean   :   4074   Mean   : 6.042   Mean   : 8.876                     
    ##  3rd Qu.:   2000   3rd Qu.: 8.000   3rd Qu.:10.000                     
    ##  Max.   :1000000   Max.   :12.000   Max.   :48.000                     
    ##                    NA's   :98       NA's   :118                        
    ##    exercise            needs            return_pet       
    ##  Length:456         Length:456         Length:456        
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##   how_heard          trello_id                   City         State    
    ##  Length:456         Length:456         PHILADELPHIA:355   PA     :421  
    ##  Class :character   Class :character   BRISTOL     :  3   NJ     : 19  
    ##  Mode  :character   Mode  :character   CHALFONT    :  3   NY     :  7  
    ##                                        EAGLEVILLE  :  3   DE     :  3  
    ##                                        HATBORO     :  3   MD     :  3  
    ##                                        PHILA       :  3   MA     :  2  
    ##                                        (Other)     : 86   (Other):  1  
    ##      ZIP            animal_type.x      budget_monthly_ranges
    ##  Length:456         Length:456         <25      :  1        
    ##  Class :character   Class :character   >5000    :  2        
    ##  Mode  :character   Mode  :character   1001-5000:  6        
    ##                                        101-200  :135        
    ##                                        201-500  :128        
    ##                                        26-100   :164        
    ##                                        501-1000 : 20        
    ##  budget_emergency_ranges reason.for.adoption_gift_ind
    ##  <25      :  2           Min.   :0.00000             
    ##  >5000    : 26           1st Qu.:0.00000             
    ##  1001-5000:153           Median :0.00000             
    ##  101-200  : 20           Mean   :0.02697             
    ##  201-500  :113           3rd Qu.:0.00000             
    ##  26-100   : 19           Max.   :1.00000             
    ##  501-1000 :123           NA's   :11                  
    ##  reason.for.adoption_mouser_ind reason.for.adoption_my.kids_ind
    ##  Min.   :0.00000                Min.   :0.0000                 
    ##  1st Qu.:0.00000                1st Qu.:0.0000                 
    ##  Median :0.00000                Median :0.0000                 
    ##  Mean   :0.03596                Mean   :0.1461                 
    ##  3rd Qu.:0.00000                3rd Qu.:0.0000                 
    ##  Max.   :1.00000                Max.   :1.0000                 
    ##  NA's   :11                     NA's   :11                     
    ##  reason.for.adoption_myself_ind reason.for.adoption_other_ind
    ##  Min.   :0.0000                 Min.   :0.00000              
    ##  1st Qu.:1.0000                 1st Qu.:0.00000              
    ##  Median :1.0000                 Median :0.00000              
    ##  Mean   :0.9528                 Mean   :0.04719              
    ##  3rd Qu.:1.0000                 3rd Qu.:0.00000              
    ##  Max.   :1.0000                 Max.   :1.00000              
    ##  NA's   :11                     NA's   :11                   
    ##  reason.for.adoption_protection_ind all.household.agree_a.surprise_ind
    ##  Min.   :0.000000                   Min.   :0.0000                    
    ##  1st Qu.:0.000000                   1st Qu.:0.0000                    
    ##  Median :0.000000                   Median :0.0000                    
    ##  Mean   :0.006742                   Mean   :0.0307                    
    ##  3rd Qu.:0.000000                   3rd Qu.:0.0000                    
    ##  Max.   :1.000000                   Max.   :1.0000                    
    ##  NA's   :11                                                           
    ##  all.household.agree_no_ind all.household.agree_yes_ind
    ##  Min.   :0.000000           Min.   :0.0000             
    ##  1st Qu.:0.000000           1st Qu.:1.0000             
    ##  Median :0.000000           Median :1.0000             
    ##  Mean   :0.002193           Mean   :0.9912             
    ##  3rd Qu.:0.000000           3rd Qu.:1.0000             
    ##  Max.   :1.000000           Max.   :1.0000             
    ##                                                        
    ##  allergies_mildly.allergic_ind allergies_no.allergies_ind
    ##  Min.   :0.0000                Min.   :0.0000            
    ##  1st Qu.:0.0000                1st Qu.:1.0000            
    ##  Median :0.0000                Median :1.0000            
    ##  Mean   :0.1031                Mean   :0.8925            
    ##  3rd Qu.:0.0000                3rd Qu.:1.0000            
    ##  Max.   :1.0000                Max.   :1.0000            
    ##                                                          
    ##  allergies_not.sure_ind allergies_very.allergic_ind home.owner_company_ind
    ##  Min.   :0.00000        Min.   :0                   Min.   :0.0000        
    ##  1st Qu.:0.00000        1st Qu.:0                   1st Qu.:0.0000        
    ##  Median :0.00000        Median :0                   Median :0.0000        
    ##  Mean   :0.02632        Mean   :0                   Mean   :0.4308        
    ##  3rd Qu.:0.00000        3rd Qu.:0                   3rd Qu.:1.0000        
    ##  Max.   :1.00000        Max.   :0                   Max.   :1.0000        
    ##                                                     NA's   :8             
    ##  home.owner_family.friend_ind home.owner_family.member.or.friend_ind
    ##  Min.   :0.00000              Min.   :0.00000                       
    ##  1st Qu.:0.00000              1st Qu.:0.00000                       
    ##  Median :0.00000              Median :0.00000                       
    ##  Mean   :0.02455              Mean   :0.07812                       
    ##  3rd Qu.:0.00000              3rd Qu.:0.00000                       
    ##  Max.   :1.00000              Max.   :1.00000                       
    ##  NA's   :8                    NA's   :8                             
    ##  home.owner_landlord_ind home.owner_myself_ind
    ##  Min.   :0.00000         Min.   :0.0000       
    ##  1st Qu.:0.00000         1st Qu.:0.0000       
    ##  Median :0.00000         Median :0.0000       
    ##  Mean   :0.06473         Mean   :0.4062       
    ##  3rd Qu.:0.00000         3rd Qu.:1.0000       
    ##  Max.   :1.00000         Max.   :1.0000       
    ##  NA's   :8               NA's   :8            
    ##  home.pet.policy_no.but.pets.allowed_ind
    ##  Min.   :0.00000                        
    ##  1st Qu.:0.00000                        
    ##  Median :0.00000                        
    ##  Mean   :0.02667                        
    ##  3rd Qu.:0.00000                        
    ##  Max.   :1.00000                        
    ##  NA's   :231                            
    ##  home.pet.policy_not.applicable_ind home.pet.policy_not.yet_ind
    ##  Min.   :0.00000                    Min.   :0.0000             
    ##  1st Qu.:0.00000                    1st Qu.:0.0000             
    ##  Median :0.00000                    Median :0.0000             
    ##  Mean   :0.03111                    Mean   :0.1644             
    ##  3rd Qu.:0.00000                    3rd Qu.:0.0000             
    ##  Max.   :1.00000                    Max.   :1.0000             
    ##  NA's   :231                        NA's   :231                
    ##  home.pet.policy_yes_ind home.pet.policy_yes.with.pet.policy_ind
    ##  Min.   :0.0000          Min.   :0.00000                        
    ##  1st Qu.:0.0000          1st Qu.:0.00000                        
    ##  Median :1.0000          Median :0.00000                        
    ##  Mean   :0.7022          Mean   :0.08444                        
    ##  3rd Qu.:1.0000          3rd Qu.:0.00000                        
    ##  Max.   :1.0000          Max.   :1.00000                        
    ##  NA's   :231             NA's   :231                            
    ##  experience_bred.sold_ind experience_current.housemates.pet_ind
    ##  Min.   :0.000000         Min.   :0.00000                      
    ##  1st Qu.:0.000000         1st Qu.:0.00000                      
    ##  Median :0.000000         Median :0.00000                      
    ##  Mean   :0.008772         Mean   :0.01754                      
    ##  3rd Qu.:0.000000         3rd Qu.:0.00000                      
    ##  Max.   :1.000000         Max.   :1.00000                      
    ##                                                                
    ##  experience_currently.have.pet_ind experience_euthanized_ind
    ##  Min.   :0.0000                    Min.   :0.0000           
    ##  1st Qu.:0.0000                    1st Qu.:0.0000           
    ##  Median :0.0000                    Median :0.0000           
    ##  Mean   :0.3355                    Mean   :0.3553           
    ##  3rd Qu.:1.0000                    3rd Qu.:1.0000           
    ##  Max.   :1.0000                    Max.   :1.0000           
    ##                                                             
    ##  experience_given.away_ind experience_given.to.shelter_ind
    ##  Min.   :0.00000           Min.   :0.0000                 
    ##  1st Qu.:0.00000           1st Qu.:0.0000                 
    ##  Median :0.00000           Median :0.0000                 
    ##  Mean   :0.07675           Mean   :0.0307                 
    ##  3rd Qu.:0.00000           3rd Qu.:0.0000                 
    ##  Max.   :1.00000           Max.   :1.0000                 
    ##                                                           
    ##  experience_grew.up.with_ind experience_never.lived.with_ind
    ##  Min.   :0.0000              Min.   :0.00000                
    ##  1st Qu.:1.0000              1st Qu.:0.00000                
    ##  Median :1.0000              Median :0.00000                
    ##  Mean   :0.8004              Mean   :0.05263                
    ##  3rd Qu.:1.0000              3rd Qu.:0.00000                
    ##  Max.   :1.0000              Max.   :1.00000                
    ##                                                             
    ##  experience_past.housemates.pet_ind experience_pet.died.in.care_ind
    ##  Min.   :0.0000                     Min.   :0.0000                 
    ##  1st Qu.:0.0000                     1st Qu.:0.0000                 
    ##  Median :0.0000                     Median :0.0000                 
    ##  Mean   :0.4145                     Mean   :0.1754                 
    ##  3rd Qu.:1.0000                     3rd Qu.:0.0000                 
    ##  Max.   :1.0000                     Max.   :1.0000                 
    ##                                                                    
    ##  experience_pet.ran.away_ind budget.monthly.ranges_<25_ind
    ##  Min.   :0.00000             Min.   :0.000000             
    ##  1st Qu.:0.00000             1st Qu.:0.000000             
    ##  Median :0.00000             Median :0.000000             
    ##  Mean   :0.03509             Mean   :0.002193             
    ##  3rd Qu.:0.00000             3rd Qu.:0.000000             
    ##  Max.   :1.00000             Max.   :1.000000             
    ##                                                           
    ##  budget.monthly.ranges_>5000_ind budget.monthly.ranges_1001.5000_ind
    ##  Min.   :0.000000                Min.   :0.00000                    
    ##  1st Qu.:0.000000                1st Qu.:0.00000                    
    ##  Median :0.000000                Median :0.00000                    
    ##  Mean   :0.004386                Mean   :0.01535                    
    ##  3rd Qu.:0.000000                3rd Qu.:0.00000                    
    ##  Max.   :1.000000                Max.   :1.00000                    
    ##                                                                     
    ##  budget.monthly.ranges_101.200_ind budget.monthly.ranges_201.500_ind
    ##  Min.   :0.0000                    Min.   :0.0000                   
    ##  1st Qu.:0.0000                    1st Qu.:0.0000                   
    ##  Median :0.0000                    Median :0.0000                   
    ##  Mean   :0.2961                    Mean   :0.2851                   
    ##  3rd Qu.:1.0000                    3rd Qu.:1.0000                   
    ##  Max.   :1.0000                    Max.   :1.0000                   
    ##                                                                     
    ##  budget.monthly.ranges_26.100_ind budget.monthly.ranges_501.1000_ind
    ##  Min.   :0.0000                   Min.   :0.00000                   
    ##  1st Qu.:0.0000                   1st Qu.:0.00000                   
    ##  Median :0.0000                   Median :0.00000                   
    ##  Mean   :0.3596                   Mean   :0.04605                   
    ##  3rd Qu.:1.0000                   3rd Qu.:0.00000                   
    ##  Max.   :1.0000                   Max.   :1.00000                   
    ##                                                                     
    ##  budget.emergency.ranges_<25_ind budget.emergency.ranges_>5000_ind
    ##  Min.   :0.000000                Min.   :0.00000                  
    ##  1st Qu.:0.000000                1st Qu.:0.00000                  
    ##  Median :0.000000                Median :0.00000                  
    ##  Mean   :0.004386                Mean   :0.05921                  
    ##  3rd Qu.:0.000000                3rd Qu.:0.00000                  
    ##  Max.   :1.000000                Max.   :1.00000                  
    ##                                                                   
    ##  budget.emergency.ranges_1001.5000_ind budget.emergency.ranges_101.200_ind
    ##  Min.   :0.0000                        Min.   :0.00000                    
    ##  1st Qu.:0.0000                        1st Qu.:0.00000                    
    ##  Median :0.0000                        Median :0.00000                    
    ##  Mean   :0.3377                        Mean   :0.04386                    
    ##  3rd Qu.:1.0000                        3rd Qu.:0.00000                    
    ##  Max.   :1.0000                        Max.   :1.00000                    
    ##                                                                           
    ##  budget.emergency.ranges_201.500_ind budget.emergency.ranges_26.100_ind
    ##  Min.   :0.0000                      Min.   :0.00000                   
    ##  1st Qu.:0.0000                      1st Qu.:0.00000                   
    ##  Median :0.0000                      Median :0.00000                   
    ##  Mean   :0.2522                      Mean   :0.04167                   
    ##  3rd Qu.:1.0000                      3rd Qu.:0.00000                   
    ##  Max.   :1.0000                      Max.   :1.00000                   
    ##                                                                        
    ##  budget.emergency.ranges_501.1000_ind home.alone.avg_0_ind
    ##  Min.   :0.0000                       Min.   :0.00000     
    ##  1st Qu.:0.0000                       1st Qu.:0.00000     
    ##  Median :0.0000                       Median :0.00000     
    ##  Mean   :0.2741                       Mean   :0.01385     
    ##  3rd Qu.:1.0000                       3rd Qu.:0.00000     
    ##  Max.   :1.0000                       Max.   :1.00000     
    ##                                       NA's   :95          
    ##  home.alone.avg_1_ind home.alone.avg_10_ind home.alone.avg_11_ind
    ##  Min.   :0.00000      Min.   :0.00000       Min.   :0.00000      
    ##  1st Qu.:0.00000      1st Qu.:0.00000       1st Qu.:0.00000      
    ##  Median :0.00000      Median :0.00000       Median :0.00000      
    ##  Mean   :0.01939      Mean   :0.06371       Mean   :0.00277      
    ##  3rd Qu.:0.00000      3rd Qu.:0.00000       3rd Qu.:0.00000      
    ##  Max.   :1.00000      Max.   :1.00000       Max.   :1.00000      
    ##  NA's   :95           NA's   :95            NA's   :95           
    ##  home.alone.avg_12_ind home.alone.avg_13_ind home.alone.avg_2_ind
    ##  Min.   :0.00000       Min.   :0             Min.   :0.00000     
    ##  1st Qu.:0.00000       1st Qu.:0             1st Qu.:0.00000     
    ##  Median :0.00000       Median :0             Median :0.00000     
    ##  Mean   :0.00831       Mean   :0             Mean   :0.04986     
    ##  3rd Qu.:0.00000       3rd Qu.:0             3rd Qu.:0.00000     
    ##  Max.   :1.00000       Max.   :0             Max.   :1.00000     
    ##  NA's   :95            NA's   :95            NA's   :95          
    ##  home.alone.avg_24_ind home.alone.avg_3_ind home.alone.avg_4_ind
    ##  Min.   :0             Min.   :0.00000      Min.   :0.0000      
    ##  1st Qu.:0             1st Qu.:0.00000      1st Qu.:0.0000      
    ##  Median :0             Median :0.00000      Median :0.0000      
    ##  Mean   :0             Mean   :0.08587      Mean   :0.1385      
    ##  3rd Qu.:0             3rd Qu.:0.00000      3rd Qu.:0.0000      
    ##  Max.   :0             Max.   :1.00000      Max.   :1.0000      
    ##  NA's   :95            NA's   :95           NA's   :95          
    ##  home.alone.avg_5_ind home.alone.avg_6_ind home.alone.avg_7_ind
    ##  Min.   :0.000        Min.   :0.0000       Min.   :0.00000     
    ##  1st Qu.:0.000        1st Qu.:0.0000       1st Qu.:0.00000     
    ##  Median :0.000        Median :0.0000       Median :0.00000     
    ##  Mean   :0.133        Mean   :0.1191       Mean   :0.05817     
    ##  3rd Qu.:0.000        3rd Qu.:0.0000       3rd Qu.:0.00000     
    ##  Max.   :1.000        Max.   :1.0000       Max.   :1.00000     
    ##  NA's   :95           NA's   :95           NA's   :95          
    ##  home.alone.avg_8_ind home.alone.avg_9_ind home.alone.max_0_ind
    ##  Min.   :0.000        Min.   :0.00000      Min.   :0           
    ##  1st Qu.:0.000        1st Qu.:0.00000      1st Qu.:0           
    ##  Median :0.000        Median :0.00000      Median :0           
    ##  Mean   :0.241        Mean   :0.07202      Mean   :0           
    ##  3rd Qu.:0.000        3rd Qu.:0.00000      3rd Qu.:0           
    ##  Max.   :1.000        Max.   :1.00000      Max.   :0           
    ##  NA's   :95           NA's   :95           NA's   :115         
    ##  home.alone.max_1_ind home.alone.max_10_ind home.alone.max_11_ind
    ##  Min.   :0.00000      Min.   :0.0000        Min.   :0.00000      
    ##  1st Qu.:0.00000      1st Qu.:0.0000        1st Qu.:0.00000      
    ##  Median :0.00000      Median :0.0000        Median :0.00000      
    ##  Mean   :0.00587      Mean   :0.1554        Mean   :0.00587      
    ##  3rd Qu.:0.00000      3rd Qu.:0.0000        3rd Qu.:0.00000      
    ##  Max.   :1.00000      Max.   :1.0000        Max.   :1.00000      
    ##  NA's   :115          NA's   :115           NA's   :115          
    ##  home.alone.max_12_ind home.alone.max_13_ind home.alone.max_14_ind
    ##  Min.   :0.0000        Min.   :0.00000       Min.   :0.00000      
    ##  1st Qu.:0.0000        1st Qu.:0.00000       1st Qu.:0.00000      
    ##  Median :0.0000        Median :0.00000       Median :0.00000      
    ##  Mean   :0.1056        Mean   :0.00587       Mean   :0.00587      
    ##  3rd Qu.:0.0000        3rd Qu.:0.00000       3rd Qu.:0.00000      
    ##  Max.   :1.0000        Max.   :1.00000       Max.   :1.00000      
    ##  NA's   :115           NA's   :115           NA's   :115          
    ##  home.alone.max_15_ind home.alone.max_16_ind home.alone.max_18_ind
    ##  Min.   :0.00000       Min.   :0.00000       Min.   :0.00000      
    ##  1st Qu.:0.00000       1st Qu.:0.00000       1st Qu.:0.00000      
    ##  Median :0.00000       Median :0.00000       Median :0.00000      
    ##  Mean   :0.00587       Mean   :0.00293       Mean   :0.00587      
    ##  3rd Qu.:0.00000       3rd Qu.:0.00000       3rd Qu.:0.00000      
    ##  Max.   :1.00000       Max.   :1.00000       Max.   :1.00000      
    ##  NA's   :115           NA's   :115           NA's   :115          
    ##  home.alone.max_2_ind home.alone.max_20_ind home.alone.max_23_ind
    ##  Min.   :0.00000      Min.   :0             Min.   :0            
    ##  1st Qu.:0.00000      1st Qu.:0             1st Qu.:0            
    ##  Median :0.00000      Median :0             Median :0            
    ##  Mean   :0.03226      Mean   :0             Mean   :0            
    ##  3rd Qu.:0.00000      3rd Qu.:0             3rd Qu.:0            
    ##  Max.   :1.00000      Max.   :0             Max.   :0            
    ##  NA's   :115          NA's   :115           NA's   :115          
    ##  home.alone.max_24_ind home.alone.max_28_ind home.alone.max_3_ind
    ##  Min.   :0.00000       Min.   :0.00000       Min.   :0.00000     
    ##  1st Qu.:0.00000       1st Qu.:0.00000       1st Qu.:0.00000     
    ##  Median :0.00000       Median :0.00000       Median :0.00000     
    ##  Mean   :0.02639       Mean   :0.00587       Mean   :0.03812     
    ##  3rd Qu.:0.00000       3rd Qu.:0.00000       3rd Qu.:0.00000     
    ##  Max.   :1.00000       Max.   :1.00000       Max.   :1.00000     
    ##  NA's   :115           NA's   :115           NA's   :115         
    ##  home.alone.max_30_ind home.alone.max_36_ind home.alone.max_4_ind
    ##  Min.   :0             Min.   :0.00000       Min.   :0.00000     
    ##  1st Qu.:0             1st Qu.:0.00000       1st Qu.:0.00000     
    ##  Median :0             Median :0.00000       Median :0.00000     
    ##  Mean   :0             Mean   :0.00587       Mean   :0.06745     
    ##  3rd Qu.:0             3rd Qu.:0.00000       3rd Qu.:0.00000     
    ##  Max.   :0             Max.   :1.00000       Max.   :1.00000     
    ##  NA's   :115           NA's   :115           NA's   :115         
    ##  home.alone.max_48_ind home.alone.max_5_ind home.alone.max_6_ind
    ##  Min.   :0.0000        Min.   :0.00000      Min.   :0.0000      
    ##  1st Qu.:0.0000        1st Qu.:0.00000      1st Qu.:0.0000      
    ##  Median :0.0000        Median :0.00000      Median :0.0000      
    ##  Mean   :0.0088        Mean   :0.08504      Mean   :0.1261      
    ##  3rd Qu.:0.0000        3rd Qu.:0.00000      3rd Qu.:0.0000      
    ##  Max.   :1.0000        Max.   :1.00000      Max.   :1.0000      
    ##  NA's   :115           NA's   :115          NA's   :115         
    ##  home.alone.max_7_ind home.alone.max_8_ind home.alone.max_9_ind
    ##  Min.   :0.00000      Min.   :0.0000       Min.   :0.00000     
    ##  1st Qu.:0.00000      1st Qu.:0.0000       1st Qu.:0.00000     
    ##  Median :0.00000      Median :0.0000       Median :0.00000     
    ##  Mean   :0.04106      Mean   :0.2346       Mean   :0.04692     
    ##  3rd Qu.:0.00000      3rd Qu.:0.0000       3rd Qu.:0.00000     
    ##  Max.   :1.00000      Max.   :1.0000       Max.   :1.00000     
    ##  NA's   :115          NA's   :115          NA's   :115         
    ##  pet.kept_crate_ind pet.kept_inside.only_ind pet.kept_inside.outside_ind
    ##  Min.   :0.00000    Min.   :0.000            Min.   :0.00000            
    ##  1st Qu.:0.00000    1st Qu.:1.000            1st Qu.:0.00000            
    ##  Median :0.00000    Median :1.000            Median :0.00000            
    ##  Mean   :0.01566    Mean   :0.962            Mean   :0.01342            
    ##  3rd Qu.:0.00000    3rd Qu.:1.000            3rd Qu.:0.00000            
    ##  Max.   :1.00000    Max.   :1.000            Max.   :1.00000            
    ##  NA's   :9          NA's   :9                NA's   :9                  
    ##  pet.kept_inside.with.yard.access_ind pet.kept_leash.harness_ind
    ##  Min.   :0.00000                      Min.   :0.00000           
    ##  1st Qu.:0.00000                      1st Qu.:0.00000           
    ##  Median :0.00000                      Median :0.00000           
    ##  Mean   :0.03356                      Mean   :0.03803           
    ##  3rd Qu.:0.00000                      3rd Qu.:0.00000           
    ##  Max.   :1.00000                      Max.   :1.00000           
    ##  NA's   :9                            NA's   :9                 
    ##  pet.kept_other_ind pet.kept_outside.only_ind
    ##  Min.   :0.000000   Min.   :0.000000         
    ##  1st Qu.:0.000000   1st Qu.:0.000000         
    ##  Median :0.000000   Median :0.000000         
    ##  Mean   :0.008949   Mean   :0.006711         
    ##  3rd Qu.:0.000000   3rd Qu.:0.000000         
    ##  Max.   :1.000000   Max.   :1.000000         
    ##  NA's   :9          NA's   :9                
    ##  pet.kept_supervised.in.my.yard_ind
    ##  Min.   :0.00000                   
    ##  1st Qu.:0.00000                   
    ##  Median :0.00000                   
    ##  Mean   :0.02908                   
    ##  3rd Qu.:0.00000                   
    ##  Max.   :1.00000                   
    ##  NA's   :9                         
    ##  pet.kept_unsupervised.access.to.my.yard.doggie.door.etc_ind
    ##  Min.   :0.000000                                           
    ##  1st Qu.:0.000000                                           
    ##  Median :0.000000                                           
    ##  Mean   :0.004474                                           
    ##  3rd Qu.:0.000000                                           
    ##  Max.   :1.000000                                           
    ##  NA's   :9                                                  
    ##  exercise_another.pet_ind exercise_dog.parks_ind
    ##  Min.   :0.00000          Min.   :0.0000        
    ##  1st Qu.:0.00000          1st Qu.:0.0000        
    ##  Median :0.00000          Median :0.0000        
    ##  Mean   :0.08764          Mean   :0.1258        
    ##  3rd Qu.:0.00000          3rd Qu.:0.0000        
    ##  Max.   :1.00000          Max.   :1.0000        
    ##  NA's   :11               NA's   :11            
    ##  exercise_jogging.together_ind exercise_not.much_ind
    ##  Min.   :0.00000               Min.   :0.0000       
    ##  1st Qu.:0.00000               1st Qu.:0.0000       
    ##  Median :0.00000               Median :0.0000       
    ##  Mean   :0.06067               Mean   :0.1191       
    ##  3rd Qu.:0.00000               3rd Qu.:0.0000       
    ##  Max.   :1.00000               Max.   :1.0000       
    ##  NA's   :11                    NA's   :11           
    ##  exercise_other.cats_ind exercise_other.pets_ind
    ##  Min.   :0.0000          Min.   :0.00000        
    ##  1st Qu.:0.0000          1st Qu.:0.00000        
    ##  Median :0.0000          Median :0.00000        
    ##  Mean   :0.3528          Mean   :0.01348        
    ##  3rd Qu.:1.0000          3rd Qu.:0.00000        
    ##  Max.   :1.0000          Max.   :1.00000        
    ##  NA's   :11              NA's   :11             
    ##  exercise_playing.in.my.yard_ind exercise_toy.mice_ind
    ##  Min.   :0.0000                  Min.   :0.0000       
    ##  1st Qu.:0.0000                  1st Qu.:1.0000       
    ##  Median :0.0000                  Median :1.0000       
    ##  Mean   :0.1011                  Mean   :0.7888       
    ##  3rd Qu.:0.0000                  3rd Qu.:1.0000       
    ##  Max.   :1.0000                  Max.   :1.0000       
    ##  NA's   :11                      NA's   :11           
    ##  exercise_walks.off.leash_ind exercise_walks.on.leash_ind
    ##  Min.   :0.00000              Min.   :0.000              
    ##  1st Qu.:0.00000              1st Qu.:0.000              
    ##  Median :0.00000              Median :0.000              
    ##  Mean   :0.03371              Mean   :0.173              
    ##  3rd Qu.:0.00000              3rd Qu.:0.000              
    ##  Max.   :1.00000              Max.   :1.000              
    ##  NA's   :11                   NA's   :11                 
    ##  exercise_wand.toys_ind needs_declaw_ind  needs_groom.myself_ind
    ##  Min.   :0.0000         Min.   :0.00000   Min.   :0.00000       
    ##  1st Qu.:0.0000         1st Qu.:0.00000   1st Qu.:0.00000       
    ##  Median :1.0000         Median :0.00000   Median :0.00000       
    ##  Mean   :0.6989         Mean   :0.01573   Mean   :0.08764       
    ##  3rd Qu.:1.0000         3rd Qu.:0.00000   3rd Qu.:0.00000       
    ##  Max.   :1.0000         Max.   :1.00000   Max.   :1.00000       
    ##  NA's   :11             NA's   :11        NA's   :11            
    ##  needs_nail.clip_ind needs_no.grooming_ind needs_not.sure_ind
    ##  Min.   :0.0000      Min.   :0.000000      Min.   :0.00000   
    ##  1st Qu.:0.0000      1st Qu.:0.000000      1st Qu.:0.00000   
    ##  Median :0.0000      Median :0.000000      Median :0.00000   
    ##  Mean   :0.4966      Mean   :0.004494      Mean   :0.06966   
    ##  3rd Qu.:1.0000      3rd Qu.:0.000000      3rd Qu.:0.00000   
    ##  Max.   :1.0000      Max.   :1.000000      Max.   :1.00000   
    ##  NA's   :11          NA's   :11            NA's   :11        
    ##  needs_other_ind   needs_professional.groomer_ind
    ##  Min.   :0.00000   Min.   :0.0000                
    ##  1st Qu.:0.00000   1st Qu.:0.0000                
    ##  Median :0.00000   Median :0.0000                
    ##  Mean   :0.02697   Mean   :0.1371                
    ##  3rd Qu.:0.00000   3rd Qu.:0.0000                
    ##  Max.   :1.00000   Max.   :1.0000                
    ##  NA's   :11        NA's   :11                    
    ##  needs_scratching.post_ind return.pet_allergies.appear_ind
    ##  Min.   :0.0000            Min.   :0.0000                 
    ##  1st Qu.:1.0000            1st Qu.:0.0000                 
    ##  Median :1.0000            Median :0.0000                 
    ##  Mean   :0.7573            Mean   :0.1031                 
    ##  3rd Qu.:1.0000            3rd Qu.:0.0000                 
    ##  Max.   :1.0000            Max.   :1.0000                 
    ##  NA's   :11                                               
    ##  return.pet_becomes.aggressive_ind return.pet_destructive_ind
    ##  Min.   :0.0000                    Min.   :0.000000          
    ##  1st Qu.:0.0000                    1st Qu.:0.000000          
    ##  Median :0.0000                    Median :0.000000          
    ##  Mean   :0.3026                    Mean   :0.006579          
    ##  3rd Qu.:1.0000                    3rd Qu.:0.000000          
    ##  Max.   :1.0000                    Max.   :1.000000          
    ##                                                              
    ##  return.pet_jumps.on.counters_ind return.pet_jumps.on.furniture_ind
    ##  Min.   :0.000000                 Min.   :0                        
    ##  1st Qu.:0.000000                 1st Qu.:0                        
    ##  Median :0.000000                 Median :0                        
    ##  Mean   :0.002193                 Mean   :0                        
    ##  3rd Qu.:0.000000                 3rd Qu.:0                        
    ##  Max.   :1.000000                 Max.   :0                        
    ##                                                                    
    ##  return.pet_litter.box.issues_ind return.pet_moving.too.far_ind
    ##  Min.   :0.00000                  Min.   :0.00000              
    ##  1st Qu.:0.00000                  1st Qu.:0.00000              
    ##  Median :0.00000                  Median :0.00000              
    ##  Mean   :0.04605                  Mean   :0.03947              
    ##  3rd Qu.:0.00000                  3rd Qu.:0.00000              
    ##  Max.   :1.00000                  Max.   :1.00000              
    ##                                                                
    ##  return.pet_new.baby_ind return.pet_none_ind
    ##  Min.   :0.000000        Min.   :0.0000     
    ##  1st Qu.:0.000000        1st Qu.:0.0000     
    ##  Median :0.000000        Median :1.0000     
    ##  Mean   :0.002193        Mean   :0.6316     
    ##  3rd Qu.:0.000000        3rd Qu.:1.0000     
    ##  Max.   :1.000000        Max.   :1.0000     
    ##                                             
    ##  return.pet_not.allowed.new.living.space_ind
    ##  Min.   :0.00000                            
    ##  1st Qu.:0.00000                            
    ##  Median :0.00000                            
    ##  Mean   :0.01316                            
    ##  3rd Qu.:0.00000                            
    ##  Max.   :1.00000                            
    ##                                             
    ##  return.pet_not.enough.time_ind return.pet_not.housebroken_ind
    ##  Min.   :0.000000               Min.   :0.000000              
    ##  1st Qu.:0.000000               1st Qu.:0.000000              
    ##  Median :0.000000               Median :0.000000              
    ##  Mean   :0.008772               Mean   :0.008772              
    ##  3rd Qu.:0.000000               3rd Qu.:0.000000              
    ##  Max.   :1.000000               Max.   :1.000000              
    ##                                                               
    ##  return.pet_other_ind return.pet_pet.sheds_ind
    ##  Min.   :0.00000      Min.   :0.000000        
    ##  1st Qu.:0.00000      1st Qu.:0.000000        
    ##  Median :0.00000      Median :0.000000        
    ##  Mean   :0.03728      Mean   :0.006579        
    ##  3rd Qu.:0.00000      3rd Qu.:0.000000        
    ##  Max.   :1.00000      Max.   :1.000000        
    ##                                               
    ##  return.pet_scratches.furniture_ind return.pet_too.playful_ind
    ##  Min.   :0.000000                   Min.   :0                 
    ##  1st Qu.:0.000000                   1st Qu.:0                 
    ##  Median :0.000000                   Median :0                 
    ##  Mean   :0.008772                   Mean   :0                 
    ##  3rd Qu.:0.000000                   3rd Qu.:0                 
    ##  Max.   :1.000000                   Max.   :0                 
    ##                                                               
    ##  return.pet_vet.becomes.expensive_ind animal_type.y     
    ##  Min.   :0.00000                      Length:456        
    ##  1st Qu.:0.00000                      Class :character  
    ##  Median :0.00000                      Mode  :character  
    ##  Mean   :0.05044                                        
    ##  3rd Qu.:0.00000                                        
    ##  Max.   :1.00000                                        
    ##                                                         
    ##    species          primary_breed      secondary_breed   
    ##  Length:456         Length:456         Length:456        
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##    markings            gender            altered         
    ##  Length:456         Length:456         Length:456        
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##       dob               age_intake    intake_asilomar   
    ##  Min.   :2006-05-14   Min.   : -1.0   Length:456        
    ##  1st Qu.:2016-08-03   1st Qu.:  1.0   Class :character  
    ##  Median :2018-04-27   Median :  4.0   Mode  :character  
    ##  Mean   :2017-02-21   Mean   : 17.9                     
    ##  3rd Qu.:2018-07-17   3rd Qu.: 24.0                     
    ##  Max.   :2018-10-09   Max.   :144.0                     
    ##                                                         
    ##  intake_condition    intake_date                  intake_type       
    ##  Length:456         Min.   :2017-12-01 14:53:00   Length:456        
    ##  Class :character   1st Qu.:2018-08-03 17:19:00   Class :character  
    ##  Mode  :character   Median :2018-09-06 13:42:30   Mode  :character  
    ##                     Mean   :2018-09-01 06:53:37                     
    ##                     3rd Qu.:2018-10-10 22:12:15                     
    ##                     Max.   :2019-01-04 15:57:00                     
    ##                                                                     
    ##  intake_subtype     intake_reason      intake_sitename   
    ##  Length:456         Length:456         Length:456        
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  agency_name        outcome_asilomar    release_date                
    ##  Length:456         Length:456         Min.   :2017-12-28 18:28:00  
    ##  Class :character   Class :character   1st Qu.:2018-10-17 19:26:15  
    ##  Mode  :character   Mode  :character   Median :2018-11-16 03:54:00  
    ##                                        Mean   :2018-11-13 04:49:28  
    ##                                        3rd Qu.:2018-12-10 21:41:30  
    ##                                        Max.   :2019-01-23 10:50:00  
    ##                                                                     
    ##   outcome_date                 outcome_type       outcome_subtype   
    ##  Min.   :2017-12-28 18:28:00   Length:456         Length:456        
    ##  1st Qu.:2018-10-17 19:26:15   Class :character   Class :character  
    ##  Median :2018-11-16 03:54:00   Mode  :character   Mode  :character  
    ##  Mean   :2018-11-13 04:49:28                                        
    ##  3rd Qu.:2018-12-10 21:41:30                                        
    ##  Max.   :2019-01-23 10:50:00                                        
    ##                                                                     
    ##  outcome_sitename   outcome_city       outcome_state     
    ##  Length:456         Length:456         Length:456        
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  outcome_ZIP                new_age_group  process_time     
    ##  Length:456         <4 weeks       :177   Min.   :  0.0007  
    ##  Class :character   2-4years       : 55   1st Qu.: 28.9194  
    ##  Mode  :character   4-12 weeks     : 50   Median : 69.9188  
    ##                     6months-1year  : 45   Mean   : 72.9555  
    ##                     1-2years       : 43   3rd Qu.: 98.2585  
    ##                     12weeks-6months: 31   Max.   :362.0743  
    ##                     (Other)        : 55                     
    ##  process_time_periods new.age.group_<4.weeks_ind
    ##  31-90 days:197       Min.   :0.0000            
    ##  91-180days:121       1st Qu.:0.0000            
    ##  11-30 days: 76       Median :0.0000            
    ##  6-10 days : 21       Mean   :0.4013            
    ##  >180 days : 21       3rd Qu.:1.0000            
    ##  4-5 days  :  9       Max.   :1.0000            
    ##  (Other)   : 11                                 
    ##  new.age.group_1.2years_ind new.age.group_12weeks.6months_ind
    ##  Min.   :0.00000            Min.   :0.00000                  
    ##  1st Qu.:0.00000            1st Qu.:0.00000                  
    ##  Median :0.00000            Median :0.00000                  
    ##  Mean   :0.09868            Mean   :0.07895                  
    ##  3rd Qu.:0.00000            3rd Qu.:0.00000                  
    ##  Max.   :1.00000            Max.   :1.00000                  
    ##                                                              
    ##  new.age.group_2.4years_ind new.age.group_4.12.weeks_ind
    ##  Min.   :0.0000             Min.   :0.0000              
    ##  1st Qu.:0.0000             1st Qu.:0.0000              
    ##  Median :0.0000             Median :0.0000              
    ##  Mean   :0.1316             Mean   :0.1184              
    ##  3rd Qu.:0.0000             3rd Qu.:0.0000              
    ##  Max.   :1.0000             Max.   :1.0000              
    ##                                                         
    ##  new.age.group_4.6years_ind new.age.group_6.10years_ind
    ##  Min.   :0.0000             Min.   :0.00000            
    ##  1st Qu.:0.0000             1st Qu.:0.00000            
    ##  Median :0.0000             Median :0.00000            
    ##  Mean   :0.0636             Mean   :0.05263            
    ##  3rd Qu.:0.0000             3rd Qu.:0.00000            
    ##  Max.   :1.0000             Max.   :1.00000            
    ##                                                        
    ##  new.age.group_6months.1year_ind new.age.group_NA_ind
    ##  Min.   :0.0000                  Min.   :0           
    ##  1st Qu.:0.0000                  1st Qu.:0           
    ##  Median :0.0000                  Median :0           
    ##  Mean   :0.1031                  Mean   :0           
    ##  3rd Qu.:0.0000                  3rd Qu.:0           
    ##  Max.   :1.0000                  Max.   :0           
    ##                                                      
    ##  new.age.group_older.than.10years_ind animal_type.x.x   
    ##  Min.   :0.000000                     Length:456        
    ##  1st Qu.:0.000000                     Class :character  
    ##  Median :0.000000                     Mode  :character  
    ##  Mean   :0.006579                                       
    ##  3rd Qu.:0.000000                                       
    ##  Max.   :1.000000                                       
    ##                                                         
    ##    date_start                  checklist_ACCT  checklist_CHQ   
    ##  Min.   :2018-05-10 20:24:18   Min.   :10.89   Min.   : 0.000  
    ##  1st Qu.:2018-09-26 14:41:27   1st Qu.:10.89   1st Qu.: 0.100  
    ##  Median :2018-10-22 16:02:13   Median :10.89   Median : 0.970  
    ##  Mean   :2018-10-21 17:09:35   Mean   :10.89   Mean   : 2.095  
    ##  3rd Qu.:2018-11-19 00:41:43   3rd Qu.:10.89   3rd Qu.: 2.780  
    ##  Max.   :2018-12-31 23:02:02   Max.   :10.89   Max.   :31.820  
    ##  NA's   :9                     NA's   :455     NA's   :21      
    ##   checklist_LL     checklist_PP     checklist_SPCA   checklist_TR   
    ##  Min.   : 0.000   Min.   : 0.0000   Min.   : 1.25   Min.   : 0.000  
    ##  1st Qu.: 0.120   1st Qu.: 0.1175   1st Qu.: 3.66   1st Qu.: 0.100  
    ##  Median : 1.030   Median : 1.0250   Median : 6.07   Median : 0.950  
    ##  Mean   : 2.540   Mean   : 2.2510   Mean   : 6.07   Mean   : 2.071  
    ##  3rd Qu.: 3.075   3rd Qu.: 2.9425   3rd Qu.: 8.48   3rd Qu.: 2.805  
    ##  Max.   :61.900   Max.   :31.8200   Max.   :10.89   Max.   :31.820  
    ##  NA's   :20       NA's   :20        NA's   :454     NA's   :18      
    ##  checklist_VET      wday_start dateLastActivity      due     
    ##  Min.   : 0.000   Fri    :77   Min.   :NA       Min.   :NA   
    ##  1st Qu.: 0.360   Wed    :76   1st Qu.:NA       1st Qu.:NA   
    ##  Median : 1.780   Sat    :75   Median :NA       Median :NA   
    ##  Mean   : 3.256   Mon    :63   Mean   :NA       Mean   :NA   
    ##  3rd Qu.: 4.760   Sun    :59   3rd Qu.:NA       3rd Qu.:NA   
    ##  Max.   :31.820   (Other):97   Max.   :NA       Max.   :NA   
    ##  NA's   :28       NA's   : 9   NA's   :456      NA's   :456  
    ##  animal_type.y.y    label_names         last_label          num_labels   
    ##  Length:456         Length:456         Length:456         Min.   :0.000  
    ##  Class :character   Class :character   Class :character   1st Qu.:1.000  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :1.000  
    ##                                                           Mean   :1.382  
    ##                                                           3rd Qu.:2.000  
    ##                                                           Max.   :4.000  
    ##                                                           NA's   :1      
    ##  label.names_adopted_ind label.names_adopted.elsewhere_ind
    ##  Min.   :0.0000          Min.   :0                        
    ##  1st Qu.:1.0000          1st Qu.:0                        
    ##  Median :1.0000          Median :0                        
    ##  Mean   :0.9376          Mean   :0                        
    ##  3rd Qu.:1.0000          3rd Qu.:0                        
    ##  Max.   :1.0000          Max.   :0                        
    ##  NA's   :7               NA's   :7                        
    ##  label.names_adoption.follow.up_ind label.names_approved_ind
    ##  Min.   :0.00000                    Min.   :0.000000        
    ##  1st Qu.:0.00000                    1st Qu.:0.000000        
    ##  Median :0.00000                    Median :0.000000        
    ##  Mean   :0.02005                    Mean   :0.006682        
    ##  3rd Qu.:0.00000                    3rd Qu.:0.000000        
    ##  Max.   :1.00000                    Max.   :1.000000        
    ##  NA's   :7                          NA's   :7               
    ##  label.names_approved.with.limitation_ind label.names_checks_ind
    ##  Min.   :0.00000                          Min.   :0             
    ##  1st Qu.:0.00000                          1st Qu.:0             
    ##  Median :0.00000                          Median :0             
    ##  Mean   :0.01782                          Mean   :0             
    ##  3rd Qu.:0.00000                          3rd Qu.:0             
    ##  Max.   :1.00000                          Max.   :0             
    ##  NA's   :7                                NA's   :7             
    ##  label.names_declaw.only_ind label.names_denied_ind
    ##  Min.   :0                   Min.   :0             
    ##  1st Qu.:0                   1st Qu.:0             
    ##  Median :0                   Median :0             
    ##  Mean   :0                   Mean   :0             
    ##  3rd Qu.:0                   3rd Qu.:0             
    ##  Max.   :0                   Max.   :0             
    ##  NA's   :7                   NA's   :7             
    ##  label.names_do.not.follow.up_ind label.names_dog.meet_ind
    ##  Min.   :0                        Min.   :0.000000        
    ##  1st Qu.:0                        1st Qu.:0.000000        
    ##  Median :0                        Median :0.000000        
    ##  Mean   :0                        Mean   :0.002227        
    ##  3rd Qu.:0                        3rd Qu.:0.000000        
    ##  Max.   :0                        Max.   :1.000000        
    ##  NA's   :7                        NA's   :7               
    ##  label.names_foster.to.adopt_ind label.names_landlord_ind
    ##  Min.   :0                       Min.   :0               
    ##  1st Qu.:0                       1st Qu.:0               
    ##  Median :0                       Median :0               
    ##  Mean   :0                       Mean   :0               
    ##  3rd Qu.:0                       3rd Qu.:0               
    ##  Max.   :0                       Max.   :0               
    ##  NA's   :7                       NA's   :7               
    ##  label.names_manager.decision_ind label.names_need.info_ind
    ##  Min.   :0                        Min.   :0                
    ##  1st Qu.:0                        1st Qu.:0                
    ##  Median :0                        Median :0                
    ##  Mean   :0                        Mean   :0                
    ##  3rd Qu.:0                        3rd Qu.:0                
    ##  Max.   :0                        Max.   :0                
    ##  NA's   :7                        NA's   :7                
    ##  label.names_need.proof.of.ownership_ind
    ##  Min.   :0                              
    ##  1st Qu.:0                              
    ##  Median :0                              
    ##  Mean   :0                              
    ##  3rd Qu.:0                              
    ##  Max.   :0                              
    ##  NA's   :7                              
    ##  label.names_need.roommates.vet.info_ind label.names_need.to.see.id_ind
    ##  Min.   :0                               Min.   :0.000000              
    ##  1st Qu.:0                               1st Qu.:0.000000              
    ##  Median :0                               Median :0.000000              
    ##  Mean   :0                               Mean   :0.002227              
    ##  3rd Qu.:0                               3rd Qu.:0.000000              
    ##  Max.   :0                               Max.   :1.000000              
    ##  NA's   :7                               NA's   :7                     
    ##  label.names_need.vet.info_ind label.names_need.written.ll.permission_ind
    ##  Min.   :0                     Min.   :0                                 
    ##  1st Qu.:0                     1st Qu.:0                                 
    ##  Median :0                     Median :0                                 
    ##  Mean   :0                     Mean   :0                                 
    ##  3rd Qu.:0                     3rd Qu.:0                                 
    ##  Max.   :0                     Max.   :0                                 
    ##  NA's   :7                     NA's   :7                                 
    ##  label.names_needs.app.attached_ind
    ##  Min.   :0                         
    ##  1st Qu.:0                         
    ##  Median :0                         
    ##  Mean   :0                         
    ##  3rd Qu.:0                         
    ##  Max.   :0                         
    ##  NA's   :7                         
    ##  label.names_needs.review.before.approval_ind label.names_not.s.n_ind
    ##  Min.   :0                                    Min.   :0.000000       
    ##  1st Qu.:0                                    1st Qu.:0.000000       
    ##  Median :0                                    Median :0.000000       
    ##  Mean   :0                                    Mean   :0.004454       
    ##  3rd Qu.:0                                    3rd Qu.:0.000000       
    ##  Max.   :0                                    Max.   :1.000000       
    ##  NA's   :7                                    NA's   :7              
    ##  label.names_not.utd_ind label.names_opa_ind label.names_pet.policy_ind
    ##  Min.   :0.000000        Min.   :0           Min.   :0                 
    ##  1st Qu.:0.000000        1st Qu.:0           1st Qu.:0                 
    ##  Median :0.000000        Median :0           Median :0                 
    ##  Mean   :0.004454        Mean   :0           Mean   :0                 
    ##  3rd Qu.:0.000000        3rd Qu.:0           3rd Qu.:0                 
    ##  Max.   :1.000000        Max.   :0           Max.   :0                 
    ##  NA's   :7               NA's   :7           NA's   :7                 
    ##  label.names_questions_ind label.names_ready.for.review_ind
    ##  Min.   :0                 Min.   :0.000000                
    ##  1st Qu.:0                 1st Qu.:0.000000                
    ##  Median :0                 Median :0.000000                
    ##  Mean   :0                 Mean   :0.006682                
    ##  3rd Qu.:0                 3rd Qu.:0.000000                
    ##  Max.   :0                 Max.   :1.000000                
    ##  NA's   :7                 NA's   :7                       
    ##  label.names_ready.to.adopt_ind label.names_red.flag_ind
    ##  Min.   :0.00000                Min.   :0.00000         
    ##  1st Qu.:0.00000                1st Qu.:0.00000         
    ##  Median :0.00000                Median :0.00000         
    ##  Mean   :0.08018                Mean   :0.01336         
    ##  3rd Qu.:0.00000                3rd Qu.:0.00000         
    ##  Max.   :1.00000                Max.   :1.00000         
    ##  NA's   :7                      NA's   :7               
    ##  label.names_rescue.check_ind label.names_returned_ind
    ##  Min.   :0                    Min.   :0.00000         
    ##  1st Qu.:0                    1st Qu.:0.00000         
    ##  Median :0                    Median :0.00000         
    ##  Mean   :0                    Mean   :0.01782         
    ##  3rd Qu.:0                    3rd Qu.:0.00000         
    ##  Max.   :0                    Max.   :1.00000         
    ##  NA's   :7                    NA's   :7               
    ##  label.names_reviewed.with.handouts.only_ind
    ##  Min.   :0.0000                             
    ##  1st Qu.:0.0000                             
    ##  Median :0.0000                             
    ##  Mean   :0.2851                             
    ##  3rd Qu.:1.0000                             
    ##  Max.   :1.0000                             
    ##  NA's   :7                                  
    ##  label.names_serial.no.show_ind label.names_unsure.foster.or.adopt_ind
    ##  Min.   :0                      Min.   :0                             
    ##  1st Qu.:0                      1st Qu.:0                             
    ##  Median :0                      Median :0                             
    ##  Mean   :0                      Mean   :0                             
    ##  3rd Qu.:0                      3rd Qu.:0                             
    ##  Max.   :0                      Max.   :0                             
    ##  NA's   :7                      NA's   :7                             
    ##  label.names_vet_ind label.names_vet.check.in.process_ind
    ##  Min.   :0.000000    Min.   :0                           
    ##  1st Qu.:0.000000    1st Qu.:0                           
    ##  Median :0.000000    Median :0                           
    ##  Mean   :0.002227    Mean   :0                           
    ##  3rd Qu.:0.000000    3rd Qu.:0                           
    ##  Max.   :1.000000    Max.   :0                           
    ##  NA's   :7           NA's   :7                           
    ##  label.names_withdrawn_ind
    ##  Min.   :0                
    ##  1st Qu.:0                
    ##  Median :0                
    ##  Mean   :0                
    ##  3rd Qu.:0                
    ##  Max.   :0                
    ##  NA's   :7

``` r
#write.csv(master_apps, "Analyses/2_Applicants/master_apps.csv", row.names = FALSE)

# Save a single object to a file
saveRDS(master_apps, "masterapps.rds")
# Restore it under a different name to verify
reloaded_data <- readRDS("masterapps.rds")
str(reloaded_data)
```

    ## 'data.frame':    456 obs. of  252 variables:
    ##  $ date_submitted                                             : Date, format: "2018-12-31" "2018-12-27" ...
    ##  $ ideal_adoption_timeline                                    : chr  "today" "one-month-or-more" "one-month-or-more" "today" ...
    ##  $ reason_for_adoption                                        : chr  "myself" "myself" "myself" "myself,my-kids,gift" ...
    ##  $ specific_animal                                            : logi  TRUE TRUE FALSE FALSE TRUE FALSE ...
    ##  $ adults_in_home                                             : num  1 2 1 1 2 2 2 1 1 1 ...
    ##  $ children_in_home                                           : int  0 0 0 3 1 2 2 1 0 0 ...
    ##  $ all_household_agree                                        : chr  "yes" "yes" "yes" "yes,a-surprise" ...
    ##  $ allergies                                                  : chr  "no-allergies" "no-allergies" "mildly-allergic" "no-allergies" ...
    ##  $ home_owner                                                 : Factor w/ 5 levels "company","family-friend",..: 5 5 5 5 5 3 3 5 3 1 ...
    ##  $ home_pet_policy                                            : Factor w/ 5 levels "no-but-pets-allowed",..: NA NA NA NA NA NA NA NA NA 4 ...
    ##  $ experience                                                 : chr  "given-away,pet-died-in-care,euthanized,grew-up-with,currently-have-pet,current-housemates-pet" "euthanized,grew-up-with,past-housemates-pet" "grew-up-with,past-housemates-pet" "grew-up-with" ...
    ##  $ budget_monthly                                             : num  125 250 100 150 250 100 100 1000 100 60 ...
    ##  $ budget_emergency                                           : num  3000 1000 2500 300 250 500 500 5000 500 500 ...
    ##  $ home_alone_avg                                             : num  NA 8 0 6 5 6 6 3 5 6 ...
    ##  $ home_alone_max                                             : num  3 NA 6 NA 5 8 8 3 8 8 ...
    ##  $ pet_kept                                                   : chr  "inside-only" "inside-only" "inside-only" "inside-only" ...
    ##  $ exercise                                                   : chr  "toy-mice,wand-toys,other-pets" "toy-mice,wand-toys" "toy-mice,wand-toys" "toy-mice,wand-toys,other-pets" ...
    ##  $ needs                                                      : chr  "nail-clip,scratching-post" "nail-clip,scratching-post" "scratching-post" "not-sure" ...
    ##  $ return_pet                                                 : chr  "none,becomes-aggressive" "none" "none,allergies-appear" "none" ...
    ##  $ how_heard                                                  : chr  "other" "website" NA NA ...
    ##  $ trello_id                                                  : chr  "5bfecbeaf9c3e187eb632753" "5830b6fd9b5eb5693b943550" "5c23b9c9afe9f63c84e03f76" "5c1fc899f249bd6e1f45d851" ...
    ##  $ City                                                       : Factor w/ 258 levels "ABINGDON","ABINGTON",..: 186 186 186 186 186 2 2 56 186 186 ...
    ##  $ State                                                      : Factor w/ 14 levels "CA","CO","CT",..: 14 14 14 14 14 14 14 14 14 14 ...
    ##  $ ZIP                                                        : chr  "19104" "19144" "19148" "19129" ...
    ##  $ animal_type.x                                              : chr  "cats" "cats" "cats" "cats" ...
    ##  $ budget_monthly_ranges                                      : Factor w/ 7 levels "<25",">5000",..: 4 5 6 4 5 6 6 7 6 6 ...
    ##  $ budget_emergency_ranges                                    : Factor w/ 7 levels "<25",">5000",..: 3 7 3 5 5 5 5 3 5 5 ...
    ##  $ reason.for.adoption_gift_ind                               : num  0 0 0 1 0 0 0 0 0 0 ...
    ##  $ reason.for.adoption_mouser_ind                             : num  0 0 0 0 1 0 0 0 0 0 ...
    ##  $ reason.for.adoption_my.kids_ind                            : num  0 0 0 1 1 0 0 1 0 0 ...
    ##  $ reason.for.adoption_myself_ind                             : num  1 1 1 1 1 1 1 0 1 1 ...
    ##  $ reason.for.adoption_other_ind                              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ reason.for.adoption_protection_ind                         : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ all.household.agree_a.surprise_ind                         : num  0 0 0 1 0 0 0 0 1 0 ...
    ##  $ all.household.agree_no_ind                                 : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ all.household.agree_yes_ind                                : num  1 1 1 1 1 1 1 1 0 1 ...
    ##  $ allergies_mildly.allergic_ind                              : num  0 0 1 0 0 0 0 0 0 0 ...
    ##  $ allergies_no.allergies_ind                                 : num  1 1 0 1 1 1 1 1 1 1 ...
    ##  $ allergies_not.sure_ind                                     : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ allergies_very.allergic_ind                                : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.owner_company_ind                                     : num  1 0 0 0 0 0 0 0 0 1 ...
    ##  $ home.owner_family.friend_ind                               : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.owner_family.member.or.friend_ind                     : num  0 0 0 0 0 1 1 0 1 0 ...
    ##  $ home.owner_landlord_ind                                    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.owner_myself_ind                                      : num  1 1 1 1 1 0 0 1 0 0 ...
    ##  $ home.pet.policy_no.but.pets.allowed_ind                    : num  0 NA NA NA NA NA NA NA NA 0 ...
    ##  $ home.pet.policy_not.applicable_ind                         : num  0 NA NA NA NA NA NA NA NA 0 ...
    ##  $ home.pet.policy_not.yet_ind                                : num  0 NA NA NA NA NA NA NA NA 0 ...
    ##  $ home.pet.policy_yes_ind                                    : num  1 NA NA NA NA NA NA NA NA 1 ...
    ##  $ home.pet.policy_yes.with.pet.policy_ind                    : num  0 NA NA NA NA NA NA NA NA 0 ...
    ##  $ experience_bred.sold_ind                                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ experience_current.housemates.pet_ind                      : num  1 0 0 0 0 0 0 0 0 0 ...
    ##  $ experience_currently.have.pet_ind                          : num  1 0 0 0 0 0 0 1 0 1 ...
    ##  $ experience_euthanized_ind                                  : num  1 1 0 0 0 0 0 0 0 0 ...
    ##  $ experience_given.away_ind                                  : num  1 0 0 0 0 0 0 0 0 0 ...
    ##  $ experience_given.to.shelter_ind                            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ experience_grew.up.with_ind                                : num  1 1 1 1 1 1 1 0 0 1 ...
    ##  $ experience_never.lived.with_ind                            : num  0 0 0 0 0 0 0 0 1 0 ...
    ##  $ experience_past.housemates.pet_ind                         : num  0 1 1 0 1 0 0 0 0 0 ...
    ##  $ experience_pet.died.in.care_ind                            : num  1 0 0 0 0 1 1 0 0 0 ...
    ##  $ experience_pet.ran.away_ind                                : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ budget.monthly.ranges_<25_ind                              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ budget.monthly.ranges_>5000_ind                            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ budget.monthly.ranges_1001.5000_ind                        : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ budget.monthly.ranges_101.200_ind                          : num  1 0 0 1 0 0 0 0 0 0 ...
    ##  $ budget.monthly.ranges_201.500_ind                          : num  0 1 0 0 1 0 0 0 0 0 ...
    ##  $ budget.monthly.ranges_26.100_ind                           : num  0 0 1 0 0 1 1 0 1 1 ...
    ##  $ budget.monthly.ranges_501.1000_ind                         : num  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ budget.emergency.ranges_<25_ind                            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ budget.emergency.ranges_>5000_ind                          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ budget.emergency.ranges_1001.5000_ind                      : num  1 0 1 0 0 0 0 1 0 0 ...
    ##  $ budget.emergency.ranges_101.200_ind                        : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ budget.emergency.ranges_201.500_ind                        : num  0 0 0 1 1 1 1 0 1 1 ...
    ##  $ budget.emergency.ranges_26.100_ind                         : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ budget.emergency.ranges_501.1000_ind                       : num  0 1 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_0_ind                                       : num  0 0 1 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_1_ind                                       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_10_ind                                      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_11_ind                                      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_12_ind                                      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_13_ind                                      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_2_ind                                       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_24_ind                                      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_3_ind                                       : num  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ home.alone.avg_4_ind                                       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_5_ind                                       : num  0 0 0 0 1 0 0 0 1 0 ...
    ##  $ home.alone.avg_6_ind                                       : num  0 0 0 1 0 1 1 0 0 1 ...
    ##  $ home.alone.avg_7_ind                                       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_8_ind                                       : num  1 1 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.avg_9_ind                                       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ home.alone.max_0_ind                                       : num  0 NA 0 NA 0 0 0 0 0 0 ...
    ##  $ home.alone.max_1_ind                                       : num  0 NA 0 NA 0 0 0 0 0 0 ...
    ##  $ home.alone.max_10_ind                                      : num  0 NA 0 NA 0 0 0 0 0 0 ...
    ##  $ home.alone.max_11_ind                                      : num  0 NA 0 NA 0 0 0 0 0 0 ...
    ##  $ home.alone.max_12_ind                                      : num  1 NA 0 NA 0 0 0 0 0 0 ...
    ##  $ home.alone.max_13_ind                                      : num  0 NA 0 NA 0 0 0 0 0 0 ...
    ##  $ home.alone.max_14_ind                                      : num  0 NA 0 NA 0 0 0 0 0 0 ...
    ##  $ home.alone.max_15_ind                                      : num  0 NA 0 NA 0 0 0 0 0 0 ...
    ##  $ home.alone.max_16_ind                                      : num  0 NA 0 NA 0 0 0 0 0 0 ...
    ##   [list output truncated]

``` r
identical(master_apps,reloaded_data)
```

    ## [1] TRUE

Data Visualizations
-------------------

### Petpoint Visualizations

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-1.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-2.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-3.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-4.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-5.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-6.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-7.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-8.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-9.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-10.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-11.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-12.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-13.png)![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-2-14.png)

### Cards Visualizations

``` r
#Distribution of lastLabel 
cards %>% filter(!is.na(label_names)) %>% 
  ggplot(mapping=aes(x=last_label,col=animal_type,fill=animal_type)) + 
     geom_bar() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Distribution of Last Label in the sequence of Label_Names") +
  geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
cards %>% select(last_label) %>% table(useNA="always") %>% sort(decreasing = T)
```

    ## .
    ##                         <NA>               ready to adopt 
    ##                         3444                         1404 
    ##                      adopted             ready for review 
    ##                         1136                          927 
    ##  reviewed with handouts only                    questions 
    ##                          541                          351 
    ##                      not utd                          vet 
    ##                          257                          242 
    ##                     red flag                    need info 
    ##                          195                          176 
    ##                     returned            adopted elsewhere 
    ##                          173                          160 
    ##                need vet info                       denied 
    ##                          150                          120 
    ##           adoption follow up             manager decision 
    ##                          113                          104 
    ##                   pet policy                     approved 
    ##                           92                           82 
    ##                       checks               need to see id 
    ##                           55                           48 
    ##                    withdrawn                  declaw only 
    ##                           41                           37 
    ##                 rescue check     approved with limitation 
    ##                           35                           31 
    ##                     dog meet                          opa 
    ##                           28                           14 
    ##      need proof of ownership   need written ll permission 
    ##                            5                            5 
    ##       unsure foster or adopt              foster to adopt 
    ##                            5                            4 
    ##             do not follow up      need roommates vet info 
    ##                            3                            3 
    ##         vet check in process                     landlord 
    ##                            3                            2 
    ##           needs app attached needs review before approval 
    ##                            1                            1 
    ##               serial no show 
    ##                            1

### Apps Visualizations

``` r
apps %>% ggplot(mapping=aes(x=budget_monthly_ranges,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Monthly budgets") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
apps %>% ggplot(mapping=aes(x=budget_emergency_ranges,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Emergency budgets") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
### Home alone avg
apps %>% ggplot(mapping=aes(x=home_alone_avg,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Home Alone Average") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
### Home alone max
apps %>% ggplot(mapping=aes(x=home_alone_max,col=animal_type,fill=animal_type)) +
   geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Home Alone Maximum") +
   geom_text(aes(label=..count..),stat='count',position=position_stack(1.1))
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-4-4.png)

### Actions Visualizations

``` r
#plot the distribution of the action time - number of days between first action and checklist_VET 
actions %>% ggplot(mapping=aes(x=checklist_ACCT,col=animal_type,fill=animal_type)) +
  geom_histogram(binwidth=2,col="red",fill="green",alpha=0.2) +
  coord_cartesian(xlim=c(0,25)) +
  facet_grid(animal_type ~ .) +
  labs(title="Histogram of time to action (checklist_ACCT)")
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
actions %>% ggplot(mapping=aes(x=checklist_CHQ,col=animal_type,fill=animal_type)) +
  geom_histogram(binwidth=2,col="red",fill="green",alpha=0.2) +
  coord_cartesian(xlim=c(0,25)) +
  facet_grid(animal_type ~ .) +
  labs(title="Histogram of time to action (checklist_CHQ)")
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
actions %>% ggplot(mapping=aes(x=checklist_LL,col=animal_type,fill=animal_type)) +
  geom_histogram(binwidth=2,col="red",fill="green",alpha=0.2) +
  coord_cartesian(xlim=c(0,25)) +
  facet_grid(animal_type ~ .) +
  labs(title="Histogram of time to action (checklist_LL)")
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-5-3.png)

``` r
actions %>% ggplot(mapping=aes(x=checklist_PP,col=animal_type,fill=animal_type)) +
  geom_histogram(binwidth=2,col="red",fill="green",alpha=0.2) +
  coord_cartesian(xlim=c(0,25)) +
  facet_grid(animal_type ~ .) +
  labs(title="Histogram of time to action (checklist_PP)")
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-5-4.png)

``` r
actions %>% ggplot(mapping=aes(x=checklist_SPCA,col=animal_type,fill=animal_type)) +
  geom_histogram(binwidth=2,col="red",fill="green",alpha=0.2) +
  coord_cartesian(xlim=c(0,25)) +
  facet_grid(animal_type ~ .) +
  labs(title="Histogram of time to action (checklist_SPCA)")
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-5-5.png)

``` r
actions %>% ggplot(mapping=aes(x=checklist_TR,col=animal_type,fill=animal_type)) +
  geom_histogram(binwidth=2,col="red",fill="green",alpha=0.2) +
  coord_cartesian(xlim=c(0,25)) +
  facet_grid(animal_type ~ .) +
  labs(title="Histogram of time to action (checklist_TR)")
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-5-6.png)

``` r
actions %>% ggplot(mapping=aes(x=checklist_VET,col=animal_type,fill=animal_type)) +
  geom_histogram(binwidth=2,col="red",fill="green",alpha=0.2) +
  coord_cartesian(xlim=c(0,25)) +
  facet_grid(animal_type ~ .) +
  labs(title="Histogram of time to action (checklist_VET)")
```

![](combined_cleanedup_data_files/figure-markdown_github/unnamed-chunk-5-7.png)
