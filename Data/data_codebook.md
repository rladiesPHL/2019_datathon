# Codebook

## Cards data (dog_cards.csv and cat_cards.csv)

- *id:* a unique ID
- *dateLastActivity:* the last time an activity was recorded on the card
- *dueComplete:* indicates whether the application is marked as complete or they lost contact with the applicant
- *due:* indicates the date the application is marked as complete or they lost contact with the applicant
- *animal_type:* cat or dog
- *label_names:* the label names indicate the last recorded status of the application

### Label names:
There are 37 unique label names which can be categorized as follows:

* labels relating to application approval status:
    + *approved*
    + *ready to adopt*
    + *ready for review* - means ready to adopt but applicant needs to be given additional info to set up for success
    + *reviewed with handouts only* - ready to adopt, should be given a handout
    + *approved with limitation*
    + *needs review before approval*
    + *denied*
    + *withdrawn*
    + *do not follow up*
    + *manager decision*

* labels relating to issues with the application:
    + *vet*
    + *vet check in process*
    + *need vet info*
    + *need roommates vet info*
    + *not utd* - applicant’s current animals are not up to date on vaccines
    + *questions* - follow up questions about pets they had before without explanation
    + *not s n* - KF thinks that this means applicant’s current animals are not spayed or neutered
    + *dog meet* - dog meet needs to occur before proceeding
    + *need to see id* - applicant didn’t provide their ID info on application
    + *need info* - need additional info
    + *checks* - checklist needs to be added and items gone through
    + *needs app attached*
    + *rescue check* - check applicant’s name with other rescues
    + *pet policy* - pet policy needs to be confirmed
    + *need written ll permission* - need written landlord permission
    + *opa* - owner of property where applicant lives must be confirmed
    + *need proof of ownership* - applicant said they own their property but this info cannot be found publicly
    + *landlord* - must check with landlord

* label relating to limitations of the application:
    + *declaw only* - will only adopt a declawed cat
    + *red flag* - some issue with applicant, might not be enough to disqualify but could require follow up
    + *unsure foster or adopt* - applicant might be ambivalent about fostering or adopting
    + *foster to adopt* - applicant will foster an animal until the animal can be fixed, when it is legal for PAWS to adopt out animal
    + *serial no show*

* labels relating to adoption status:
    + *adopted*
    + *adopted elsewhere*
    + *adoption follow up* - to be followed up with post-adoption
    + *returned*

## Actions data (dog_actions.csv and cat_actions.csv)

- *type:* type of update done to the card. "createCard" (when the card was created) / "updateCheckItemStateOnCard" (when a checklist item was modified). PAWS use a fairly standardized checklist for going through the applications, so the timeline in which that checklist gets completed can be informative
- *data.checkItem.state:* if a checklist item was modified, whether it was modified to "complete" or "incomplete"
- *date:* the date when the action was taken
- *data.card.id:*
- *animal_type_:* cat or dog
- *checklist_ACCT:* Check with ACCT (Animal Care and Control Team)
- *checklist_CHQ:* Check with clinic HQ (the PAWS clinic uses a different software for logging clients than the adoptions process)
- *checklist_LL:* Check with property owner
- *checklist_PP:* Check with PetPoint
- *checklist_SPCA:* Check with SPCA (Society for the Prevention of Cruelty to Animals)
- *checklist_TR:* Check with Trello
- *checklist_VET:* Check with vet

Note that *checklist_seq_num* has been removed.

## Apps data (dog_apps.csv and cat_apps.csv)

- *date_submitted:* date when the application was submitted
- *ideal_adoption_timeline:* when the applicant would like to adopt
- *reason_for_adoption:* applicant's stated reason for applying to adopt an animal
- *specific_animal:* applicant is interested in a specific animal
- *adults_in_home:* number of adults, other than applicant, living in applicant's home
- *children_in_home:* number of children living in applicant's home
- *all_household_agree:* do all members of the household agree on adopting?
- *allergies:* is anyone in household allergic to the type of animal to be adopted?
- *home_owner:* who owns applicant's home?
- *home_pet_policy:* if renting, was landlord asked about bringing a new pet home?
- *experience:* applicant's experience with pets
- *budget_monthly:* applicant's monthly budget for pet supplies and medical care
- *budget_emergency:* applicant's emergency budget for pet supplies and medical care
- *home_alone_avg:* time the pet will be alone in the home on an average day
- *home_alone_max:* time the pet will be alone in the home on an average day
- *pet_kept:* where will the pet be kept
- *exercise:* applicant's desired exercise & playtime with pet
- *needs:* plan for grooming needs (dogs) / scratching needs (cats)
- *return_pet:* circumstances that would prompt applicant to return pet to PAWS
- *how_heard:* if applying for specific animal, how did the applicant hear about the animal
- *outcome_trello_id:* id for matching with cards and actions datasets
- *STATEFP:* State FIPS Code (https://www.census.gov/geo/reference/codes/cou.html)
- *COUNTYFP:* County FIPS Code (https://www.census.gov/geo/reference/codes/cou.html)
- *TRACTCE:* Census Tract Code
- *GEOID:* Geographic Identifier
- *NAME:* Regional name
- *NAMELSAD:* Regional name and legal/statistical area description
- *MTFCC:* MAF/TIGER feature class code
- *FUNCSTAT:* Current functional status
- *ALAND:* Current land area
- *AWATER:* Current water area 
- *INTPTLAT:* Current latitude of the internal point 
- *INTPTLON:* Current longitude of the internal point 
- *City*
- *State*
- *ZIP*

### ideal_adoption_timeline:

- *few-months:* within a few months
- *few-weeks:* within a few weeks
- *today:* day of application
- *one-month-or-more* (on cat applications; assume it means same as "few-months")
- *next-few-weeks:* (on cat applications; assume it means same as "few-weeks")

### reason_for_adoption:

- *myself:* applicant wants to adopt a pet for themselves
- *my-kids:* applicant wants to adopt a pet for their children
- *protection:* applicant wants to adopt a dog for protection
- *gift:* applicant wants to adopt a pet to give as a gift
- *mouser:* applicant wants to adopt a cat as mouse intervention
- *other:* applicant wants to adopt a pet for other reasons

### home_pet_policy

- *not-yet:* applicant has not asked landlord
- *no-but-pets-allowed:* applicant has not asked landlord but believes pets are allowed
- *yes-with-pet-policy:* applicant has asked landlord and understands the pet policy
- *yes:* applicant has asked landlord
- *havent-asked:* applicant hasn't asked landlord
- *no-yet:* applicant has not yet asked landlord
- *n-a:* not applicable

### experience
Note: dog and cat apps have different labels but mean the same thing (see duplicates below)

- *given-to-shelter:* Given a pet to a shelter
- *given-a-pet-to-a-shelter*
- *bred-sold:* Bred/sold a pet
- *bred-sold-a-pet*
- *pet-ran-away:* Had a pet run away
- *given-to-another:* Given a pet to another person
- *given-pet-to-another*
- *had-pet-die:* Had a pet die in applicant's care
- *pet-died-in-care*
- *euthanized:* Had to euthanize a pet
- *euthanized-a-pet*
- *grew-up-with:* Grew up with a pet
- *grew-up-with-pet*
- *currently-have-pet:* Currently has a pet
- *lived-with-housemate-pet:* Lived with housemate's pet
- *lived-wit-previous-housemates-pets*
- *never-lived-or-owned:* Never lived with or owned a pet before
- *never-lived-with-or-owned-a-pet-before*

### pet_kept

- *inside-only*
- *outside-only*
- *inside-outside:* Inside/outside (allowed to roam)
- *supervised-in-my-yard:*
- *leash-harness:* leash/harness training
- *inside-with-yard-access:* inside with access to fenced yard
- *unsupervised-access-to-my-yard-doggie-door-etc:* unsupervised access to yard (e.g. doggie door etc.)
- *crate*
- *other*

### exercise

- *walks-on-leash*
- *walks-off-leash*
- *jogging-together*
- *playing-in-my-yard*
- *dog-parks*
- *another-pet:* playmates with another pet
- *other-cats*
- *not-much:* not much: I’d like a calm pet
- *toy-mice:* toy mice/balls
- *wand-toys:* wand toys

### needs

- *no-grooming:* want a dog that doesn't need grooming
- *professional-groomer*
- *groom-myself*
- *declaw:* declaw surgery
- *nail-clip:* nail clipping
- *scratching-post*
- *not-sure:* not sure yet
- *other*

### return_pet

- *new-baby:* new baby
- *not-enough-time:* not enough time for pet
- *pet-sheds:* pet sheds
- *allergies-appear:* someone in household becomes allergic
- *destructive:* destructive (chews objects)
- *scratches-furniture:* scratches furniture
- *vet-becomes-expensive:* vet costs too expensive
- *not-allowed-new-living-space:* not allowed in new living space
- *becomes-aggressive:* becomes aggressive
- *not-housebroken:* not housebroken
- *litter-box-issues:* litter box issues
- *moving-too-far:* moving too far
- *none*
- *jumps-on-counters*
- *too-playful*

## Petpoint data (petpoint.csv)

- *animal_type* 
- *species*
- *primary_breed*
- *secondary_breed*
- *markings*
- *gender*
- *altered:* whether animal was spayed/neutered
- *dob:* date of birth
- *age_intake:* age in months at intake
- *age_group*
- *intake_asilomar:* asilomar status at intake (see https://www.aspca.org/about-us/aspca-policy-and-position-statements/asilomar-accords-definitions for details)
- *intake_condition*
- *intake_date*
- *intake_type:* main category for how animal arrived at PAWS
- *intake_subtype:* additional details for how animal arrived at PAWS
- *intake_reason:* reason why animal was surrendered or arrived at PAWS
- *intake_sitename:* PAWS location where intake occurred
- *agency_name:* agency that transferred animal to PAWS,
- *outcome_asilomar:* asilomar status at outcome
- *release_date:* date of release
- *outcome_date:* date of outcome
- *outcome_type:* main category for how animal left PAWS
- *outcome_subtype:* additional details for how animal left PAWS
- *outcome_sitename:* location from where animal left PAWS
- *outcome_trello_id:* id for matching with cards and actions datasets
- *STATEFP:* State FIPS Code (https://www.census.gov/geo/reference/codes/cou.html)
- *COUNTYFP:* County FIPS Code (https://www.census.gov/geo/reference/codes/cou.html)
- *TRACTCE:* Census Tract Code
- *GEOID:* Geographic Identifier
- *NAME:* Regional name
- *NAMELSAD:* Regional name and legal/statistical area description
- *MTFCC:* MAF/TIGER feature class code
- *FUNCSTAT:* Current functional status
- *ALAND:* Current land area
- *AWATER:* Current water area 
- *INTPTLAT:* Current latitude of the internal point 
- *INTPTLON:* Current longitude of the internal point 
- *outcome_city:* City where adopter is from
- *outcome_state:* State where adopter is from
- *outcome_ZIP:* ZIP where adopter is from

### intake_asilomar

- *Unassigned:* status not assigned
- *Healthy:* all dogs and cats eight weeks of age or older that [sic], at or subsequent to the time the animal is taken into possession, have manifested no sign of a behavioral or temperamental characteristic that could pose a health or safety risk or otherwise make the animal unsuitable for placement as a pet, and have manifested no sign of disease, injury, a congenital or hereditary condition that adversely affects the health of the animal or that is likely to adversely affect the animal’s health in the future.
- *Treatable-Rehabilitatable:* all dogs and cats who are not “healthy,” but who are likely to become “healthy” if given medical, foster, behavioral, or other care equivalent to the care typically provided to pets by reasonable and caring pet guardians in the community.
- *Treatable-Manageable:* all dogs and cats who are not “healthy,” and who are not likely to become “healthy,” regardless of the care provided; but who would likely maintain a satisfactory quality of life, if given medical, foster, behavioral, or other care, including long-term care, equivalent to the care typically provided to pets by reasonable and caring guardians in the community; provided, however, that the term “manageable” does not include any dog or cat who is determined to pose a significant risk to human health or safety or to the health or safety of other animals.
- *Unhealthy/Untreatable:*  all dogs and cats who, at or subsequent to the time they are taken into possession, 1. have a behavioral or temperamental characteristic that poses a health or safety risk or otherwise makes the animal unsuitable for placement as a pet, and are not likely to become “healthy” or “treatable” even if provided the care typically provided to pets by reasonable and caring pet guardians in the community; or 2. are suffering from a disease, injury, or congenital or hereditary condition that adversely affects the animal’s health or is likely to adversely affect the animal’s health in the future, and are not likely to become “healthy” or “treatable” even if provided the care typically provided to pets by reasonable and caring pet guardians in the community; or 3. are under the age of eight weeks and are not likely to become “healthy” or “treatable,” even if provided the care typically provided to pets by reasonable an caring pet guardians in the community.

### intake_type

- *Return:* animal returned to PAWS from an adopter
- *Transfer In:* animal transferred to PAWS from a partner shelter/rescue
- *Stray:* animal found as a stray
- *Owner/Guardian Surrender:* animal surrendered to PAWS by owner
- *Wildlife In:* wildlife animal brought to PAWS
- *Clinic:* meaning unclear

### intake_sitename

- *Grays Ferry Avenue:* PAWS Grays Ferry Shelter and Clinic
- *PAC:* PAWS Old City Adoptions Center
- *Grant Avenue:* PAWS Grant Avenue Shelter and Clinic
- *PAWS Foster Program*
- *PAWS Offsite Adoptions* 
- *2007 Recordkeeping*
- *The PACCA Legacy*


