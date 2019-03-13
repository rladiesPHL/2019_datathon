#check directory
getwd() 

library(readr)
mydata <- read.csv("Data/dog_apps.csv")

hist(mydata[,6])


#aggregate(data.frame(count = mydata$allergies), list(value = mydata$allergies), length)

#aggregate(data.frame(count = mydata$home_owner), list(value = mydata$home_owner), length)


#Cleaned data created by Amy
dataCleaned <- read.csv("Analyses/2_Applicants/amygood/output/apps_clean.csv")

#frequency of different home_owner types
dplyr::count(dataCleaned, home_owner)
#frequency of different types of allergies
dplyr::count(dataCleaned, allergies)


  
