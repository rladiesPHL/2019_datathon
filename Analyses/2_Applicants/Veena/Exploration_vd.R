#check directory
getwd() 

library(readr)
mydata <- read.csv("Data/dog_apps.csv")

hist(mydata[,6])

#frequency of different types of allergies
aggregate(data.frame(count = mydata$allergies), list(value = mydata[,9]), length)
  #using dplyr
  dplyr::count(mydata, allergies)
  
#frequency of different home_owner types
aggregate(data.frame(count = mydata$home_owner), list(value = mydata$home_owner), length)
  #using dplyr
  dplyr::count(mydata, home_owner)




  
