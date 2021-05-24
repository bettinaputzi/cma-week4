library(readr)        
library(dplyr)        
library(ggplot2)      
library(lubridate)

wildschwein<-read.delim("wildschwein_BE_2056.csv",header = T, sep = ",")
str(wildschwein)
wildschwein$E<-as.integer(wildschwein$E)
wildschwein$N<-as.integer(wildschwein$N)

testfun <- function(){}
testfun()
class(testfun)

testfun <- function(){print("this function does nothing")}
testfun()

testfun <- function(sometext){print(sometext)}
testfun(sometext = "this function does slightly more, but still not much")

my_age <- function(birthday, units){
  difftime(Sys.time(),birthday, units = units) #systime to provide todays date
}
my_age(birthday = "1992-02-16", units="days")
#  if we declare our variables in the order that we initially listed them, we do not need to specify the parameters (no need of birthday = and units =).
my_age("1992-02-16", "days")

my_age <- function(birthday, units="days"){
  difftime(Sys.time(),birthday, units = units)
}
my_age("1992-02-16")
my_age("1992-02-16", units="hours") #overwrite with hours


## Task 1
e_dist<- function(E,N){
  sqrt((E-lead(E,1))^2+(N-lead(N,1))^2)
}

wildschwein$E
e_dist_wildschwein<-e_dist(wildschwein$E,wildschwein$N)

#Task 2

