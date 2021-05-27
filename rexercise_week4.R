library(readr)        
library(dplyr)        
library(ggplot2)      
library(lubridate)

wildschwein<-read.delim("wildschwein_BE_2056.csv",header = T, sep = ",")
str(wildschwein)
wildschwein$E<-as.integer(wildschwein$E)
wildschwein$N<-as.integer(wildschwein$N)
wildschwein$DatetimeUTC<-as_datetime(wildschwein$DatetimeUTC)
wildschwein$TierName<-as.factor(wildschwein$TierName)

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
wildschwein_filter<-wildschwein%>%
  filter(DatetimeUTC>"2015-04-01" & DatetimeUTC<="2015-04-15")

# Task 3
wildschwein_filter<-wildschwein_filter%>%
  mutate(roundedDatetime=round_date(DatetimeUTC, "15 mins"))


# Task 4 
wildschwein_filter_sabi<-wildschwein_filter%>%
  subset(TierName=="Sabi")
wildschwein_filter_sabi

wildschwein_filter_rosa<-wildschwein_filter%>%
  subset(TierName=="Rosa")

wildschwein_filter_ruth<-wildschwein_filter%>%
  subset(TierName=="Ruth")


library(plyr)
wildschwein_filter_j<-join_all(list(wildschwein_filter_sabi,wildschwein_filter_rosa,wildschwein_filter_ruth), by='roundedDatetime', type='left',match="all")
wildschwein_filter_j
?join_all

wildschwein_filter_j<-full_join(wildschwein_filter_sabi,wildschwein_filter_rosa,by="roundedDatetime",suffix=c(".Sabi",".Rosa"))
wildschwein_filter_j<-full_join(wildschwein_filter_j,wildschwein_filter_ruth,by="roundedDatetime")

wildschwein_filter_j <- wildschwein_filter_j %>%
  mutate(
    dist_Sabi_Rosa = sqrt((E.Sabi-E.Rosa)^2+(N.Sabi-N.Rosa)^2),   # distance Sabi to Rosa
    dist_Sabi_Ruth = sqrt((E.Sabi-E)^2+(N.Sabi-N)^2),
    dist_Rosa_Ruth = sqrt((E.Rosa-E)^2+(N.Rosa-N)^2),
    close_Sabi_Rosa= dist_Sabi_Rosa < 100,
    close_Sabi_Ruth= dist_Sabi_Ruth < 100,
    close_Rosa_Ruth= dist_Rosa_Ruth < 100
  )

