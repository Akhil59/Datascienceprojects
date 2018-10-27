#Libraries Used

library(devtools)
library(flipTime)
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)
install_github("Displayr/flipTime")

#reading the data
uber<-read.csv("Uber Request Data.csv",stringsAsFactors = F)


#data clening and manipulation

#converting date format to one format
uber$Request.timestamp<-str_replace_all(uber$Request.timestamp,"-","/")

uber$Drop.timestamp<-str_replace_all(uber$Drop.timestamp,"-","/")

#used lubridatelibrary to convert every datetime to 'dmy HMS' format 

uber$Request.timestamp<-parse_date_time(uber$Request.timestamp,'dmy HMS',truncated = 3)

uber$Drop.timestamp<-parse_date_time(uber$Drop.timestamp,'dmy HMS',truncated = 3)

#converting into factor
uber$Status<-as.factor(uber$Status)

uber$Pickup.point<-as.factor(uber$Pickup.point)

#geting hours from request time 

uber$Hours<-format(uber$Request.timestamp,'%H')

#factoring the hours column
uber$Hours<-as.factor(uber$Hours)

#geting the hours and sec from drop time stamp
uber$Droptimehoursandmin<-format(uber$Drop.timestamp,'%H:%M')


#geting the hours and sec from request time stamp
uber$Requesttimehoursandmin<-format(uber$Request.timestamp,'%H:%M')

#geting the trip time 
uber$Triptime<-uber$Drop.timestamp-uber$Request.timestamp



#Visually identify the most pressing problems for Uber. 


#From city number of cabs showing status cancelled or no cars avalible  
a<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Status=='Cancelled'|uber$Status=='No Cars Available')),aes(x=Hours,fill='blue'))+geom_bar()+
  theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('Number of cars cancelled and no cars available at city')

#From Airport  number of cabs showing status cancelled or no cars avalible  
b<-ggplot(subset(uber,uber$Pickup.point=='Airport'&uber$Status=='Cancelled'|uber$Status=='No Cars Available'),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('Number of cars cancelled and no cars available at Airport')

#arrange the two graphs

grid.arrange(a,b)


#cars_cancelled_no_cars_avalible_in_early_mornings_in_city


status_of_cars_cancelled_no_cars_avalible_in_early_mornings<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Status=='No Cars Available'|uber$Status=='Cancelled')&(uber$Hours=='00'|uber$Hours=='01'|uber$Hours=='02'|uber$Hours=='03'|uber$Hours=='04'|uber$Hours=='05')),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('cars cancelled and not  avalible at 00 t0 05')

#cars_cancelled_no_cars_avalible_in_mornings_in_city

status_of_cars_cancelled_no_cars_avalible_in_mornings<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Status=='No Cars Available'|uber$Status=='Cancelled')&(uber$Hours=='06'|uber$Hours=='07'|uber$Hours=='08'|uber$Hours=='09'|uber$Hours=='10'|uber$Hours=='11')),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('cars cancelled and not  avalible at 06 t0 11')

#cars_cancelled_no_cars_avalible_in_after_noon_in_city

status_of_cars_cancelled_no_cars_avalible_in_after_noon<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Status=='No Cars Available'|uber$Status=='Cancelled')&(uber$Hours=='12'|uber$Hours=='13'|uber$Hours=='14'|uber$Hours=='15')),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('cars cancelled and not avalible at 12 to 15')

#cars_cancelled_no_cars_avalible_in_evening_in_city

status_of_cars_cancelled_no_cars_avalible_in_evenings<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Status=='No Cars Available'|uber$Status=='Cancelled')&(uber$Hours=='16'|uber$Hours=='17'|uber$Hours=='18'|uber$Hours=='19')),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('cars cancelled and not avalible at 16 to 19')

#cars_cancelled_no_cars_avalible_in_nights_in_city

status_of_cars_cancelled_no_cars_avalible_in_nights<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Status=='No Cars Available'|uber$Status=='Cancelled')&(uber$Hours=='20'|uber$Hours=='21'|uber$Hours=='22'|uber$Hours=='23')),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('cars cancelled and not avalible at  20 to 23')


#cars_cancelled_no_cars_avalible_in_early_mornings_in_airport

status_of_cars_cancelled_no_cars_avalible_in_early_mornings_from_airport<-ggplot(subset(uber,uber$Pickup.point=='Airport'&(uber$Status=='No Cars Available'|uber$Status=='Cancelled')&(uber$Hours=='00'|uber$Hours=='01'|uber$Hours=='02'|uber$Hours=='03'|uber$Hours=='04'|uber$Hours=='05')),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('cars cancelled and not  avalible at 00 t0 05')

#cars_cancelled_no_cars_avalible_in_mornings_in_airport

status_of_cars_cancelled_no_cars_avalible_in_mornings_from_airport<-ggplot(subset(uber,uber$Pickup.point=='Airport'&(uber$Status=='No Cars Available'|uber$Status=='Cancelled')&(uber$Hours=='06'|uber$Hours=='07'|uber$Hours=='08'|uber$Hours=='09'|uber$Hours=='10'|uber$Hours=='11')),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('cars cancelled and not  avalible at 06 t0 11')

#cars_cancelled_no_cars_avalible_in_after_noon_in_airport
status_of_cars_cancelled_no_cars_avalible_in_after_noon_from_airport<-ggplot(subset(uber,uber$Pickup.point=='Airport'&(uber$Status=='No Cars Available'|uber$Status=='Cancelled')&(uber$Hours=='12'|uber$Hours=='13'|uber$Hours=='14'|uber$Hours=='15')),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('cars cancelled and not avalible at 12 to 15')

#cars_cancelled_no_cars_avalible_in_evenings_in_airport
status_of_cars_cancelled_no_cars_avalible_in_evenings_from_airport<-ggplot(subset(uber,uber$Pickup.point=='Airport'&(uber$Status=='No Cars Available'|uber$Status=='Cancelled')&(uber$Hours=='16'|uber$Hours=='17'|uber$Hours=='18'|uber$Hours=='19')),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('cars cancelled and not avalible at 16 to 19')

#cars_cancelled_no_cars_avalible_in_nights_in_airport
status_of_cars_cancelled_no_cars_avalible_in_nights_from_airport<-ggplot(subset(uber,uber$Pickup.point=='Airport'&(uber$Status=='No Cars Available'|uber$Status=='Cancelled')&(uber$Hours=='20'|uber$Hours=='21'|uber$Hours=='22'|uber$Hours=='23')),aes(x=Hours,fill='blue'))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab('cars cancelled and not avalible at  20 to 23')


#Arrangeing the plots from 00 to 23 at the city

status_of_cars_cancelled_no_cars_avalible_from_city<-grid.arrange(status_of_cars_cancelled_no_cars_avalible_in_early_mornings,status_of_cars_cancelled_no_cars_avalible_in_mornings,status_of_cars_cancelled_no_cars_avalible_in_after_noon,status_of_cars_cancelled_no_cars_avalible_in_evenings,status_of_cars_cancelled_no_cars_avalible_in_nights,ncol=3)

#Arrangeing the plots from 00 to 23 at the airport

status_of_cars_cancelled_no_cars_avalible_from_airport<-grid.arrange(status_of_cars_cancelled_no_cars_avalible_in_early_mornings_from_airport,status_of_cars_cancelled_no_cars_avalible_in_mornings_from_airport,status_of_cars_cancelled_no_cars_avalible_in_after_noon_from_airport,status_of_cars_cancelled_no_cars_avalible_in_evenings_from_airport,status_of_cars_cancelled_no_cars_avalible_in_nights_from_airport,ncol=3)





#supply and demand 


#overall supply and demand at city 

demand_at_the_city<-ggplot(subset(uber,uber$Pickup.point=='City'),aes(x=Hours))+geom_bar(fill='green')+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the city")


supply_at_the_city<-ggplot(subset(uber,uber$Pickup.point=='Airport'&!is.na(uber$Droptimehours)),aes(x=Droptimehours))+geom_bar(fill='blue')+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the city")


grid.arrange(demand_at_the_city,supply_at_the_city)


#supply and demand at city in early mornings
demand_at_the_city_early_mornings<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Hours=='00'|uber$Hours=='01'|uber$Hours=='02'|uber$Hours=='03'|uber$Hours=='04'|uber$Hours=='05')),aes(x=Hours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the city from 00 to 05")+geom_text(size = 3, position = position_stack(vjust = 0.5))


supply_at_the_city_early_mornings<-ggplot(subset(uber,uber$Pickup.point=='Airport'&!is.na(uber$Droptimehours)&(uber$Droptimehours=='00'|uber$Droptimehours=='01'|uber$Droptimehours=='02'|uber$Droptimehours=='03'|uber$Droptimehours=='04'|uber$Droptimehours=='05')),aes(x=Droptimehours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the city from 00 to 05")


grid.arrange(demand_at_the_city_early_mornings,supply_at_the_city_early_mornings)



#supply and demand at city in mornings

demand_at_the_city_mornings<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Hours=='06'|uber$Hours=='07'|uber$Hours=='08'|uber$Hours=='09'|uber$Hours=='10'|uber$Hours=='11')),aes(x=Hours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the city from 06 to 11")


supply_at_the_city_mornings<-ggplot(subset(uber,uber$Pickup.point=='Airport'&!is.na(uber$Droptimehours)&(uber$Droptimehours=='00'|uber$Droptimehours=='06'|uber$Droptimehours=='07'|uber$Droptimehours=='08'|uber$Droptimehours=='09'|uber$Droptimehours=='10'|uber$Droptimehours=='11')),aes(x=Droptimehours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the city from 06 to 11")


grid.arrange(demand_at_the_city_mornings,supply_at_the_airport_mornings)


#supply and demand at city in after noon




demand_at_the_city_after_noon<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Hours=='12'|uber$Hours=='13'|uber$Hours=='14'|uber$Hours=='15')),aes(x=Hours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the city from 12 to 15")


supply_at_the_city_afternoon<-ggplot(subset(uber,uber$Pickup.point=='Airport'&!is.na(uber$Droptimehours)&(uber$Droptimehours=='12'|uber$Droptimehours=='13'|uber$Droptimehours=='14'|uber$Droptimehours=='15')),aes(x=Droptimehours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the city from 12 to 15")



grid.arrange(demand_at_the_city_after_noon,supply_at_the_airport_afternoon)




#supply and demand at city in evenings


demand_at_the_city_evenings<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Hours=='16'|uber$Hours=='17'|uber$Hours=='18'|uber$Hours=='19')),aes(x=Hours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the city from 16 to 19")


supply_at_the_city_evenings<-ggplot(subset(uber,uber$Pickup.point=='Airport'&!is.na(uber$Droptimehours)&(uber$Droptimehours=='16'|uber$Droptimehours=='17'|uber$Droptimehours=='18'|uber$Droptimehours=='19')),aes(x=Droptimehours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the city from 16 to 19")


grid.arrange(demand_at_the_city_evenings,supply_at_the_city_evenings)




#supply and demand at city in nights

demand_at_the_city_nights<-ggplot(subset(uber,uber$Pickup.point=='City'&(uber$Hours=='21'|uber$Hours=='22'|uber$Hours=='20'|uber$Hours=='23')),aes(x=Hours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the city from 20 to 23")

supply_at_the_city_nights<-ggplot(subset(uber,uber$Pickup.point=='Airport'&!is.na(uber$Droptimehours)&(uber$Droptimehours=='20'|uber$Droptimehours=='21'|uber$Droptimehours=='22'|uber$Droptimehours=='23')),aes(x=Droptimehours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the city from 20 to 23")

grid.arrange(demand_at_the_city_nights,supply_at_the_city_nights)




#over all supply and demand at airport

demand_at_the_airport<-ggplot(subset(uber,uber$Pickup.point=='Airport'),aes(x=Hours))+geom_bar(fill='green')+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the airport")


supply_at_the_airport<-ggplot(subset(uber,uber$Pickup.point=='City'&!is.na(uber$Droptimehours)),aes(x=Droptimehours))+geom_bar(fill='blue')+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Supply at the airport")


grid.arrange(demand_at_the_airport,supply_at_the_airport)




#supply and demand at air port in early mornings 


demand_at_the_airport_early_mornings<-ggplot(subset(uber,uber$Pickup.point=='Airport'&(uber$Hours=='00'|uber$Hours=='01'|uber$Hours=='02'|uber$Hours=='03'|uber$Hours=='04'|uber$Hours=='05')),aes(x=Hours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the Airport from 00 to 05")


supply_at_the_airport_early_mornings<-ggplot(subset(uber,uber$Pickup.point=='City'&!is.na(uber$Droptimehours)&(uber$Droptimehours=='00'|uber$Droptimehours=='01'|uber$Droptimehours=='02'|uber$Droptimehours=='03'|uber$Droptimehours=='04'|uber$Droptimehours=='05')),aes(x=Droptimehours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the airport from 00 to 05")


grid.arrange(demand_at_the_airport_early_mornings,supply_at_the_airport_early_mornings)



#supply and demand at air port in  mornings 


demand_at_the_airport_mornings<-ggplot(subset(uber,uber$Pickup.point=='Airport'&(uber$Hours=='06'|uber$Hours=='07'|uber$Hours=='08'|uber$Hours=='09'|uber$Hours=='10'|uber$Hours=='11')),aes(x=Hours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the Airport from 06 to 11")


supply_at_the_airport_mornings<-ggplot(subset(uber,uber$Pickup.point=='City'&!is.na(uber$Droptimehours)&(uber$Droptimehours=='00'|uber$Droptimehours=='06'|uber$Droptimehours=='07'|uber$Droptimehours=='08'|uber$Droptimehours=='09'|uber$Droptimehours=='10'|uber$Droptimehours=='11')),aes(x=Droptimehours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the airport from 06 to 11")


grid.arrange(demand_at_the_airport_mornings,supply_at_the_airport_mornings)



#supply and demand at air port in  after noon


demand_at_the_airport_after_noon<-ggplot(subset(uber,uber$Pickup.point=='Airport'&(uber$Hours=='12'|uber$Hours=='13'|uber$Hours=='14'|uber$Hours=='15')),aes(x=Hours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the Airport from 12 to 15")

supply_at_the_airport_afternoon<-ggplot(subset(uber,uber$Pickup.point=='City'&!is.na(uber$Droptimehours)&(uber$Droptimehours=='12'|uber$Droptimehours=='13'|uber$Droptimehours=='14'|uber$Droptimehours=='15')),aes(x=Droptimehours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the airport from 12 to 15")


grid.arrange(demand_at_the_airport_after_noon,supply_at_the_airport_afternoon)



#supply and demand at air port in  evenings


demand_at_the_airport_evenings<-ggplot(subset(uber,uber$Pickup.point=='Airport'&(uber$Hours=='16'|uber$Hours=='17'|uber$Hours=='18'|uber$Hours=='19')),aes(x=Hours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the Airport from 16 to 19")

supply_at_the_airport_evenings<-ggplot(subset(uber,uber$Pickup.point=='City'&!is.na(uber$Droptimehours)&(uber$Droptimehours=='16'|uber$Droptimehours=='17'|uber$Droptimehours=='18'|uber$Droptimehours=='19')),aes(x=Droptimehours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the airport from 16 to 19")



grid.arrange(demand_at_the_airport_evenings,supply_at_the_airport_evenings)




#supply and demand at air port in  nights

demand_at_the_airport_nights<-ggplot(subset(uber,uber$Pickup.point=='Airport'&(uber$Hours=='21'|uber$Hours=='22'|uber$Hours=='18'|uber$Hours=='23')),aes(x=Hours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("Demand at the Airport from 20 to 23")


supply_at_the_airport_nights<-ggplot(subset(uber,uber$Pickup.point=='City'&!is.na(uber$Droptimehours)&(uber$Droptimehours=='20'|uber$Droptimehours=='21'|uber$Droptimehours=='22'|uber$Droptimehours=='23')),aes(x=Droptimehours))+geom_bar()+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)+xlab("suppy at the airport from 20 to 23")



grid.arrange(demand_at_the_airport_nights,supply_at_the_airport_nights)


#plot to see the trend between cars status and request time 

#Status of cars  vs Request time at city

cars_status_and_request_time_at_city<-ggplot(subset(uber,uber$Pickup.point=='City'),aes(x=Hours,fill=Status))+geom_bar(position = 'dodge')+ylim(0,300)+xlab("From City")


#Status of cars  vs Request time at airport


cars_status_and_request_time_at_airport<-ggplot(subset(uber,uber$Pickup.point=='Airport'),aes(x=Hours,fill=Status))+geom_bar(position = 'dodge')+ylim(0,500)+xlab("From Airport")



grid.arrange(cars_status_and_request_time_at_city,cars_status_and_request_time_at_airport)



#For overall supply and demand at city and airport

grid.arrange(demand_at_the_city,supply_at_the_city,demand_at_the_airport,supply_at_the_airport,ncol=2)




#writeing the csv file

write.csv(uber,"uber.csv",row.names = FALSE)








