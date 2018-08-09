#ProblemStatement
#Spark Funds wants to make investments in a few companies
#Spark Funds has two minor constraints for investments:

#It wants to invest between 5 to 15 million USD per round of investment

#It wants to invest only in English-speaking countries because of the ease of communication with the companies it would invest in

#Business objective: The objective is to identify the best sectors, countries, and a suitable investment type for making investments.
#The overall strategy is to invest where others are investing, implying that the best sectors and countries are the ones where most investments are happening.

#Approch for data analysis
#Investment type analysis: Understanding investments in the venture, seed/angel, private equity categories, etc. so Spark Funds can decide which type is best suited for its strategy.
#Country analysis: Understanding which countries have had the most investments in the past. These will be Spark Funds' favourites as well.
#Sector analysis: Understanding the distribution of investments across the eight main sectors.


#library used

library(dplyr)
library(tidyr)

#loading the requried data 
companies<-read.delim("companies.txt",sep = "\t",header = TRUE,stringsAsFactors = F)  
rounds2<-read.csv("rounds2.csv",header = TRUE,stringsAsFactors = FALSE)
mapping<-read.csv("mapping.csv",header = T,stringsAsFactors = F)

#Understanding the data

str(companies)
class(companies)
View(companies)

str(rounds2)
class(rounds2)
View(rounds2)

#Geting the number of unique companies
unique_companies<-distinct(companies,permalink)

count(unique_companies)

unique_companies_round2<-distinct(rounds3,company_permalink)

count(unique_companies_round2)

#converting the names of companies to lowercase to merge the two files  

names(rounds2)[1]<-"permalink"

companies$permalink<-tolower(companies$permalink)

rounds2$permalink<-tolower(rounds2$permalink)



#checking wheather the companies present in round2 file are present in companies file 
setdiff( unique_companies_round2$company_permalink,unique_companies$permalink)



#mergeing the companies and round2 files 
master_frame<-merge(companies,rounds2,by = "permalink")

##--Investment type analasis-----------


#Createing the dataframes from master_frame for each type of funding type and calcuating the mean

venture_type<-filter(master_frame,funding_round_type=="venture")

summarise(venture_type,mean(raised_amount_usd,na.rm = T))

seed_type<-filter(master_frame,funding_round_type=="seed")

summarise(seed_type,mean(raised_amount_usd,na.rm = T))

angel_type<-filter(master_frame,funding_round_type=="angel")

summarise(angel_type,mean(raised_amount_usd,na.rm = T))

private_equity_type<-filter(master_frame,funding_round_type=="private_equity")

summarise(private_equity_type,mean(raised_amount_usd,na.rm = T))

#From the above results i got venture type is best for the  spark fund investments 


#--country analaysis 

#geting the top 9 countries which raised more funding 

names(venture_type)
venture_type_countries<-select(venture_type,country_code,raised_amount_usd)

venture_type_top_9<-group_by(venture_type_countries,country_code)

venture_type_top_9<-summarise(venture_type_top_9,sum(raised_amount_usd,na.rm = T))

names(venture_type_top_9)<-c("country_code","raisedamount")

top_9<-arrange(venture_type_top_9,desc(raisedamount))

top9<-head(top_9,n=9)

#Sector Analaysis

sector_data<-subset(venture_type,(venture_type$country_code =="USA" | venture_type$country_code =="GBR" | venture_type$country_code =="IND"))

View(sector_data)

#understanding the mapping data

str(mapping)

names(mapping)

View(mapping)

#converting wide to long format
mapping_data <- gather(mapping,main_category,value,Automotive...Sports:Social..Finance..Analytics..Advertising)

mapping_data <- mapping_data[!(mapping_data$value == 0),]

mapping_df <- mapping_data[, -3]

mapping_df<-mapping_df[!(mapping_df$main_category =="Blanks"),]

View(mapping_df) 

sectors<- separate(sector_data,category_list,into=c("primary_sector", "other_sectors"), sep = "\\|")

mapping_df$category_list<-as.factor(str_to_lower(mapping_df$category_list))

sectors$primary_sector<-as.factor(str_to_lower(sectors$primary_sector))

names(mapping_df)<-c("primary_sector","main_category")

#merging data to get primary_sector and main_sector
master_frame_sectors<-merge(sectors,mapping_df,by ="primary_sector",all.x = T)

View(master_frame_sectors)


# Creating three separate data frames D1, D2 and D3 for each of the three
# countries containing the observations of funding type venture falling
# within the 5-15 million USD range
D1_df<-subset(master_frame_sectors,(master_frame_sectors$country_code =="USA" & master_frame_sectors$raised_amount_usd >= 5000000 & master_frame_sectors$raised_amount_usd<=15000000))

View(D1_df)

D2_df<-subset(master_frame_sectors,(master_frame_sectors$country_code =="GBR" & master_frame_sectors$raised_amount_usd >= 5000000 & master_frame_sectors$raised_amount_usd<=15000000))

View(D2_df)
D3_df<-subset(master_frame_sectors,(master_frame_sectors$country_code =="IND" & master_frame_sectors$raised_amount_usd >= 5000000 & master_frame_sectors$raised_amount_usd<=15000000))

View(D3_df)

#getting count of investments for each main_category for D1,D2,D3 dataframes

tot_investment_count_D1<-aggregate(D1_df$main_category,by=list(D1_df$main_category),FUN = length)

names(tot_investment_count_D1)<-c("main_category","total_investment_count")

tot_investment_count_D1

tot_investment_count_D2<-aggregate(D2_df$main_category,by=list(D2_df$main_category),FUN = length)

names(tot_investment_count_D2)<-c("main_category","total_investment_count")

tot_investment_count_D2

tot_investment_count_D3<-aggregate(D3_df$main_category,by=list(D3_df$main_category),FUN = length)

names(tot_investment_count_D3)<-c("main_category","total_investment_count")

tot_investment_count_D3

#merging the count related dataframes with D1,D2,D3
D1_data<-merge(D1_df,tot_investment_count_D1,by ="main_category",all = T)

D2_data<-merge(D2_df,tot_investment_count_D2,by ="main_category",all = T)

D3_data<-merge(D3_df,tot_investment_count_D3,by ="main_category",all = T)

#getting total investment for each main_category for D1_df,D2_df,D3_df data frames

tot_investment_D1<-aggregate(D1_data$raised_amount_usd,by=list(D1_data$main_category),FUN = sum)

names(tot_investment_D1)<-c("main_category","total_investment")

tot_investment_D1

tot_investment_D2<-aggregate(D2_data$raised_amount_usd,by=list(D2_data$main_category),FUN = sum)

names(tot_investment_D2)<-c("main_category","total_investment")

tot_investment_D2

tot_investment_D3<-aggregate(D3_data$raised_amount_usd,by=list(D3_data$main_category),FUN = sum)

names(tot_investment_D3)<-c("main_category","total_investment")

tot_investment_D3

#merging the total investment related dataframes with D1_df,D2_df,D3_df

D1<-merge(D1_data,tot_investment_D1,by ="main_category",all = T)

D2<-merge(D2_data,tot_investment_D2,by ="main_category",all = T)

D3<-merge(D3_data,tot_investment_D3,by ="main_category",all = T)

#total no of investments in usa,gbr,ind
nrow(D1)

nrow(D2)

nrow(D3)

#total investment for each country
sum(D1$raised_amount_usd)

sum(D2$raised_amount_usd)

sum(D3$raised_amount_usd)

#top sector,second and third sector for three countries
arrange(tot_investment_count_D1,desc(tot_investment_count_D1$total_investment))

arrange(tot_investment_count_D2,desc(tot_investment_count_D2$total_investment))

arrange(tot_investment_count_D3,desc(tot_investment_count_D3$total_investment))

#for top and second sectors which company received the highest investment
#for D1
usa_top_sector<-subset(D1,(D1$main_category =="Others"))

usa_top_company<-aggregate(usa_top_sector$raised_amount_usd,by=list(usa_top_sector$company_permalink),FUN = sum,na.rm=TRUE)

head(arrange(usa_top_company,desc(usa_top_company$x)))

usa_top_sector[(usa_top_sector$company_permalink =="/organization/virtustream"),]

#forD2

gbr_top_sector<-subset(D2,(D2$main_category =="Others"))

gbr_top_company<-aggregate(gbr_top_sector$raised_amount_usd,by=list(gbr_top_sector$company_permalink),FUN = sum,na.rm=TRUE)

head(arrange(gbr_top_company,desc(gbr_top_company$x)))

gbr_top_sector[(gbr_top_sector$company_permalink =="/organization/electric-cloud"),]

#forD3

ind_top_sector<-subset(D3,(D3$main_category =="Others"))

ind_top_company<-aggregate(ind_top_sector$raised_amount_usd,by=list(ind_top_sector$company_permalink),FUN = sum,na.rm=TRUE)

head(arrange(ind_top_company,desc(ind_top_company$x)))

ind_top_sector[(ind_top_sector$company_permalink =="/organization/firstcry-com"),]

#for D1 second sector

usa_second_sector<-subset(D1,(D1$main_category =="Cleantech...Semiconductors"))

usa_second_company<-aggregate(usa_second_sector$raised_amount_usd,by=list(usa_second_sector$company_permalink),FUN = sum,na.rm=TRUE)

head(arrange(usa_second_company,desc(usa_second_company$x)))

usa_second_sector[(usa_second_sector$company_permalink =="/organization/biodesix"),]

#for D2 second sector
gbr_second_sector<-subset(D2,(D2$main_category =="Cleantech...Semiconductors"))

gbr_second_company<-aggregate(gbr_second_sector$raised_amount_usd,by=list(gbr_second_sector$company_permalink),FUN = sum,na.rm=TRUE)

head(arrange(gbr_second_company,desc(gbr_second_company$x)))

gbr_second_sector[(gbr_second_sector$company_permalink =="/organization/eusa-pharma"),]

#for D3 second sector
ind_second_sector<-subset(D3,(D3$main_category =="News..Search.and.Messaging"))

ind_second_company<-aggregate(ind_second_sector$raised_amount_usd,by=list(ind_second_sector$company_permalink),FUN = sum,na.rm=TRUE)

head(arrange(ind_second_company,desc(ind_second_company$x)))

ind_second_sector[(ind_second_sector$company_permalink =="/organization/gupshup-technology-india-pvt-ltd"),]

# For tableau plots

write.csv(master_frame,"cs_master_frame.csv")







