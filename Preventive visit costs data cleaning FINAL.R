#Data cleaning and descriptive statistics of 2022 Medicare preventive medicine visit pricing for new patients 
#Data retrieved from CMS.gov
#Pricing data: https://data.cms.gov/provider-data/dataset/0330-b6e0
#Zip code/state data: https://www.cms.gov/medicare/medicare-fee-for-service-payment/feeschedulegeninfo (2022 End of Year Zip Code File)
library (readxl)
library(dplyr)

costs <- read_xlsx("C:/Users/kodyk/Downloads/Preventive_Medicine.xlsx")
View(costs)

#converting zip code from numeric to character
class(costs$zip_code)
costs$zipchr <- as.character(costs$zip_code)

#adding leading zeros to correct zip codes
newzip <- sapply(costs$zipchr, function(x){
  if(nchar(x)==4){
    paste0(0,x)
  } else if(nchar(x)==3){
    paste0(0,0, x)
  }else{x}
  })
print(newzip)
costs$correctzip <- newzip

#matching zip codes to states
cmszips <- read_xlsx("C:/Users/kodyk/Downloads/ZPLC_DEC2022_FINAL/ZIP5_DEC2022_FINAL.xlsx")
costs_merge<- merge(x= costs, y=cmszips,
                    by.x=c("correctzip"),
                    by.y=c("ZIP CODE"))

#removing US territories that are not states
costs_merge_states <- costs_merge %>%
  filter(STATE != "GU", STATE != "FM", STATE != "MP", STATE != "MH", STATE != "PR", STATE != "PW", STATE != "VI", STATE != "AS")

#correcting non-standard state abbreviations 
costs_merge_states <- costs_merge_states %>%
  mutate(STATE = recode(STATE, "EM" = "MO", "WM" = "MO", "EK" = "KS", "WK" = "KS"))
                    
#checking for missing values in columns of interest 
sum(is.na(costs_merge_states$STATE))
sum(is.na(costs_merge_states$min_medicare_pricing_for_new_patient))
sum(is.na(costs_merge_states$max_medicare_pricing_for_new_patient))
sum(is.na(costs_merge_states$mode_medicare_pricing_for_new_patient))

#calculating min,max, average prices for new patients by state
finaldf <- costs_merge_states %>%
  group_by(STATE) %>% 
  summarize(newmin = min(min_medicare_pricing_for_new_patient),
            newmax = max(max_medicare_pricing_for_new_patient),
            newavg = mean(mode_medicare_pricing_for_new_patient))

#exporting data
write.csv(finaldf, file = "C:/Users/kodyk/Downloads/df3.csv")




