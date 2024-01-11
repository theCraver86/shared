### - Load Library

library(adobeanalyticsr)
library(dplyr)
library(lubridate)

library(cleaner) #Unistall?
library(gt)
library(writexl)
library(scales)
library(tidyverse)
library(httr)
library(data.table) #Unistall?

### - Function

extract_df <- function(date, dimensions, metrics, top, segmentId, search){
  
  data.frame(
    aw_freeform_table(
      company_id = Sys.getenv("AW_COMPANY_ID"),
      rsid = Sys.getenv("AW_REPORTSUITE_ID"),
      date_range = date,
      dimensions = dimensions,
      metrics = metrics,
      top = top,
      page = 0,
      filterType = "breakdown",
      #segmentId = NA,
      segmentId = segmentId,
      metricSort = "desc",
      include_unspecified = FALSE,
      search = search,
      prettynames = TRUE,
      debug = FALSE,
      check_components = TRUE
    )
  )
  
}

extractMultipleDay <- function(firstDay, nDay, dimensions, metrics, top, segmentId, search){
  
  output = "";
  
  for (i in 1:nDay){
    
    sDay = as.Date(firstDay , "%Y-%m-%d") + i - 1;
    eDay = as.Date(firstDay , "%Y-%m-%d") + i - 1;
    
    date = c(sDay, eDay)
    
    df <- extract_df(date, dimensions, metrics, top, segmentId, search) %>%
      select(-c(Day)) %>%
      mutate(Day = sDay) %>% 
      relocate(Day)
    
    output = rbind(output,df)
    
  }
  output <- output %>% slice(-1)
  return(output)
}

extractDailyCountry <- function(period, firstDay, nDay, segmentId){
  period = period;
  firstDay = firstDay;
  nDay = nDay;
  #segmentId = c(segmentId);
  segmentId = NA;
  
  ##DF_PURCHASE
  dimensions <- c("daterangeday","prop13", "evar9");
  metrics <- c("revenue","orders");
  nPathname = 1;
  nCountry = 50;
  top = c(nDay, nPathname, nCountry);
  search = c("", "MATCH '/booking/confirmation'", "");
  
  df_purchase <- extractMultipleDay(firstDay, nDay, dimensions, metrics, top, segmentId, search) %>%
    mutate(
      Country = tolower(Country),
      Period = period
    ) %>%
    mutate_at(c('Orders','Revenue'), as.numeric) %>%
    select(-c(Pathname)) %>% 
    relocate(Period, .after = Day)

  ##DF_PAGEVIEW
  dimensions <- c("daterangeday", "evar9");
  metrics <- c("visits","cm4461_641c20238ffe75048ad05192","event101","cm4461_6336e44d7136c5051634396c");
  top = c(nDay, nCountry);
  search = c("", "");
  
  df_pageView <- extractMultipleDay(firstDay, nDay, dimensions, metrics, top, segmentId, search) %>% 
    mutate(
      Period = period,
      VCR_tkts = X013..VCR..Booking.,
      Select_flight = XDM.e101...select.flight,
      BCR_tkts = X008..BCR..Updated.,
      Country = tolower(Country)
    ) %>% 
    mutate_at(c('Visits', "VCR_tkts", "Select_flight", 'BCR_tkts'), as.numeric) %>% 
    select(-c(X013..VCR..Booking., XDM.e101...select.flight, X008..BCR..Updated.)) %>% 
    relocate(Period, .after = Day)
  
  df_output_country <-  df_purchase %>% 
  left_join(df_pageView, by=c('Day', 'Period','Country'))
  
}


### - Login & Utilities
#se dentro VPN: turn ON
#set_config(use_proxy("proxy.user.alitalia.local", port = 8080, username = "IT011820", password = "Novembre23", auth = "basic"))


# Login JWT
aw_auth_with('jwt')
aw_auth()

nDay = 3
firstDay = '2024-01-01'

df_dailyCountryPre <- extractDailyCountry("pre", firstDay, nDay, 'NAbyDefaultForThisFunction')

firstDay = as.character((ymd(firstDay) + 7))
  
df_dailyCountryPost <- extractDailyCountry("post", firstDay, nDay, 'NAbyDefaultForThisFunction')

nPathname = 1;
nCountry = 50;
segmentId = NA;
dimensions <- c("daterangeday","prop13", "evar9");
metrics <- c("revenue");
top = c(nDay, nPathname, nCountry);
search = c("", "MATCH '/booking/confirmation'", "");

df_revenue_sort <- extractMultipleDay(firstDay, nDay, dimensions, metrics, top, segmentId, search) %>% 
  mutate(
    Country = tolower(Country),
  ) %>% 
  mutate_at(c('Revenue'), as.numeric) %>% 
  group_by(Country) %>% 
  summarise(Rev_tot = sum(Revenue)) %>% 
  arrange(desc(Rev_tot)) 

df_dailyCountryPre_Sort <-  df_dailyCountryPre %>% 
  left_join(df_revenue_sort, by=c('Country')) %>% 
  group_by(Day) %>% 
  arrange(desc(Rev_tot) , .by_group=TRUE)

df_dailyCountryPost_Sort <-  df_dailyCountryPost %>% 
  left_join(df_revenue_sort, by=c('Country')) %>% 
  group_by(Day) %>% 
  arrange(desc(Rev_tot) , .by_group=TRUE)

df_output_country = rbind(df_dailyCountryPre_Sort, df_dailyCountryPost_Sort)

write_xlsx(df_output_country, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/df_output_country.xlsx")