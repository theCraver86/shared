### - Load Library

library(adobeanalyticsr)
library(dplyr)
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

extractDaily <- function(period, firstDay, nDay, segmentId){
  period = period;
  firstDay = firstDay;
  nDay = nDay;
  segmentId = c(segmentId);

  ##DF_PAGEVIEW
  dimensions <- c("daterangeday");
  metrics <- c("visits","cm4461_641c20238ffe75048ad05192","event101","cm4461_6336e44d7136c5051634396c");
  top = c(nDay);
  search = c("");
  
  df_pageView <- extractMultipleDay(firstDay, nDay, dimensions, metrics, top, segmentId, search) %>% 
    mutate(
      Period = period,
      VCR_tkts = X013..VCR..Booking.,
      Select_flight = XDM.e101...select.flight,
      BCR_tkts = X008..BCR..Updated.
    ) %>% 
    mutate_at(c('Visits', "VCR_tkts", "Select_flight", 'BCR_tkts'), as.numeric) %>% 
    select(-c(X013..VCR..Booking., XDM.e101...select.flight, X008..BCR..Updated.)) %>% 
    relocate(Period, .after = Day)
    
  ##DF_PURCHASE
  dimensions <- c("daterangeday","category","prop13");
  metrics <- c("orders","revenue");
  nCategory = 3;
  nPathname = 1;
  top = c(nDay, nCategory, nPathname);
  search = c("","","MATCH '/booking/confirmation'");
  
  df_purchase <- extractMultipleDay(firstDay, nDay, dimensions, metrics, top, segmentId, search) %>% 
    mutate(
      Period = period
    ) %>% 
    mutate_at(c('Orders','Revenue'), as.numeric) %>% 
    relocate(Period, .after = Day)
  
  df_orders <- df_purchase %>% 
    filter(Category %in% 'FlightTicket') %>% 
    select(-c(Category, Pathname)) %>% 
    rename(Rev_tkts = Revenue) %>% 
    left_join(df_pageView, by=c('Day','Period'))
  
  df_revenue <- df_purchase %>% 
    filter(Category != "FlightTicket") %>%
    group_by(Day, Period) %>% 
    summarise(Rev_anc = sum(Revenue)) %>% 
    left_join(df_orders, by=c('Day','Period'))
}

### - Login & Utilities
#se dentro VPN: turn ON
#set_config(use_proxy("proxy.user.alitalia.local", port = 8080, username = "IT011820", password = "Novembre23", auth = "basic"))

# Login JWT
aw_auth_with('jwt')
aw_auth()

# dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
# metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
# calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.
# segment = data.frame(aw_get_segments(limit = 1000))

### - Setup Default Variable

### - E_xtract

nDay = 4
firstDay = '2023-12-25'

df_preWeb <- extractDaily('PRE_WEB', firstDay, nDay, 's4461_651d73ffbcbd9254fe0ddee0')

df_preApp <- extractDaily('PRE_APP', firstDay, nDay, 's4461_64771bc20788655ac137bcd6')

firstDay = '2024-01-01'

df_postWeb <- extractDaily('POST_WEB', firstDay, nDay, 's4461_651d73ffbcbd9254fe0ddee0')

df_postApp <- extractDaily('POST_APP', firstDay, nDay, 's4461_64771bc20788655ac137bcd6')


df_output = rbind(df_preWeb, df_postWeb, df_preApp, df_postApp)

write_xlsx(df_output, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/df_output.xlsx")

#write_xlsx(df_output, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/df_output.xlsx")
