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

# install.packages("readxl")
# install.packages("openxlsx")
# install.packages("openxlsx", version='4.0.0')
# remove.packages("openxlsx")

# path_to_file = "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/05. library/openxlsx_4.2.3.tar.gz";
# install.packages(path_to_file, repos = NULL, type="source")


#sessionInfo()

library(readxl)
library(openxlsx)

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

extractMultipleDay <- function(period, firstDay, nDay, dimensions, metrics, top, segmentId, search){
  
  output = "";
  
  for (i in 1:nDay){
    
    sDay = as.Date(firstDay , "%Y-%m-%d") + i - 1;
    eDay = as.Date(firstDay , "%Y-%m-%d") + i - 1;
    
    date = c(sDay, eDay)
    
    df <- extract_df(date, dimensions, metrics, top, segmentId, search) %>%
      select(-c(Day)) %>%
      mutate(
        Day = sDay,
        Period = period) %>% 
      relocate(Day)
    
    output = rbind(output,df)
    
  }
  output <- output %>% slice(-1)
  return(output)
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

##DF_PAGEVIEW
period = 'PRE_WEB';
firstDay = '2024-06-03';
nDay = 2;
nLTC=20;
dimensions <- c("daterangeday","lasttouchchannel");
metrics <- c("visits","revenue");
top = c(nDay,nLTC);
segmentId = 's4461_651d73ffbcbd9254fe0ddee0';
search = c("","");

df_pageView <- extractMultipleDay(period, firstDay, nDay, dimensions, metrics, top, segmentId, search) 

df_pageView_elab <- df_pageView %>% 
  select(-c('Revenue', 'Period')) %>% 
  pivot_wider(
    names_from = Day,
    values_from = c(Visits)
  )
  
  mutate(
    Period = period,
    VCR_tkts = X004..VCR,
    Select_flight = XDM.e101...select.flight,
    BCR_tkts = X005..BCR
  ) %>% 
  mutate_at(c('Visits', "VCR_tkts", "Select_flight", 'BCR_tkts'), as.numeric) %>% 
  select(-c(X004..VCR, XDM.e101...select.flight, X005..BCR)) %>% 
  relocate(Period, .after = Day)

### - Export to a specific Excel

target_wb <- loadWorkbook("C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/copyPaste.xlsx")
target_sheet <- "a"

writeData(target_wb, sheet = target_sheet, x = df_pageView, startCol = 1, startRow = 1)
saveWorkbook(target_wb, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/copyPaste.xlsx", overwrite = TRUE)



##DF_PURCHASE
dimensions <- c("daterangeday","category","prop13");
metrics <- c("orders","revenue","units");
nCategory = 3;
nPathname = 1;
top = c(nDay, nCategory, nPathname);
search = c("","","MATCH '/booking/confirmation'");

df_purchase <- extractMultipleDay(firstDay, nDay, dimensions, metrics, top, segmentId, search) %>% 
  mutate(
    Period = period
  ) %>% 
  mutate_at(c('Orders','Revenue','Units'), as.numeric) %>% 
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