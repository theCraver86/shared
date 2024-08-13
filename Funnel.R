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
library(readxl)
library(openxlsx)

# path_to_file = "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/05. library/openxlsx_4.2.3.tar.gz";
# install.packages(path_to_file, repos = NULL, type="source")

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
#set_config(use_proxy("proxy.user.alitalia.local", port = 8080, username = "IT011820", password = "Agosto24#", auth = "basic"))

# Login JWT
aw_auth_with('jwt')
#aw_auth_with('oauth')
aw_auth()


# dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
# metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
# calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.
# segment = data.frame(aw_get_segments(limit = 1000))

### - Setup Default Variable

### - E_xtract
updateWeek = 'Y';

##DF_PAGEVIEW
period = 'PRE_WEB';
firstDay = '2024-08-05';
nDay = 7;
nCountries = 1;
segmentId = NA;

##PAGE VIEW
dimensions <- c("daterangeday", "evar9");
metrics <- c("visits","event101", "event102", "event103", "event105", "event106", "event107");
top = c(nDay, nCountries);
search = c("", "MATCH 'IT'");

df_pageView <- extractMultipleDay(period, firstDay, nDay, dimensions, metrics, top, segmentId, search) 

#legend
#e101 - Select Flight
#e102 - Flight Recap
#e103 - Info Pax
#e105 - Ancillary
#e106 - Payment
#e107 - Receipt

df_pageView_elab <- df_pageView %>% 
  rename(
    e101 = XDM.e101...select.flight,
    e102 = XDM.e102...flight.recap,
    e103 = XDM.e103...infoPax_1...Personal.information,
    e105 = XDM.e105...ancillary,
    e106 = XDM.e106...payment,
    e107 = XDM.e107...receipt
  ) %>% 
  mutate_at(c('Visits','e101','e102','e103','e105','e106','e107'), as.numeric) %>% 
  mutate(
    visit_e101 = Visits - e101,
    e101_e102 = e101 - e102,
    e102_e103 = e102 - e103,
    e103_e105 = e103 - e105,
    e105_e106 = e105 - e106,
    e106_e107 = e106 - e107,
    drop_visit = visit_e101 / Visits,
    drop_e101 = e101_e102 / e101,
    drop_e102 = e102_e103 / e102,
    drop_e103 = e103_e105 / e103,
    drop_e106 = e105_e106 / e105,
    drop_e107 = e106_e107 / e106,
  ) %>%
  select(-c(Period, Visits, e101, e102, e103, e105, e106, e107)) %>%
  select(-c(visit_e101, e101_e102, e102_e103, e103_e105, e105_e106, e106_e107))

d_visit <- df_pageView_elab %>% 
  select(c(Country, Day, drop_visit)) %>% 
  mutate(
    label = 'Visit --> Select Flight'
  ) %>% 
  pivot_wider(
    names_from = Day,
    values_from = drop_visit
  ) 

d_e101 <- df_pageView_elab %>% 
  select(c(Country, Day, drop_e101)) %>% 
  mutate(
    label = 'Select Flight --> Flight Recap'
  ) %>% 
  pivot_wider(
    names_from = Day,
    values_from = drop_e101
  )

d_e102 <- df_pageView_elab %>% 
  select(c(Country, Day, drop_e102)) %>% 
  mutate(
    label = 'Flight Recap --> Personal Information'
  ) %>% 
  pivot_wider(
    names_from = Day,
    values_from = drop_e102
  )

d_e103 <- df_pageView_elab %>% 
  select(c(Country, Day, drop_e103)) %>% 
  mutate(
    label = 'Personal Information --> Ancillary'
  ) %>% 
  pivot_wider(
    names_from = Day,
    values_from = drop_e103
  )

d_e106 <- df_pageView_elab %>% 
  select(c(Country, Day, drop_e106)) %>% 
  mutate(
    label = 'Ancillary --> Payment'
  ) %>% 
  pivot_wider(
    names_from = Day,
    values_from = drop_e106
  )

d_e107 <- df_pageView_elab %>% 
  select(c(Country, Day, drop_e107)) %>% 
  mutate(
    label = 'Payment --> Receipt'
  ) %>% 
  pivot_wider(
    names_from = Day,
    values_from = drop_e107
  )

  df_vMerge <- d_visit %>% 
  rbind(d_e101, d_e102, d_e103, d_e106, d_e107)


write_xlsx(df_pageView_elab, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/df_pageView_elab.xlsx")

##PURCHHASE
nCategory = 1;
nPathname = 1;

dimensions <- c("daterangeday","lasttouchchannel","category", "prop13");
metrics <- c("revenue", "orders", "units");
top = c(nDay, nLTC, nCategory, nPathname);
search = c("", "", "MATCH 'FlightTicket'", "MATCH '/booking/confirmation'");

df_purchase <- extractMultipleDay(period, firstDay, nDay, dimensions, metrics, top, segmentId, search) 

df_purchase_elab <- df_purchase %>% 
  select(-c(Category, Pathname, Period)) %>% 
  mutate_at(c('Orders','Revenue','Units'), as.numeric) 

df_join <- df_pageView_elab %>% 
  left_join(df_purchase_elab, by=c('Day','Last.Touch.Channel')) %>% 
  mutate(
    VCR = ifelse(Visits > 0, Orders / Visits, 0),
    BCR = ifelse(Select_flight > 0, Orders / Select_flight, 0),
    AUV = ifelse(Units > 0, round(Revenue / Units,1), 0),
    AOV = ifelse(Orders > 0, round(Revenue / Orders,1), 0)
  )



### - Export to a specific Excel
#path = "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/";
setwd("C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/")

target_wb <- loadWorkbook("copyPaste.xlsx");
target_sheet <- "a"

writeData(target_wb, sheet = target_sheet, x = df_join, startCol = 1, startRow = 1)
saveWorkbook(target_wb, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/copyPaste.xlsx", overwrite = TRUE)