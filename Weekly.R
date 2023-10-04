### - Load Library

library(adobeanalyticsr)
library(dplyr)
library(cleaner) #Unistall?
library(gt)
library(writexl)
library(scales)
library(tidyverse)
library(httr)

### - Function

extract_df <- function(date, dimensions, metrics, top, search){
  
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
      segmentId = NA,
      metricSort = "desc",
      include_unspecified = FALSE,
      search = search,
      prettynames = TRUE,
      debug = FALSE,
      check_components = TRUE
    )
  )
  
}

extractMultipleWeek <- function(lastSunday, nWeek, dimensions, metrics, top, search){
  
  output = "";
  
  for (i in 1:nWeek){
    sWeek = as.Date(lastSunday , "%Y-%m-%d") - ((7*i)-1);
    eWeek = as.Date(lastSunday , "%Y-%m-%d") - (7*(i-1));
    
    date = c(sWeek, eWeek)

    df <- extract_df(date, dimensions, metrics, top, search) %>% 
      mutate(Range = sWeek)
    
    output = rbind(output,df)
    
  }
  output <- output %>% slice(-1)
  return(output)
}

### - Login & Utilities

set_config(use_proxy("proxy.user.alitalia.local", port = 8080, username = "IT011820", password = "Agosto23", auth = "basic"))

# Login JWT
aw_auth_with('jwt')
aw_auth()

# dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
# metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
# calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.
# segment = data.frame(aw_get_segments(limit = 1000))

### - Setup Default Variable


### - E_xtract
lastSunday = '2023-10-01';
nWeek = 4;

nCategory = 30;
nPrimaryCategory = 3;

dimensions <- c("category","prop17");
metrics <- c("revenue", "orders");
top = c(nCategory, nPrimaryCategory);
search = c("", "MATCH 'Booking' OR 'Checkin' OR 'Rebooking'");

ff__test <- extractMultipleWeek(lastSunday, nWeek, dimensions, metrics, top, search) 

write_xlsx(ff__test, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/ff__test.xlsx")

get_me()
aw_get_reportsuites(company_id = 'ITASPA')

dimensions <- c("category","prop17");
metrics <- c("revenue", "orders");
top = c(nCategory, nPrimaryCategory);
search = c("", "MATCH 'Booking' OR 'Checkin' OR 'Rebooking'");

ff__purchase <- extractMultipleWeek(lastSunday, nWeek, dimensions, metrics, top, search) 

metrics <- c("visits");

ff__visit <- extractMultipleWeek(lastSunday, nWeek, dimensions, metrics, top, search) 

write_xlsx(ff__purchase, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/W_purchase.xlsx")
write_xlsx(ff__visit, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/W_visit.xlsx")