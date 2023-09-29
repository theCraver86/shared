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

### - Login & Utilities

#set_config(use_proxy("proxy.user.alitalia.local", port = 8080, username = "IT011820", password = "Agosto23", auth = "basic"))

# Login JWT
aw_auth_with('jwt')
aw_auth()


### Login OAUTH
#aw_auth_with('oauth')

# dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
# metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
# calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.
# segment = aw_get_segments()

### - Setup Default Variable
W_start = '2023-08-28';

nDateRange = 4;
W_start = as.Date(W_start , "%Y-%m-%d");
W_end = as.Date(W_start , "%Y-%m-%d") + ((nDateRange*7)-1);
nCategory = 1;
nPrimaryCategory = 1;

### - E_xtract

## ff_top10Countries_purchase
nCountries = 10;

date <- c(CW_start, CW_end);
dimensions <- c("daterangeweek","category","prop17", "evar9");
metrics <- c("revenue", "orders");
top = c(nDateRange, nCategory, nPrimaryCategory, nCountries);
search = c("", "MATCH 'FlightTicket'", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'");

ff_top10Countries_purchase <- extract_df(date, dimensions, metrics, top, search) %>% 
  mutate(Country = tolower(Country));

































