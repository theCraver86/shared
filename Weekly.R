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

extractMultipleWeek <- function(lastSunday, nWeek, dimensions, metrics, top, segmentId, search){
  
  output = "";
  
  for (i in 1:nWeek){
    sWeek = as.Date(lastSunday , "%Y-%m-%d") - ((7*i)-1);
    eWeek = as.Date(lastSunday , "%Y-%m-%d") - (7*(i-1));
    
    date = c(sWeek, eWeek)

    df <- extract_df(date, dimensions, metrics, top, segmentId, search) %>% 
      mutate(Range = sWeek)
    
    output = rbind(output,df)
    
  }
  output <- output %>% slice(-1)
  return(output)
}

### - Login & Utilities

set_config(use_proxy("proxy.user.alitalia.local", port = 8080, username = "IT011820", password = "Novembre23", auth = "basic"))

# Login JWT
aw_auth_with('jwt')
aw_auth()

# dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
# metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
# calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.
# segment = data.frame(aw_get_segments(limit = 1000))

### - Setup Default Variable


### - E_xtract
#segmentId = c('s4461_651d73ffbcbd9254fe0ddee0');
segmentId = NA;
lastSunday = '2023-12-24';
nWeek = 1;

## - Overall

#NOTE
#cm4461_633fc91d19d98828d5df7e80 = 011. AOV
#cm4461_6336e5ce7136c5051634396f = 013. VCR (Overall)

dimensions <- c("daterangeweek");
metrics <- c("visits","revenue", "orders","cm4461_633fc91d19d98828d5df7e80","cm4461_6336e5ce7136c5051634396f");
top = c(nWeek);
search = c("");

ff__visit <- extractMultipleWeek(lastSunday, nWeek, dimensions, metrics, top, segmentId, search) %>% 
  mutate(
    AOV = prettyNum(X011..AOV, digits = 3),
    VCR = prettyNum(X013..VCR..Overall.)
  ) %>% 
  select(-c(Week,X011..AOV,X013..VCR..Overall.)) 

write_xlsx(ff__visit, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/W_visit.xlsx")

nCategory = 3;
nPrimaryCategory = 3;

dimensions <- c("category","prop17");
metrics <- c("revenue", "orders");
top = c(nCategory, nPrimaryCategory);
search = c("", "MATCH 'Booking' OR 'Checkin' OR 'Rebooking'");

ff__purchase <- extractMultipleWeek(lastSunday, nWeek, dimensions, metrics, top, segmentId, search)

write_xlsx(ff__purchase, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/W_purchase.xlsx")

## - by primaryCategory (prop17)

dimensions <- c("daterangeweek","prop17");
metrics <- c("visits");
top = c(nWeek);
search = c("", "MATCH 'Booking' OR 'Checkin' OR 'Rebooking'");

ff__visit_pc <- extractMultipleWeek(lastSunday, nWeek, dimensions, metrics, top, segmentId, search) %>% 
  select(-c(Week))

write_xlsx(ff__visit_pc, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/W_visit_pc.xlsx")