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

label_and_pivot <- function(df, label, toPivotValue){
  
  df %>% 
    select(c(Country, Day, toPivotValue)) %>% 
    mutate(
      label = label
    ) %>% 
    pivot_wider(
      names_from = Day,
      values_from = all_of(toPivotValue)
    )
  
}

# dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
# metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
# calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.
# segment = data.frame(aw_get_segments(limit = 1000))

# Login JWT
aw_auth_with('jwt')
#aw_auth_with('oauth')
aw_auth()

### - Setup Default Variable
period = '4weeks+wtd';

#Extract 4weeks + WTD
if (wday(Sys.Date()) == 1) { #Sunday
  fixWeekDay = -5;
} else {
  fixWeekDay = 2;
}
nDay = 7 + wday(Sys.Date()) - fixWeekDay; 
firstDay = Sys.Date() - nDay;

segmentId = NA;
search_evar9 = "(CONTAINS 'IT') OR (CONTAINS 'US') OR (CONTAINS 'APP') OR (CONTAINS 'BR') OR (CONTAINS 'EN') OR (CONTAINS 'AR') OR (CONTAINS 'GB') OR (CONTAINS 'ES') OR (CONTAINS 'FR') OR (CONTAINS 'IL') OR (CONTAINS 'DE') OR (CONTAINS 'CH') OR (CONTAINS 'JP') OR (CONTAINS 'CA') OR (CONTAINS 'NL') OR (CONTAINS 'BE') OR (CONTAINS 'IN') OR (CONTAINS 'AU') OR (CONTAINS 'GR') OR (CONTAINS 'EG') OR (CONTAINS 'AL') OR (CONTAINS 'BG') OR (CONTAINS 'TN)"

#EXTRACT

##PAGE VIEW
nCountries = 50;
dimensions <- c("daterangeday", "evar9");
metrics <- c("visits","event101", "event102", "event103", "event105", "event106", "event107", "cm4461_6336e44d7136c5051634396c", "cm4461_64832b12159d784fb1ca05dc"); #005. BCR & error_rate
top = c(nDay, nCountries);
search = c("", search_evar9);

df_pageView <- extractMultipleDay(period, firstDay, nDay, dimensions, metrics, top, segmentId, search) 

# ##ERROR
# dimensions <- c("daterangeday", "evar9");
# metrics <- c("cm4461_64832b12159d784fb1ca05dc"); #Failure Rate
# top = c(nDay, nCountries);
# search = c("",  search_evar9);
# 
# df_error <- extractMultipleDay(period, firstDay, nDay, dimensions, metrics, top, segmentId, search) 

##PURCHASE
nPathname = 1;
dimensions <- c("daterangeday", "prop13", "evar9");
metrics <- c("revenue", "orders");
top = c(nDay, nPathname, nCountries);
search = c("", "MATCH '/booking/confirmation'", search_evar9);

df_purchase <- extractMultipleDay(period, firstDay, nDay, dimensions, metrics, top, segmentId, search) 

#TRANSFORM

##PURCHASE --> to sort by Revenue
df_purchase_elab <- df_purchase %>% 
  mutate(
    Country = tolower(Country),
  ) %>% 
  mutate_at(c('Revenue'), as.numeric) %>% 
  group_by(Country) %>% 
  summarise(totRevenue = sum(Revenue))

##PAGE VIEW
df_pageView_elab <- df_pageView %>% 
  rename(
    e101 = XDM.e101...select.flight,
    e102 = XDM.e102...flight.recap,
    e103 = XDM.e103...infoPax_1...Personal.information,
    e105 = XDM.e105...ancillary,
    e106 = XDM.e106...payment,
    e107 = XDM.e107...receipt,
    BCR = X005..BCR,
    error_rate = Failure.Rate.Amadeus....Unique.Visitor.Overall.
    
  ) %>% 
  mutate_at(c('Visits','e101','e102','e103','e105','e106','e107', 'BCR', 'error_rate'), as.numeric) %>% 
  mutate(
    
    Country = tolower(Country),
    Day = format(Day, "%d-%m-%Y"),
    
    #Drop Valori Assoluti
    visit_e101 = Visits - e101,
    e101_e102 = e101 - e102,
    e102_e103 = e102 - e103,
    e103_e105 = e103 - e105,
    e105_e106 = e105 - e106,
    e106_e107 = e106 - e107,
    
    #Drop perc
    drop_visit = visit_e101 / Visits,
    drop_e101 = e101_e102 / e101,
    drop_e102 = e102_e103 / e102,
    drop_e103 = e103_e105 / e103,
    drop_e105 = e105_e106 / e105,
    drop_e106 = e106_e107 / e106,
  ) %>% 
  select(-c(Period))
#%>% select(-c(Period, Visits, e101, e102, e103, e105, e106, e107))

# ##ERROR
# df_error_elab <- df_error %>% 
#   rename(error_rate = Failure.Rate.Amadeus....Unique.Visitor.Overall.) %>% 
#   mutate_at(c('error_rate'), as.numeric) %>% 
#   mutate(
#     Country = tolower(Country),
#     Day = format(Day, "%d-%m-%Y"),
#     label = '0000. error_rate'
#     ) %>% 
#   pivot_wider(
#     names_from = Day,
#     values_from = error_rate
#   ) %>% 
#   right_join(df_purchase_elab, by=c('Country')) %>% 
#   arrange(desc(totRevenue), label) %>% 
#   select(-c(totRevenue, Period))

#valori assoluti
va_visit <- label_and_pivot(df_pageView_elab, '01. Visit', 'Visits')
va_e101 <- label_and_pivot(df_pageView_elab, '02. Select Flight', 'e101')
va_e102 <- label_and_pivot(df_pageView_elab, '03. Flight Recap', 'e102')
va_e103 <- label_and_pivot(df_pageView_elab, '04. Personal Information', 'e103')
va_e105 <- label_and_pivot(df_pageView_elab, '05. Ancillary', 'e105')
va_e106 <- label_and_pivot(df_pageView_elab, '06. Payment', 'e106')
va_e107 <- label_and_pivot(df_pageView_elab, '07. Receipt', 'e107')

df_output_va <- va_visit %>% 
  rbind(va_e101, va_e102, va_e103, va_e105, va_e106, va_e107) %>% 
  right_join(df_purchase_elab, by=c('Country')) %>% 
  arrange(desc(totRevenue), label) %>% 
  select(-c(totRevenue))

#drop %
d_visit <- label_and_pivot(df_pageView_elab, '01. Visit --> Select Flight', 'drop_visit')
d_e101 <- label_and_pivot(df_pageView_elab, '02. Select Flight --> Flight Recap', 'drop_e101')
d_e102 <- label_and_pivot(df_pageView_elab, '03. Flight Recap --> Personal Information', 'drop_e102')
d_e103 <- label_and_pivot(df_pageView_elab, '04. Personal Information --> Ancillary', 'drop_e103')
d_e105 <- label_and_pivot(df_pageView_elab, '05. Ancillary --> Payment', 'drop_e105')
d_e106 <- label_and_pivot(df_pageView_elab, '06. Payment --> Receipt', 'drop_e106')

df_output_drop <- d_visit %>% 
  rbind(d_e101, d_e102, d_e103, d_e105, d_e106) %>% 
  right_join(df_purchase_elab, by=c('Country')) %>% 
  arrange(desc(totRevenue), label) %>% 
  select(-c(totRevenue))

BCR <- label_and_pivot(df_pageView_elab, '000. BCR', 'BCR')
error_rate <- label_and_pivot(df_pageView_elab, '0000. error_rate', 'error_rate')


#LOAD
df_output <- df_output_va %>% 
  rbind(df_output_drop,  df_error_elab, BCR) %>% 
  right_join(df_purchase_elab, by=c('Country')) %>% 
  arrange(desc(totRevenue), label)
  
write_xlsx(df_output, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/df_output.xlsx")

setwd("C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/06. Automated Report - template/02. Funnel & Error");

target_wb <- loadWorkbook("tmp-Funnel_&_Error_dashboard.xlsx");
target_sheet <- "H_DB";

writeData(target_wb, sheet = target_sheet, x = df_output, startCol = 2, startRow = 2, colNames = TRUE)
saveWorkbook(target_wb, "tmp-Funnel_&_Error_dashboard.xlsx", overwrite = TRUE)

# LEGEND #
#e101 - Select Flight
#e102 - Flight Recap
#e103 - Info Pax
#e105 - Ancillary
#e106 - Payment
#e107 - Receipt