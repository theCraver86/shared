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

#set_config(use_proxy("proxy.user.alitalia.local", port = 8080, username = "IT011820", password = "Febbraio24", auth = "basic"))

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
PW_start = '2024-04-29';

PW_start = as.Date(PW_start , "%Y-%m-%d");
PW_end = as.Date(PW_start , "%Y-%m-%d") + 6;
CW_start = as.Date(PW_start , "%Y-%m-%d") + 7;
CW_end = as.Date(PW_start , "%Y-%m-%d") + 13;
nDateRange = 2;
nCategory = 1;
nPrimaryCategory = 1;

### - E_xtract

## ff_top10Countries_purchase
nCountries = 10;

date <- c(CW_start, CW_end);
dimensions <- c("daterangeweek","category","prop17", "evar9");
metrics <- c("revenue", "orders", "units");
top = c(nDateRange, nCategory, nPrimaryCategory, nCountries);
search = c("", "MATCH 'FlightTicket'", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'");

ff_top10Countries_purchase <- extract_df(date, dimensions, metrics, top, search) %>% 
  mutate(Country = tolower(Country));

## ff_purchase
nCountries = 20; # VAULT >=20
nRoute = 100; # VAULT >=100

dimensions <- c("daterangeweek","category","prop17", "evar9","evar101");
metrics <- c("revenue", "orders", "units");
top = c(nDateRange, nCategory, nPrimaryCategory, nCountries, nRoute);
search = c("", "MATCH 'FlightTicket'", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'", "(CONTAINS '-')");

date <- c(PW_start, PW_end);
ff_purchase_PW <- extract_df(date, dimensions, metrics, top, search);

date <- c(CW_start, CW_end);
ff_purchase_CW <- extract_df(date, dimensions, metrics, top, search);

## ff_pageView
nCountries = 20; # VAULT >=20
nRoute = 100; # VAULT >=100

dimensions <- c("daterangeweek" , "prop17", "evar9", "evar101");
metrics <- c("revenue", "event101");
top = c(nDateRange, nPrimaryCategory, nCountries, nRoute);
search = c("", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'", "(CONTAINS '-')");

date <- c(PW_start, PW_end);
ff_pageView_PW <- extract_df(date, dimensions, metrics, top, search);

date <- c(CW_start, CW_end);
ff_pageView_CW <- extract_df(date, dimensions, metrics, top, search);

### - T_RANSFORM

ff_purchase_PW_elab <- ff_purchase_PW %>%  mutate(Range = "PW") %>% mutate(Country = tolower(Country))
ff_purchase_CW_elab <- ff_purchase_CW %>%  mutate(Range = "CW") %>% mutate(Country = tolower(Country))

ff_pageView_PW_elab <- ff_pageView_PW %>%  mutate(Range = "PW") %>% mutate(Country = tolower(Country))
ff_pageView_CW_elab <- ff_pageView_CW %>%  mutate(Range = "CW") %>% mutate(Country = tolower(Country))

ff_purchase <- rbind(ff_purchase_PW_elab, ff_purchase_CW_elab)
ff_pageView <- rbind(ff_pageView_PW_elab, ff_pageView_CW_elab)

df_ff_purchase <- ff_purchase %>% 
  filter(tolower(Country) %in% ff_top10Countries_purchase$Country)

df_ff_pageView <- ff_pageView %>% 
  filter(tolower(Country) %in% ff_top10Countries_purchase$Country)

df_join <- df_ff_pageView %>% 
  right_join( df_ff_purchase, by=c('Range','Country','Route')) %>%
  rename(
    select_flight = XDM.e101...select.flight,
    Revenue = Revenue.y
  ) %>% 
  mutate(
    BCR = ifelse(select_flight > 0, Orders / select_flight, 0),
    AOV = ifelse(Orders > 0, round(Revenue / Orders,1), 0)
  ) %>% 
  select(-c(Week.x,Week.y,primaryCategory.x, primaryCategory.y, Category, Orders, Revenue.x)) 

df_pw <- df_join %>% 
  filter(Range == "PW") %>% 
  rename(
    select_flight_pw = select_flight,
    Revenue_pw = Revenue,
    Units_pw = Units,
    BCR_pw = BCR,
    AOV_pw = AOV
  ) %>% 
  select(-c(Range)) 

df_cw <- df_join %>% 
  filter(Range == "CW") %>% 
  rename(
    select_flight_cw = select_flight,
    Revenue_cw = Revenue,
    Units_cw = Units,
    BCR_cw = BCR,
    AOV_cw = AOV
  ) %>% 
  select(-c(Range)) 

df_join_wow <- df_cw %>% 
  left_join( df_pw, by=c('Country','Route')) %>% 
  mutate(
    revenue_delta = ifelse(Revenue_cw > 0, (Revenue_cw - Revenue_pw) / Revenue_pw, 0),
    select_flight_delta = ifelse(select_flight_cw > 0, (select_flight_cw - select_flight_pw) / select_flight_pw, 0),
    units_delta = ifelse(Units_cw > 0, (Units_cw - Units_pw) / Units_pw, 0),
    BCR_delta = round((BCR_cw - BCR_pw)*100,1),
    AOV_delta = AOV_cw - AOV_pw
  ) #%>% 
  #select(-c(select_flight_cw,select_flight_pw, Revenue_cw, Revenue_pw, BCR_cw, BCR_pw, AOV_cw, AOV_pw)) 

#Order & Extract
app <- ff_top10Countries_purchase %>% 
  select(Country,Revenue)

df_export <- df_join_wow %>%
  left_join( app, by=c('Country')) %>%
  group_by(Country)  %>%
  slice(1:20) %>% 
  arrange(desc(Revenue), desc(Revenue_cw)) %>%  #Revenue_byCountry, Revenue_byRoute
  select(-"Revenue") %>% 
  select(c('Country','Route','Revenue_pw','select_flight_pw','Units_pw','BCR_pw','AOV_pw','Revenue_cw','select_flight_cw','Units_cw','BCR_cw','AOV_cw','revenue_delta','select_flight_delta','units_delta','BCR_delta','AOV_delta'))


### - Load 

write_xlsx(df_export, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/df_export_.xlsx")

##### Departure Brand - Correggere estrazione 'orders' consoliticket
nCountries = 15; # VAULT >=20
nRoute = 50; # VAULT >=100
nDepMonth = 15;

dimensions <- c("prop17", "evar9", "evar101", "evar30");
#metrics <- c("event101",  "cm4461_6336e44d7136c5051634396c");
metrics <- c("event101", "orders");
top = c(nPrimaryCategory, nCountries, nRoute, nDepMonth);
search = c("MATCH 'Booking'", "NOT '0' AND NOT 'APP'", "(CONTAINS '-')", "");
#search = c("MATCH 'Booking'", "MATCH 'US'", "(CONTAINS '-')", "");

date <- c(PW_start, PW_end);
ff_departureMonth_PW <- extract_df(date, dimensions, metrics, top, search) %>% 
  filter(Departure.Month %in% c("5-2024", "6-2024", "7-2024", "8-2024", "9-2024", "10-2024", "11-2024", "12-2024")) %>% 
  mutate(Country = tolower(Country))

date <- c(CW_start, CW_end);
ff_departureMonth_CW <- extract_df(date, dimensions, metrics, top, search) %>% 
  filter(Departure.Month %in% c("5-2024", "6-2024", "7-2024", "8-2024", "9-2024", "10-2024", "11-2024", "12-2024")) %>% 
  mutate(Country = tolower(Country))

##SLICE Departure Month

ff_departureMonth_join_slice <- ff_departureMonth_CW %>% 
  left_join(ff_departureMonth_PW, by=c('Country','Route','Departure.Month'), suffix = c("_CW", "_PW")) %>%
  rename(
    select_flight_PW = XDM.e101...select.flight_PW,
    select_flight_CW = XDM.e101...select.flight_CW
  ) %>% 
  mutate(
    BCR_PW = ifelse(select_flight_PW > 0, Orders_PW / select_flight_PW, 0),
    BCR_CW = ifelse(select_flight_CW > 0, Orders_CW / select_flight_CW, 0),
    BCR_delta = round((BCR_CW - BCR_PW)*100,1)
  ) %>% 
  arrange(Country, Route, desc(select_flight_CW)) %>% 
  group_by(Country, Route)  %>%
  slice(1:3) %>% 
  select(-c(primaryCategory_CW, primaryCategory_PW, select_flight_CW, select_flight_PW, BCR_PW, BCR_CW, Orders_PW, Orders_CW))

ff_departureMonth_slice <- ff_departureMonth_join_slice %>% 
  pivot_wider(
    names_from = Departure.Month, 
    values_from = BCR_delta
    #values_fill = "no top3 select flight"
  ) %>% 
  right_join(df_export, by=c('Country','Route')) %>% 
  select(c('Country', 'Route', 'Revenue_cw', 'select_flight_cw', 'BCR_cw', 'AOV_cw', 'revenue_delta', 'select_flight_delta', 'BCR_delta', 'AOV_delta', '5-2024', '6-2024', '7-2024', '8-2024', '9-2024', '10-2024', '11-2024', '12-2024'))


write_xlsx(ff_departureMonth_slice, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/ff_departureMonth_slice.xlsx")

##FULL Departure Month

ff_departureMonth_join_full <- ff_departureMonth_CW %>% 
  left_join(ff_departureMonth_PW, by=c('Country','Route','Departure.Month'), suffix = c("_CW", "_PW")) %>%
  rename(
    select_flight_PW = XDM.e101...select.flight_PW,
    select_flight_CW = XDM.e101...select.flight_CW
  ) %>% 
  mutate(
    BCR_PW = ifelse(select_flight_PW > 0, Orders_PW / select_flight_PW, 0),
    BCR_CW = ifelse(select_flight_CW > 0, Orders_CW / select_flight_CW, 0),
    BCR_delta = round((BCR_CW - BCR_PW)*100,1)
  ) %>% 
  arrange(Country, Route, desc(select_flight_CW)) %>% 
  group_by(Country, Route)  %>%
  #slice(1:3) %>% 
  select(-c(primaryCategory_CW, primaryCategory_PW, select_flight_CW, select_flight_PW, BCR_PW, BCR_CW, Orders_PW, Orders_CW))

ff_departureMonth_full <- ff_departureMonth_join_full %>% 
  pivot_wider(
    names_from = Departure.Month, 
    values_from = BCR_delta
    #values_fill = "no top3 select flight"
  ) %>% 
  right_join(df_export, by=c('Country','Route')) %>% 
  select(c('Country', 'Route', 'Revenue_cw', 'select_flight_cw', 'BCR_cw', 'AOV_cw', 'revenue_delta', 'select_flight_delta', 'BCR_delta', 'AOV_delta', '5-2024', '6-2024', '7-2024', '8-2024','9-2024','10-2024','11-2024','12-2024'))

write_xlsx(ff_departureMonth_full, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/ff_departureMonth_full.xlsx")