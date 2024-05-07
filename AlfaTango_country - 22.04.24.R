### - Load Library

#install.package("adobeanalyticsr")

  library(adobeanalyticsr)
  library(dplyr)
  library(cleaner) #Unistall?
  library(gt)
  library(writexl)
  library(scales)
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
PW_start = '2024-04-22';

PW_start = as.Date(PW_start , "%Y-%m-%d");
PW_end = as.Date(PW_start , "%Y-%m-%d") + 6;
CW_start = as.Date(PW_start , "%Y-%m-%d") + 7;
CW_end = as.Date(PW_start , "%Y-%m-%d") + 13;
nDateRange = 2;
nCategory = 1;
nPrimaryCategory = 1;

### - E_xtract

## ff_top10Countries_purchase
nCountries = 20;

dimensions <- c("daterangeweek","category","prop17", "evar9");
metrics <- c("revenue", "orders", "units");
top = c(nDateRange, nCategory, nPrimaryCategory, nCountries);
search = c("", "MATCH 'FlightTicket'", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'");

date <- c(CW_start, CW_end);
ff_top10Countries_purchase_CW <- extract_df(date, dimensions, metrics, top, search) %>% 
  mutate(Country = tolower(Country));

date <- c(PW_start, PW_end);
ff_top10Countries_purchase_PW <- extract_df(date, dimensions, metrics, top, search) %>% 
  mutate(Country = tolower(Country));

## ff_top10Countries_pageview
nCountries = 20;

dimensions <- c("daterangeweek" , "prop17", "evar9");
metrics <- c("revenue", "event101");
top = c(nDateRange, nPrimaryCategory, nCountries);
search = c("", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'");

date <- c(PW_start, PW_end);
ff_top10Countries_pageView_PW <- extract_df(date, dimensions, metrics, top, search);

date <- c(CW_start, CW_end);
ff_top10Countries_pageView_CW <- extract_df(date, dimensions, metrics, top, search);

### - T_RANSFORM
ff_purchase_PW_elab <- ff_top10Countries_purchase_PW %>%  mutate(Range = "PW")
ff_purchase_CW_elab <- ff_top10Countries_purchase_CW %>%  mutate(Range = "CW") 

ff_pageView_PW_elab <- ff_top10Countries_pageView_PW %>%  mutate(Range = "PW")
ff_pageView_CW_elab <- ff_top10Countries_pageView_CW %>%  mutate(Range = "CW")

ff_purchase <- rbind(ff_purchase_PW_elab, ff_purchase_CW_elab) %>% 
  mutate(Country = tolower(Country))

ff_pageView <- rbind(ff_pageView_PW_elab, ff_pageView_CW_elab) %>% 
  mutate(Country = tolower(Country))

df_ff_purchase <- ff_purchase %>% 
  filter(tolower(Country) %in% ff_purchase_CW_elab$Country)

df_ff_pageView <- ff_pageView %>% 
  filter(tolower(Country) %in% ff_purchase_CW_elab$Country)

df_join <- df_ff_pageView %>% 
  right_join( df_ff_purchase, by=c('Range','Country')) %>%
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
  left_join( df_pw, by=c('Country')) %>% 
    mutate(
      revenue_delta = ifelse(Revenue_cw > 0, (Revenue_cw - Revenue_pw) / Revenue_pw, 0),
      select_flight_delta = ifelse(select_flight_cw > 0, (select_flight_cw - select_flight_pw) / select_flight_pw, 0),
      units_delta = ifelse(Units_cw > 0, (Units_cw - Units_pw) / Units_pw, 0),
      BCR_delta = round((BCR_cw - BCR_pw)*100,1),
      AOV_delta = AOV_cw - AOV_pw
    ) %>% 
  select(c('Country','Revenue_pw','select_flight_pw','Units_pw','BCR_pw','AOV_pw','Revenue_cw','select_flight_cw','Units_cw','BCR_cw','AOV_cw','revenue_delta','select_flight_delta','units_delta','BCR_delta','AOV_delta'))

#  select(-c(select_flight_cw,select_flight_pw, Revenue_cw, Revenue_pw, BCR_cw, BCR_pw, AOV_cw, AOV_pw)) 

#Order & Extract
df_export <- df_join_wow 

### - Load 
  
write_xlsx(df_export, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/df_export_country.xlsx")
