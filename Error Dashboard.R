### - Load Library

library(adobeanalyticsr)
library(dplyr)
library(cleaner) #Unistall?
library(gt)
library(writexl)
library(scales)
library(tidyverse)
library(httr)
library(ggplot2)

#install.packages("ggplot2")

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

extractMultipleDay <- function(firstDay, nDay, dimensions, metrics, top, search, segmentId){
  
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

### - Login & Utilities

#set_config(use_proxy("proxy.user.alitalia.local", port = 8080, username = "IT011820", password = "Maggio24!!", auth = "basic"))

# Login JWT
aw_auth_with('jwt')
aw_auth()


### Login OAUTH
#aw_auth_with('oauth')

# dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
# metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
# calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.
# segment = aw_get_segments()
# nDay = wday(Sys.Date())-1

segmentId = NA #c(segmentId);

firstDay = '2024-05-27';
nDay = 10;
nDAPI = 12;
nCountries = 1;
dimensions <- c("daterangeday", "prop27", "evar9");
metrics <- c("visitors");
top = c(nDay, nDAPI, nCountries);
search = c("", "", "MATCH 'it'");

df_err <- extractMultipleDay(firstDay, nDay, dimensions, metrics, top, search, segmentId)

dimensions <- c("daterangeday", "evar9");
metrics <- c("visitors");
top = c(nDay, nCountries);
search = c("", "MATCH 'it'");

df_uv <- extractMultipleDay(firstDay, nDay, dimensions, metrics, top, search, segmentId)

df_err_elab <- df_err %>%   
  mutate(Country = tolower(Country)) %>% 
  rename(uv_err = Unique.Visitors)

df_uv_elab <- df_uv %>%   
  mutate(Country = tolower(Country)) %>% 
  rename(uv_tot = Unique.Visitors)
  
df_elab <- df_err_elab %>%   
  left_join(df_uv_elab, by=c('Day','Country')) %>%
  mutate_at(c('uv_tot','uv_err'), as.numeric) %>%
  mutate(
    err_rate = percent(round(ifelse(uv_tot > 0, uv_err / uv_tot, 0),3))
    ) 

  #filter(Country == "it" & Error.Code..DAPI. == 7959)
  #filter(Country == "it")


# ggplot(data = df_err_elab, aes(Day, Unique.Visitors)) + 
#   geom_point()

ggplot(data = df_elab, aes(x=Day, y=err_rate, group=Error.Code..DAPI., color=Error.Code..DAPI.)) +
  geom_line() +
  theme(legend.position="none") +
  facet_wrap(~Error.Code..DAPI.,, nrow=5, ncol=4)

  

write_xlsx(df_elab, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/df_elab.xlsx")

%>% 
  mutate(
    Period = period,
    VCR_tkts = X004..VCR,
    Select_flight = XDM.e101...select.flight,
    BCR_tkts = X005..BCR
  ) %>% 
  mutate_at(c('Visits', "VCR_tkts", "Select_flight", 'BCR_tkts'), as.numeric) %>% 
  select(-c(X004..VCR, XDM.e101...select.flight, X005..BCR)) %>% 
  relocate(Period, .after = Day)









### - Setup Default Variable

 PW_start = '2024-05-20';
 
 PW_start = as.Date(PW_start , "%Y-%m-%d");
 PW_end = as.Date(PW_start , "%Y-%m-%d") + 6;
 CW_start = as.Date(PW_start , "%Y-%m-%d") + 7;
 CW_end = as.Date(PW_start , "%Y-%m-%d") + 13;

nDateRange = 2;
nCategory = 1;
nPrimaryCategory = 1;
nCountries = 1;
nRete = 3; 
nRoute = 300; # VAULT >=100

### - E_xtract

## ff_purchase

dimensions <- c("daterangeweek","category","prop17", "evar9", "evar107","evar101");
metrics <- c("revenue", "orders","units");
top = c(nDateRange, nCategory, nPrimaryCategory, nCountries, nRete, nRoute);
search = c("", "MATCH 'FlightTicket'", "MATCH 'Booking'", "MATCH 'IT'", "" , "(CONTAINS '-')");

date <- c(PW_start, PW_end);
ff_purchase_PW <- extract_df(date, dimensions, metrics, top, search);

date <- c(CW_start, CW_end);
ff_purchase_CW <- extract_df(date, dimensions, metrics, top, search);

## ff_pageView

dimensions <- c("daterangeweek" , "prop17", "evar9", "evar107","evar101");
metrics <- c("event101");
top = c(nDateRange, nPrimaryCategory, nCountries, nRete, nRoute);
search = c("", "MATCH 'Booking'", "MATCH 'IT'", "NOT '0'", "(CONTAINS '-')");

date <- c(PW_start, PW_end);
ff_pageView_PW <- extract_df(date, dimensions, metrics, top, search);

date <- c(CW_start, CW_end);
ff_pageView_CW <- extract_df(date, dimensions, metrics, top, search);

### - T_RANSFORM
 
ff_purchase_CW_totByRete <- ff_purchase_CW %>% 
  group_by(Rete) %>% 
  summarise(totByRete_Revenue =sum(Revenue),
             .groups = 'drop')
   
ff_purchase_PW_elab <- ff_purchase_PW %>%  mutate(Range = "PW")
ff_purchase_CW_elab <- ff_purchase_CW %>%  mutate(Range = "CW")

ff_pageView_PW_elab <- ff_pageView_PW %>%  mutate(Range = "PW")
ff_pageView_CW_elab <- ff_pageView_CW %>%  mutate(Range = "CW")

ff_purchase <- rbind(ff_purchase_PW_elab, ff_purchase_CW_elab)
ff_pageView <- rbind(ff_pageView_PW_elab, ff_pageView_CW_elab)

df_ff_purchase <- ff_purchase

df_ff_pageView <- ff_pageView

df_join <- df_ff_pageView %>% 
  right_join( df_ff_purchase, by=c('Range','Rete','Route','Country')) %>%
  rename(
    select_flight = XDM.e101...select.flight
  ) %>% 
  mutate(
    BCR = ifelse(select_flight > 0, Orders / select_flight, 0),
    AOV = ifelse(Orders > 0, round(Revenue / Orders,1), 0)
  ) %>% 
  #select(-c(Week.x,Week.y,primaryCategory.x, primaryCategory.y, Category, Orders)) 
  select(-c(Week.x,Week.y,primaryCategory.x, primaryCategory.y, Category)) 

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
  left_join( df_pw, by=c('Rete','Route','Country')) %>% 
  mutate(
    revenue_delta = ifelse(Revenue_cw > 0, (Revenue_cw - Revenue_pw) / Revenue_pw, 0),
    select_flight_delta = ifelse(select_flight_cw > 0, (select_flight_cw - select_flight_pw) / select_flight_pw, 0),
    units_delta = ifelse(Units_cw > 0, (Units_cw - Units_pw) / Units_pw, 0),
    BCR_delta = round((BCR_cw - BCR_pw)*100,1),
    AOV_delta = AOV_cw - AOV_pw
  ) # %>% 
#  select(-c(select_flight_cw,select_flight_pw, Revenue_cw, Revenue_pw, BCR_cw, BCR_pw, AOV_cw, AOV_pw)) 


#Order & Extract

df_export <- df_join_wow %>%
  left_join(ff_purchase_CW_totByRete, by=c('Rete')) %>%
  arrange(desc(totByRete_Revenue), desc(Revenue_cw)) %>% 
  group_by(Rete) %>% 
  slice(1:30) %>%
  arrange(desc(totByRete_Revenue), desc(Revenue_cw)) %>%
#  select(-c(totByRete_Revenue)) %>%
  select(c('Country','Rete','Route','Revenue_pw','select_flight_pw','Units_pw','BCR_pw','AOV_pw','Revenue_cw','select_flight_cw','Units_cw','BCR_cw','AOV_cw','revenue_delta','select_flight_delta','units_delta','BCR_delta','AOV_delta'))

### - Load 

df_export_it <- df_export

write_xlsx(df_export_it, "C:/Users/IT011820/OneDrive - ITA Italia Trasporto Aereo/Desktop/0. R/02_exported/df_export_IT.xlsx")