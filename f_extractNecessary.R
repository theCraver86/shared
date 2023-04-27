### Setup

#install.packages("adobeanalyticsr")
#install.packages("dplyr")
#install.packages("cleaner")
#install.packages("gt")
#install.packages("writexl")
#install.packages("scales")

##PDF: https://rpubs.com/sdi_ben/adobeanalyticsr_demo
##

library(adobeanalyticsr)
library(dplyr)
library(cleaner) #Unistall?
library(gt)
library(writexl)
library(scales)

### Login JWT
aw_auth_with('jwt')
aw_auth()


### Extract
# dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
# metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
# calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.
# segment = aw_get_segments() 


#Setup Variable
PW_start = '2023-04-10';

CW_start = as.Date(PW_start , "%Y-%m-%d") + 7;
CW_end = as.Date(PW_start , "%Y-%m-%d") + 13;

nDateRange = 1;
nCategory = 1;
nPrimaryCategory = 1;
nCountries = 10;
nRoute = 20;

###top10Countries_purchase
ff_extractNecessary_r = aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(CW_start, CW_end),
  dimensions = c("daterangeweek","category","prop17", "evar9", "evar101"),
  metrics = c("revenue", "orders"),
  top = c(nDateRange, nCategory, nPrimaryCategory, nCountries, nRoute), #nDateRange = 1;
  page = 0,
  filterType = "breakdown",
  segmentId = NA,
  metricSort = "desc",
  include_unspecified = FALSE,
  search = c("", "MATCH 'FlightTicket'", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'", "(CONTAINS '-')"),
  prettynames = TRUE,
  debug = FALSE,
  check_components = TRUE
)

###top10Countries_purchase
ff_extractNecessary_p = aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(CW_start, CW_end),
  dimensions = c("daterangeweek","category","prop17", "evar9", "product"),
  metrics = c("revenue", "orders"),
  top = c(nDateRange, nCategory, nPrimaryCategory, nCountries, nRoute), #nDateRange = 1;
  page = 0,
  filterType = "breakdown",
  segmentId = NA,
  metricSort = "desc",
  include_unspecified = FALSE,
  search = c("", "MATCH 'FlightTicket'", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'", "(CONTAINS '-')"),
  prettynames = TRUE,
  debug = FALSE,
  check_components = TRUE
)

ff_route <- data.frame(ff_extractNecessary_r)  %>% 
  rename(
    Orders_r = Orders,
    Revenue_r = Revenue,
    ID = Route
  )

ff_product <- data.frame(ff_extractNecessary_p) %>% 
  rename(
    Orders_p = Orders,
    Revenue_p = Revenue,
    ID = Product
  )

df_join <- ff_product %>% 
  left_join( ff_route, by=c('Week','Category','primaryCategory','Country','ID')) %>% 
  mutate(
    Revenue_d = formatC(Revenue_p-Revenue_r, big.mark=","),
    Orders_d = formatC(Orders_p-Orders_r, big.mark=",")
  ) %>% 
  filter(Orders_d == 0)

df_ff_purchase = data.frame(ff_purchase) %>% 

df_join <- ff_product %>% 
  left_join( ff_route, by=c('Week','Category','primaryCategory','Country','ID')) %>%
  rename(
    select_flight = XDM.e101...select.flight,
    Revenue = Revenue.y
        ) %>% 
  mutate(
            Visits = formatC(Visits, big.mark=","),
            BCR = ifelse(select_flight > 0, Orders / select_flight, 0),
            AOV = ifelse(Orders > 0, round(Revenue / Orders,1), 0)
        ) %>% 
 # select(-c(primaryCategory.x, primaryCategory.y, Category, Visits, Orders, Revenue.x)) 
  select(-c(primaryCategory.x, primaryCategory.y, Category, Visits, Revenue.x)) 

df_join_wow <- df_cw %>% 
  left_join( df_pw, by=c('Country','Route')) %>% 
    mutate(
      select_flight_delta = percent(ifelse(select_flight_cw > 0, (select_flight_cw - select_flight_pw) / select_flight_pw, 0), accuracy = 0.1),
      Revenue_delta = percent(ifelse(Revenue_cw > 0, (Revenue_cw - Revenue_pw) / Revenue_pw, 0), accuracy = 0.1),
      BCR_delta = (BCR_cw - BCR_pw)*100,
      AOV_delta = AOV_cw - AOV_pw
    ) %>% 
  select(-c(select_flight_pw, Revenue_pw, BCR_pw, AOV_pw)) 

#Order & Extract
df_export <- df_join_wow %>% 
  arrange(desc(Country), desc(Revenue_cw), Route, by_group = TRUE) %>%
  group_by(Country)  %>%
  slice(1:20) %>% 
  arrange(desc(Country), desc(Revenue_cw), Route, by_group = TRUE)


###Excel 

  write_xlsx(df_export, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/df_export_.xlsx")