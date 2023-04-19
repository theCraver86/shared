### Setup

#install.packages("adobeanalyticsr")
#install.packages("dplyr")
#install.packages("cleaner")
#install.packages("gt")
#install.packages("writexl")
#install.packages("scales")

##PDF: https://rpubs.com/sdi_ben/adobeanalyticsr_demo
##

#--------------> TO DO: 3. Clean select_flight_delta = inf
#                       2. Validation Data
#                       

library(adobeanalyticsr)
library(dplyr)
library(cleaner) #Unistall?
library(gt)
library(writexl)
library(scales)

#set_config(use_proxy("proxy.user.alitalia.local", port = 8080, username = "IT011820", password = "19Miles86@", auth = "basic"))

### Login OAUTH
#aw_auth_with('oauth')

### Login JWT
aw_auth_with('jwt')
aw_auth()


### Extract
# dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
# metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
# calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.
# segment = aw_get_segments() 

# %>%  
#   filter(
#     Route == filter_debug
#   )
# filter_debug  = 'ROM-PMO';


#Setup Variable
nDateRange = 2;
nCategory = 1;
nPrimaryCategory = 1;

d_start = '2023-03-27';
d_end= '2023-04-09';

W1_start = d_start;
W2_start = '2023-04-03';

nCountries = 10;

###top10Countries_purchase
ff_top10Countries_purchase = aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(W2_start, d_end),
  dimensions = c("daterangeweek","category","prop17", "evar9"),
  metrics = c("revenue", "orders"),
  top = c(nDateRange, nCategory, nPrimaryCategory, nCountries), #nDateRange = 1;
  page = 0,
  filterType = "breakdown",
  segmentId = NA,
  metricSort = "desc",
  include_unspecified = FALSE,
  search = c("", "MATCH 'FlightTicket'", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'"),
  prettynames = TRUE,
  debug = FALSE,
  check_components = TRUE
)

df_ff_top10Countries_purchase = data.frame(ff_top10Countries_purchase)

nCountries = 20; # VAULT >=20
nRoute = 100; # VAULT >=100

###Purchase
ff_purchase = aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(d_start, d_end),
  dimensions = c("daterangeweek","category","prop17", "evar9","evar101"),
  metrics = c("revenue", "orders"),
  top = c(nDateRange, nCategory, nPrimaryCategory, nCountries, nRoute),
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

df_ff_purchase = data.frame(ff_purchase) %>% 
filter(tolower(Country) %in% df_ff_top10Countries_purchase$Country)

nCountries = 20; # VAULT >=20
nRoute = 100; # VAULT >=100

###Page View
ff_pageView = aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(d_start, d_end),
  dimensions = c("daterangeweek" , "prop17", "evar9", "evar101"),
  metrics = c("revenue", "visits", "event101"),
  top = c(nDateRange, nPrimaryCategory, nCountries, nRoute),
  page = 0,
  filterType = "breakdown",
  segmentId = NA,
  metricSort = "desc",
  include_unspecified = FALSE,
  search = c("", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'", "(CONTAINS '-')"),
  prettynames = TRUE,
  debug = FALSE,
  check_components = TRUE
)

df_ff_pageView = data.frame(ff_pageView) %>% 
  filter(tolower(Country) %in% df_ff_top10Countries_purchase$Country)
  
  # %>% 
  # group_by(Country)  %>%
  # summarise(Revenue = sum(Revenue)) %>% 
  # arrange(desc(Revenue), by_group = TRUE) 

df_check <- df_ff_pageView %>% 
  right_join( df_ff_purchase, by=c('Week','Country','Route')) %>%
  rename(
    select_flight = XDM.e101...select.flight,
    Revenue = Revenue.y
        ) %>% 
  mutate(
            Visits = formatC(Visits, big.mark=","),
            BCR = ifelse(select_flight > 0, Orders / select_flight, 0),
            AOV = ifelse(Orders > 0, round(Revenue / Orders,1), 0)
        ) %>% 
  select(-c(primaryCategory.x, primaryCategory.y, Category, Visits, Orders, Revenue.x)) 

# %>% filter(Week == '2023-03-27')
  
  # %>% 
  #   filter(
  #     Route == 'NAP-MIL',
  #     Week == '2023-03-27'
  #   )

#Capire come applicare il rename cross colonne
#suf <- "_pw";
#Revenue + suf = Revenue,

df_pw <- df_check %>% 
    filter(Week == W1_start) %>% 
    rename(
      select_flight_pw = select_flight,
      Revenue_pw = Revenue,
      BCR_pw = BCR,
      AOV_pw = AOV
    ) %>% 
  select(-c(Week)) 

df_cw <- df_check %>% 
  filter(Week == W2_start) %>% 
  rename(
    select_flight_cw = select_flight,
    Revenue_cw = Revenue,
    BCR_cw = BCR,
    AOV_cw = AOV
  ) %>% 
  select(-c(Week)) 

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
 # select(c(Country, Revenue_cw, Route)) %>% 
  arrange(desc(Country), desc(Revenue_cw), Route, by_group = TRUE) %>%
  group_by(Country)  %>%
  slice(1:20) %>% 
  arrange(desc(Country), desc(Revenue_cw), Route, by_group = TRUE)

  # group_by(Day) %>%
  # summarize(across(Revenue, sum), .groups = "drop_last")

###Excel 
  write_xlsx(df_join_wow, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/df_join_wow.xlsx")
  write_xlsx(df_check, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/df_check.xlsx")
  write_xlsx(df_ff_purchase, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/df_ff_purchase_.xlsx")
  write_xlsx(df_ff_pageView, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/df_ff_pageView.xlsx")

  write_xlsx(df_export, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/df_export_.xlsx")
  
### Trasform

  # %>% 
  # group_by(Country)  %>%
  # summarise(Revenue = sum(Revenue)) %>%
  # arrange(desc(Revenue), by_group = TRUE)
  
  
# 
# df_ff = data.frame(ff)
# 
# df_ff_l <- df_ff %>%
#   filter(Week == c("2023-03-20"))
# 
# df_ff_r <- df_ff %>%
#   filter(Week == c("2023-03-13"))
#   #select(-2)
# 
# df_elab <- df_ff_l %>% 
#   left_join( df_ff_r, by=c('Country')) %>%
#     reframe(Country, Revenue.x, Revenue.y, delta = (Revenue.x - Revenue.y)) 
# 
# 
# df_cosmetic <- df_elab %>%
#   rename(Revenue_PW = Revenue.x) %>% 
#     formatC(Revenue_PW, decimal.mark = ",")
# 
# ?as.currency

#reframe(delta = (Revenue.x - Revenue.y), n = n(), .groups = "drop")
#group_by(Country) %>%
#summarise(Country, Revenue.x, Revenue.y, delta = (Revenue.x - Revenue.y), .groups = "drop")
#summarize(across(pageviews:bounces, sum), .groups = "drop") %>%


### Load (elsewhere)


### Test & Debug

##### Top10 Countries
# df_ff_top10countries <- df_ff_purchase %>% 
#   filter(Week == W2_start) %>% 
#   group_by(Week, Country)  %>%
#   summarise(Revenue = sum(Revenue)) %>% 
#   arrange(Week, desc(Revenue), by_group = TRUE)

# metric[,1]
# dimension[,1]
# df_ff[,1]
# df_ff


### C&P whit proud!

#
#     df_ff <- df_ff_it %>%
#     count(month, wt=pageviews) %>%
#     ggplot(aes(month, n)) + geom_line() +
#     expand_limits(y=0) +
#     scale_x_date()+
#     scale_y_continuous(labels = number_format(big.mark = ".",decimal.mark = ",",accuracy = 1))+
#     labs(title="Overall Pageview - by month",
#          subtitle = "by month",
#          y="Pageviews",
#          x="Month")
#
#   df_elab <- df_GA %>%  mutate(page_to_join = substring(page_path, 5)) %>%
#     right_join(df_tblProdDiv, by = "page_to_join")  %>%
#     group_by(product, division, month) %>%
#     summarize(across(pageviews:bounces, sum), .groups = "drop") %>%
#     mutate(
#         pageviews_session = ifelse(sessions > 0, round(pageviews / sessions,1), 0),
#         avg_time_on_page = ifelse((pageviews - exits) > 0, (time_on_page / (pageviews - exits)), 0),
#         bounce_rate = ifelse(sessions > 0, (bounces / sessions) *100, 0)
#     ) %>%
#     filter( !is.na(month)) %>%
#     filter( !is.na(division))
#
# df %>%
# filter( division == division_filter,
# month(month) >= max(month(month))-1,
#                division == division_filter) %>%
#        mutate( delta = {{valueBox_kpi}} - lag({{valueBox_kpi}}),
#                delt_p = delta/lag({{valueBox_kpi}})) %>%
#        tail(1)