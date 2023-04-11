### Setup

#install.packages("adobeanalyticsr")
#install.packages("dplyr")
#install.packages("cleaner")
#install.packages("gt")
#install.packages("writexl")
#install.packages("scales")

library(adobeanalyticsr)
library(dplyr)
library(cleaner) #Unistall?
library(gt)
library(writexl)
library(scales)

### Login OAUTH
aw_auth_with('oauth')

### Login JWT
aw_auth_with('jwt')
aw_auth()

getwd()

### Extract
dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.
segment = aw_get_segments() 

# ff_top10_county = aw_freeform_table(
#   company_id = Sys.getenv("AW_COMPANY_ID"),
#   rsid = Sys.getenv("AW_REPORTSUITE_ID"),
#   date_range = c(Sys.Date() - 8, Sys.Date() - 2),
#   dimensions = c("evar9"),
#   metrics = c("revenue"),
#   top = c(10),
#   page = 0,
#   filterType = "breakdown",
#   segmentId = NA,
#   metricSort = "desc",
#   include_unspecified = TRUE,
#   search = c("NOT '0' AND NOT 'APP')"),
#   prettynames = FALSE,
#   debug = FALSE,
#   check_components = TRUE
# )
# 
# df_ff_top10_county = data.frame(ff_top10_county)
# 
# top10_county <- df_ff_top10_county %>%
# select(-2) %>%
#   as.factor()
#   
# top10_county_app = c(c('it'))
# top10_county_app[1]

# ff_VAULT = aw_freeform_table(
#   company_id = Sys.getenv("AW_COMPANY_ID"),
#   rsid = Sys.getenv("AW_REPORTSUITE_ID"),
#   date_range = c(Sys.Date() - 8, Sys.Date() - 2),
#   
#   #dimensions = c("daterangeday", "product", "evar9", "prop17", "category"),
#   dimensions = c("evar9", "prop17", "product", "evar101"),
#   metrics = c("revenue"),
#   top = c(1,1,20,20),
#   page = 0,
#   filterType = "breakdown",
#   segmentId = NA,
#   metricSort = "desc",
#   include_unspecified = TRUE,
#   search = c("NOT '0' AND NOT 'APP'", "(MATCH 'Booking')", "(CONTAINS '-')", "(NOT CONTAINS 'undefined')"),
#   prettynames = TRUE,
#   debug = FALSE,
#   check_components = TRUE
# )

S_CW = "2023-03-20"
typeof(S_CW)

### Country memo
#dimensions = c("evar9"),
#search = c("NOT '0' AND NOT 'APP'"),

f_product = '-';

ff_pageView = aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(Sys.Date() - 2, Sys.Date() - 1),
  dimensions = c("daterangeday", "prop17", "evar9", "evar101"),
  metrics = c("visits", "event101"),
  top = c(2,1,10,20),
  page = 0,
  filterType = "breakdown",
  segmentId = NA,
  metricSort = "desc",
  include_unspecified = FALSE,
  search = c("", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'", "(CONTAINS '-')"),
  #search = NA,
  prettynames = TRUE,
  debug = FALSE,
  check_components = TRUE
)

ff_purchase = aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(Sys.Date() - 2, Sys.Date() - 1),
  dimensions = c("daterangeday","category","prop17", "evar9","evar101"),
  metrics = c("revenue", "orders"),
  top = c(2,1,1,10,20),
  page = 0,
  filterType = "breakdown",
  segmentId = NA,
  metricSort = "desc",
  include_unspecified = FALSE,
  search = c("", "MATCH 'FlightTicket'", "MATCH 'Booking'", "NOT '0' AND NOT 'APP'", "(CONTAINS '-')"),
  #search = NA,
  prettynames = TRUE,
  debug = FALSE,
  check_components = TRUE
)

df_ff_pageView = data.frame(ff_pageView)
df_ff_purchase = data.frame(ff_purchase)

# df_ff_pageView_elab <- df_ff_pageView %>% 
#   rename(Product = Route)

df_check <- df_ff_pageView %>% 
  right_join( df_ff_purchase, by=c('Day','Country','Route')) %>% 
  rename(select_flight = XDM.e101...select.flight) %>% 
  mutate(
            Visits = formatC(Visits, big.mark=","),
            BCR = percent(ifelse(select_flight > 0, Orders / select_flight, 0), accuracy = 0.1),
            AOV = ifelse(Orders > 0, round(Revenue / Orders,1), 0)
        ) %>% 
  select(-c(primaryCategory.x, primaryCategory.y, Category, Visits, Orders)) 

df_pw <- df_check %>% 
    filter(Day == c("2023-04-08")) %>% 
    rename_with( ~ paste0(.x, "_pw"))

df_cw <- df_check %>% 
  filter(Day == c("2023-04-09")) %>% 
  rename_with( ~ paste0(.x, "_cw"))

df_join_wow <- df_pw %>% 
  left_join( df_cw, by=c('Country','Route')) %>% 
    mutate(
      Visits = formatC(Visits, big.mark=","),
      BCR = percent(ifelse(select_flight > 0, Orders / select_flight, 0), accuracy = 0.1),
      AOV = ifelse(Orders > 0, round(Revenue / Orders,1), 0)
    )
  

  ### NEXT STEP - Spaccare in due e join
    
  # group_by(Day) %>%
  # summarize(across(Revenue, sum), .groups = "drop_last")

str(df_check)

###Excel 
  write_xlsx(df_check, "C:\\Users\\rtadd\\OneDrive\\Desktop\\Exported\\people.xlsx")

### Trasform

df_ff = data.frame(ff)

df_ff_l <- df_ff %>%
  filter(Week == c("2023-03-20"))

df_ff_r <- df_ff %>%
  filter(Week == c("2023-03-13"))
  #select(-2)

df_elab <- df_ff_l %>% 
  left_join( df_ff_r, by=c('Country')) %>%
    reframe(Country, Revenue.x, Revenue.y, delta = (Revenue.x - Revenue.y)) 


df_cosmetic <- df_elab %>%
  rename(Revenue_PW = Revenue.x) %>% 
    formatC(Revenue_PW, decimal.mark = ",")

?as.currency

#reframe(delta = (Revenue.x - Revenue.y), n = n(), .groups = "drop")
#group_by(Country) %>%
#summarise(Country, Revenue.x, Revenue.y, delta = (Revenue.x - Revenue.y), .groups = "drop")
#summarize(across(pageviews:bounces, sum), .groups = "drop") %>%


### Load (elsewhere)


### Test & Debug

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