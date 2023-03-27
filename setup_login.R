### Setup

#install.packages("adobeanalyticsr")
#install.packages("dplyr")

library(adobeanalyticsr)
library(dplyr)

### Login

aw_auth_with('oauth')
aw_auth()

### Extract
dimension = data.frame(aw_get_dimensions()) #to get a list of available dimensions IDs.
metric = data.frame(aw_get_metrics())  #to get a list of available metrics IDs.
calculatedmetrics = data.frame(aw_get_calculatedmetrics()) #to get a list of available calculated metrics IDs.

ff = aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(Sys.Date() - 1, Sys.Date() - 1),
  #    date_range = c(2023-03-20, 2023-03-20),
  
  dimensions = c("daterangeday", "product", "evar9", "prop17", "category"),
  #dimensions = c("daterangeday", "category"),
  
  metrics = c("visits","revenue"),
  top = c(5),
  page = 0,
  filterType = "breakdown",
  segmentId = NA,
  metricSort = "desc",
  include_unspecified = TRUE,
  search = NA,
  prettynames = FALSE,
  debug = FALSE,
  check_components = TRUE
)

### Trasform

df_ff = data.frame(ff)


df_ff_it <- df_ff %>%
  filter(evar9 == "it", category == c("FlightTicket"), prop17 == c("Booking"))

df_ff_it


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