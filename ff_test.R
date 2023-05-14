#Setup Variable
nDateRange = 300;

W_start = '2023-01-01';
W_start = as.Date(W_start , "%Y-%m-%d");

W_end = as.Date(W_start , "%Y-%m-%d") + 7;

ff_test = aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(W_start, W_end),
  dimensions = c("daterangeday"),
  metrics = c("orders"),
  top = c(nDateRange),
  page = 0,
  filterType = "breakdown",
  segmentId = NA,
  metricSort = "desc",
  include_unspecified = FALSE,
  search = NA,
  prettynames = TRUE,
  debug = FALSE,
  check_components = TRUE
)

df_ff_test = data.frame(ff_test)

ff_test_2 = aw_freeform_table(
  company_id = Sys.getenv("AW_COMPANY_ID"),
  rsid = Sys.getenv("AW_REPORTSUITE_ID"),
  date_range = c(Sys.Date() - 90, Sys.Date() - 1),
  dimensions = c("daterangeday"),
  metrics = c("visits"),
  top = c(500),
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

df_ff_test_2 = data.frame(ff_test_2)

write_xlsx(df_ff_test_2, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/df_test_2_.xlsx")