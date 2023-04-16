# ALERT: no country in attesa di capire officeID
#Estrarre Dimensioni
# Estrarre Metriche
# Pulire i data
# Fare i delta
# Fare le metriche calcoalte in R
# Predisporre esportazione Excel
# Colorare i dati

setwd("C:/Users/rtadd/OneDrive/Documenti")
getwd()

str(df_check)

S_CW = "2023-03-20"
typeof(S_CW)

mtcars %>%
  summarise(mean = mean(disp), n = n())

mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(disp), n = n())

iris <- as_tibble(iris) # so it prints a little nicer
rename(iris, petal_length = Petal.Length)

lookup <- c(pl = "Petal.Length", sl = "Sepal.Length")
rename(iris, all_of(lookup))

DF <- data.frame(cost=c(1e4, 2e5))

#assign a class    
class(DF$cost) <- c("money", class(DF$cost))

#S3 print method for the class    
print.money <- function(x, ...) {
  print.default(paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=",")))
}

#format method, which is necessary for formating in a data.frame   
format.money  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

?paste0

DF
#         cost
#1  $10,000.00
#2 $200,000.00

# Load dplyr
library(dplyr)

mtcars %>% slice(1L)
mtcars %>% slice_head(n = 5)

# A summary applied to ungrouped tbl returns a single row
mtcars %>%
  summarise(mean = mean(disp), n = n())

# Usually, you'll want to group first
mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(disp), n = n())

# Each summary call removes one grouping level (since that group
# is now just a single row)
mtcars %>%
  group_by(cyl, vs) %>%
  summarise(cyl_n = n()) %>%
  group_vars()

# BEWARE: reusing variables may lead to unexpected results
mtcars %>%
  group_by(cyl) %>%
  summarise(disp = mean(disp), sd = sd(disp))

# Refer to column names stored as strings with the `.data` pronoun:
var <- "mass"
summarise(starwars, avg = mean(.data[[var]], na.rm = TRUE))
# Learn more in ?dplyr_data_masking

# In dplyr 1.1.0, returning multiple rows per group was deprecated in favor
# of `reframe()`, which never messages and always returns an ungrouped
# result:
mtcars %>%
  group_by(cyl) %>%
  summarise(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))
# ->
mtcars %>%
  group_by(cyl) %>%
  reframe(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))


mtcars %>%
  group_by(cyl, am) %>%
  summarise(disp = n(), .groups = "rowwise")

?n()

mtcars %>%
  group_by(cyl, am) %>%
  summarise(disp = n())