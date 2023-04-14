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

