#install.packages("rio")
#install.packages("ggplot2")
library(rio)
library(dplyr)
library(ggplot2)

#install_formats()

#test <- import("C:/Users/rtadd/OneDrive/Desktop/R/3titanic/0_import/test.csv")
train <- import("C:/Users/rtadd/OneDrive/Desktop/R/3titanic/0_import/train.csv")
#gender_submission <- import("C:/Users/rtadd/OneDrive/Desktop/R/3titanic/0_import/gender_submission.csv")

str(train)

train_numeric <- train %>%
  mutate(
    Sex_reco = ifelse(Sex == 'male', 1 , 2),
    Embarked_reco = case_when(
      Embarked %in% "C" ~ 1,
      Embarked %in% "Q" ~ 2,
      Embarked == "S" ~ 3
    ) 
  ) %>% 
  select(-c(Name, Sex, Embarked, Cabin, Ticket))

clean_Age <- train_numeric %>% 
  select(c(PassengerId, Age)) %>% 
  filter(!is.na(Age))

ggplot(data = clean_Age, aes(x = PassengerId, y = Age)) +  geom_point() 

ggplot(data = clean_Age, aes(Age)) + geom_density(kernel = "gaussian") 

quantile(clean_Age$Age, probs = 0.25)


sum(is.na(train_numeric$Age))
sum(is.na(train_numeric$Embarked_reco))


str(train_numeric)

res <- cor(train_numeric) 
round(res, 2)