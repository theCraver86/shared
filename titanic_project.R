#install.packages("rio")
#install.packages("neuralnet")
library(rio)
library(dplyr)
library(ggplot2)
library(writexl)
library(neuralnet)

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
  mutate(Age =ifelse(is.na(Age) == TRUE, mean(Age, na.rm = TRUE), Age)) %>%  
  mutate(Embarked_reco = ifelse(is.na(Embarked_reco) == TRUE, 3, Embarked_reco)) %>% 
  select(-c(Name, Sex, Embarked, Cabin, Ticket))

res <- cor(train_numeric) 
res <- round(res, 2)

nn <- neuralnet(Survived == 1 ~ Age + Sex_reco + Pclass, train_numeric, linear.output = FALSE)
print(nn)
plot(nn)

model <- lm(Survived ~ Age + Sex_reco + Pclass, data = train_numeric)
summary(model)
summary(model)$coefficients

log_model <- glm(Survived ~ Age + Sex_reco + Pclass + SibSp + Parch + Fare + Embarked_reco, data = train_numeric, family = "binomial")
summary(log_model)
summary(log_model)$coefficients


str(train_numeric)

### PAINT
clean_Age <- train_numeric %>% 
  select(c(PassengerId, Age)) %>% 
  mutate(Age =ifelse(is.na(Age) == TRUE, mean(Age, na.rm = TRUE), Age))

ggplot(data = clean_Age, aes(x = PassengerId, y = Age)) +  geom_point() 
ggplot(data = clean_Age, aes(Age)) + geom_density(kernel = "gaussian") 

### EXPERIMENT
quantile(clean_Age$Age, probs = 0.25)
sum(is.na(train_numeric$Age))
table(train$Embarked)

### EXPORT

out = data.frame(res);

write_xlsx(out, "C:/Users/rtadd/OneDrive/Desktop/R/3titanic/0_export/out.xlsx")

