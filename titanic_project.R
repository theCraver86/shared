#install.packages("rio")
#install.packages("neuralnet")
library(rio)
library(dplyr)
library(ggplot2)
library(writexl)
library(neuralnet)

#install_formats()

test <- import("C:/Users/rtadd/OneDrive/Desktop/R/3titanic/0_import/test.csv")
train <- import("C:/Users/rtadd/OneDrive/Desktop/R/3titanic/0_import/train.csv")
#gender_submission <- import("C:/Users/rtadd/OneDrive/Desktop/R/3titanic/0_import/gender_submission.csv")

str(train)

test_numeric <- test %>%
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
  select(-c(Name, Sex, Embarked, Cabin, Ticket)) %>% 
  mutate(Fare =ifelse(is.na(Fare) == TRUE, mean(Fare, na.rm = TRUE), Fare))

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
#  mutate_if(is.numeric, funs(as.numeric(scale(.))))

res <- cor(train_numeric) 
res <- round(res, 2)

nn <- neuralnet(Survived == 1 ~ Age + Sex_reco + Pclass, train_numeric, linear.output = FALSE)
print(nn)
plot(nn)

model <- lm(Survived ~ Age + Sex_reco + Pclass, data = train_numeric)
summary(model)
summary(model)$coefficients

log_model <- glm(Survived ~ 
                  #Sex_reco + Pclass,
                 Age + Sex_reco + Pclass + SibSp + Embarked_reco,
                data = train_numeric, family = "binomial")
                #data = train_numeric)
summary(log_model)
summary(log_model)$coefficients

chi <- log_model[["null.deviance"]] - log_model[["deviance"]]

post_logit <- predict(log_model, type = "response")
head(post_logit)

default_logit <- ifelse(post_logit>0.5, "PreV: 1", "Prev: 0")
calc <- table(default_logit, train_numeric$Survived)

perc <- round(((calc[1,1] + calc[2,2])  / (calc[1,1] + calc[1,2] + calc[2,1] + calc[2,2])*100),2)
perc

### Output
output_post_logit <- predict(log_model, 
                             newdata = test_numeric, se.fit = TRUE)
#newdata = temperature = c(70, 90), se.fit = TRUE)
head(output_post_logit)

output <- ifelse(output_post_logit$fit>0.5, 1, 0)

write_xlsx(data.frame(output), "C:/Users/rtadd/OneDrive/Desktop/R/3titanic/0_export/output.xlsx")


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

