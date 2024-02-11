library(neuralnet)

# Binary classification
nn <- neuralnet(Species == "setosa" ~ Petal.Length + Petal.Width, iris, linear.output = FALSE)
print(nn)
plot(nn)

# Multiclass classification
nn <- neuralnet(Species ~ Petal.Length + Petal.Width, iris, linear.output = FALSE)
print(nn)
plot(nn)

# Custom activation function
softplus <- function(x) log(1 + exp(x))
nn <- neuralnet((Species == "setosa") ~ Petal.Length + Petal.Width, iris, 
                  linear.output = FALSE, hidden = c(3, 2), act.fct = softplus)
print(nn)
plot(nn)

##RT  
df <- data.frame(iris)
