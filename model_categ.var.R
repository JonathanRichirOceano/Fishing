install.packages("nnet")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")

library(nnet)
library(rpart)
library(rpart.plot)
library(randomForest)

# Example dataset
data <- data.frame(
  response = factor(sample(c("Yes", "No"), 100, replace = TRUE)),
  predictor1 = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
  predictor2 = factor(sample(c("X", "Y", "Z"), 100, replace = TRUE))
)

# Logistic regression
logit_model <- glm(response ~ predictor1 + predictor2, data = data, family = binomial)
summary(logit_model)

# Multinomial logistic regression
multinom_model <- multinom(response ~ predictor1 + predictor2, data = data)
summary(multinom_model)

# Decision tree
tree_model <- rpart(response ~ predictor1 + predictor2, data = data, method = "class")
rpart.plot(tree_model)

# Random forest
rf_model <- randomForest(response ~ predictor1 + predictor2, data = data)
print(rf_model)
