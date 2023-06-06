## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library(splitTools)

# Split data into partitions
set.seed(3451)
inds <- partition(iris$Sepal.Length, p = c(train = 0.6, valid = 0.2, test = 0.2))
str(inds)

train <- iris[inds$train, ]
valid <- iris[inds$valid, ]
test <- iris[inds$test, ]

rmse <- function(y, pred) {
  sqrt(mean((y - pred)^2))
}

# Use simple validation to decide on interaction yes/no...
fit1 <- lm(Sepal.Length ~ ., data = train)
fit2 <- lm(Sepal.Length ~ . + Species:Sepal.Width, data = train)

rmse(valid$Sepal.Length, predict(fit1, valid))
rmse(valid$Sepal.Length, predict(fit2, valid))

# Yes! Choose and test final model
rmse(test$Sepal.Length, predict(fit2, test))

## -----------------------------------------------------------------------------
# Split into training and test
inds <- partition(iris$Sepal.Length, p = c(train = 0.8, test = 0.2), seed = 87)

train <- iris[inds$train, ]
test <- iris[inds$test, ]

# Get stratified CV in-sample indices
folds <- create_folds(train$Sepal.Length, k = 5, seed = 2734)

# Vectors with results per model and fold
cv_rmse1 <- cv_rmse2 <- numeric(5)

for (i in seq_along(folds)) {
  insample <- train[folds[[i]], ]
  out <- train[-folds[[i]], ]
  
  fit1 <- lm(Sepal.Length ~ ., data = insample)
  fit2 <- lm(Sepal.Length ~ . + Species:Sepal.Width, data = insample)
  
  cv_rmse1[i] <- rmse(out$Sepal.Length, predict(fit1, out))
  cv_rmse2[i] <- rmse(out$Sepal.Length, predict(fit2, out))
}

# CV-RMSE of model 1 -> close winner
mean(cv_rmse1)

# CV-RMSE of model 2
mean(cv_rmse2)

# Fit model 1 on full training data and evaluate on test data
final_fit <- lm(Sepal.Length ~ ., data = train)
rmse(test$Sepal.Length, predict(final_fit, test))

## -----------------------------------------------------------------------------
# Train/test split as before

# 15 folds instead of 5
folds <- create_folds(train$Sepal.Length, k = 5, seed = 2734, m_rep = 3)
cv_rmse1 <- cv_rmse2 <- numeric(15)

# Rest as before...
for (i in seq_along(folds)) {
  insample <- train[folds[[i]], ]
  out <- train[-folds[[i]], ]
  
  fit1 <- lm(Sepal.Length ~ ., data = insample)
  fit2 <- lm(Sepal.Length ~ . + Species:Sepal.Width, data = insample)
  
  cv_rmse1[i] <- rmse(out$Sepal.Length, predict(fit1, out))
  cv_rmse2[i] <- rmse(out$Sepal.Length, predict(fit2, out))
}

mean(cv_rmse1)
mean(cv_rmse2)

# Refit and test as before

## -----------------------------------------------------------------------------
set.seed(3451)

ir <- iris[c("Sepal.Length", "Species")]
y <- multi_strata(ir, k = 5)
inds <- partition(
  y, p = c(train = 0.6, valid = 0.2, test = 0.2), split_into_list = FALSE
)

# Check
by(ir, inds, summary)

