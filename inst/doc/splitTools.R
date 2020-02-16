## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 6
)

## -----------------------------------------------------------------------------
library(splitTools)
library(ranger)

# Split data into partitions
set.seed(3451)
inds <- partition(iris$Sepal.Length, p = c(train = 0.6, valid = 0.2, test = 0.2))
str(inds)

train <- iris[inds$train, ]
valid <- iris[inds$valid, ]
test <- iris[inds$test, ]

# Root-mean-squared error function used to evaluate results
rmse <- function(y, pred) {
  sqrt(mean((y - pred)^2))
}

# Tune mtry on validation data
valid_mtry <- numeric(ncol(train) - 1)

for (i in seq_along(valid_mtry)) {
  fit <- ranger(Sepal.Length ~ ., data = train, mtry = i)
  valid_mtry[i] <- rmse(valid$Sepal.Length, predict(fit, valid)$predictions)
}

valid_mtry
(best_mtry <- which.min(valid_mtry))

# Fit and test final model
final_fit <- ranger(Sepal.Length ~ ., data = train, mtry = best_mtry)
rmse(test$Sepal.Length, predict(final_fit, test)$predictions)

## -----------------------------------------------------------------------------
# Split into training and test
inds <- partition(iris$Sepal.Length, p = c(train = 0.8, test = 0.2))

train <- iris[inds$train, ]
test <- iris[inds$test, ]

# Get stratified cross-validation fold indices
folds <- create_folds(train$Sepal.Length, k = 5)

# Tune mtry by GridSearchCV
valid_mtry <- numeric(ncol(train) - 1)

for (i in seq_along(valid_mtry)) {
  cv_mtry <- numeric()
  for (fold in folds) {
    fit <- ranger(Sepal.Length ~ ., data = train[fold, ], mtry = i)
    cv_mtry <- c(cv_mtry, 
                 rmse(train[-fold, "Sepal.Length"], predict(fit, train[-fold, ])$predictions))
  }
  valid_mtry[i] <- mean(cv_mtry)
}

# Result of cross-validation
valid_mtry
(best_mtry <- which.min(valid_mtry))

# Use optimal mtry to make model
final_fit <- ranger(Sepal.Length ~ ., data = train, mtry = best_mtry)
rmse(test$Sepal.Length, predict(final_fit, test)$predictions)

## -----------------------------------------------------------------------------
# We start by making repeated, stratified cross-validation folds
folds <- create_folds(train$Sepal.Length, k = 5, m_rep = 3)
length(folds)

for (i in seq_along(valid_mtry)) {
  cv_mtry <- numeric()
  for (fold in folds) {
    fit <- ranger(Sepal.Length ~ ., data = train[fold, ], mtry = i)
    cv_mtry <- c(cv_mtry, 
                 rmse(train[-fold, "Sepal.Length"], predict(fit, train[-fold, ])$predictions))
  }
  valid_mtry[i] <- mean(cv_mtry)
}

# Result of cross-validation
valid_mtry
(best_mtry <- which.min(valid_mtry))

# Use optimal mtry to make model
final_fit <- ranger(Sepal.Length ~ ., data = train, mtry = best_mtry)
rmse(test$Sepal.Length, predict(final_fit, test)$predictions)

## -----------------------------------------------------------------------------
# Create data
set.seed(452)
n <- 1000
t <- seq(0, 2 * pi, length.out = n)
y <- 0.2 * sin(t) - 0.1 * cos(t) + 0.2 * runif(n)
plot(y ~ t, pch = ".", cex = 2)

# Helper function
Lag <- function(z, k = 1) {
  c(z[-seq_len(k)], rep(NA, k))
}
Lag(1:4, k = 1)

# Add lagged features
dat <- data.frame(y, 
                  lag1 = Lag(y), 
                  lag2 = Lag(y, k = 2), 
                  lag3 = Lag(y, k = 3))
dat <- dat[complete.cases(dat), ]
head(dat)
cor(dat)

# Block partitioning
inds <- partition(dat$y, p = c(train = 0.9, test = 0.1), type = "blocked")
str(inds)

train <- dat[inds$train, ]
test <- dat[inds$test, ]

# Get time series folds
folds <- create_timefolds(train$y, k = 5)
str(folds)

# Tune mtry by GridSearchCV
valid_mtry <- numeric(ncol(train) - 1)

for (i in seq_along(valid_mtry)) {
  cv_mtry <- numeric()
  for (fold in folds) {
    fit <- ranger(y ~ ., data = train[fold$insample, ], mtry = i)
    cv_mtry <- c(cv_mtry, 
                 rmse(train[fold$outsample, "y"], 
                      predict(fit, train[fold$outsample, ])$predictions))
  }
  valid_mtry[i] <- mean(cv_mtry)
}

# Result of cross-validation
valid_mtry
(best_mtry <- which.min(valid_mtry))

# Use optimal mtry to make model and evaluate on future test data
final_fit <- ranger(y ~ ., data = train, mtry = best_mtry)
test_pred <- predict(final_fit, test)$predictions
rmse(test$y, test_pred)

# Plot
x <- seq_along(dat$y)
plot(x, dat$y, pch = ".", cex = 2)
points(tail(x, length(test$y)), test$y, col = "red", pch = ".", cex = 2)
lines(tail(x, length(test$y)), test_pred)



