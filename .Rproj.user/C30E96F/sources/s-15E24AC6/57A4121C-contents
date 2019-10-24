#' Optimize Lambda for Ridge Regression
#'
#' @description This function finds the optimial hyperparameter lambda for ridge regression.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data an data frame containing the variables in the model.
#' @param lambda_vals the search range of lambda values. The default value is a sequence from 0 to 1 by 0.005.
#' @param k the number of folds used in the k-fold cross validation. The default value is 10.
#'
#' @return The optimal lambda that gives minimum prediction error using cross validation.
#'
#' @examples
#' data(iris)
#' scaled_iris <- as.data.frame(scale(iris[, -5], TRUE, TRUE))
#' opt_lambda <- optimize_lambda(Sepal.Length ~ ., scaled_iris,
#'       lambda_vals = seq(0, 1, by = 0.05), k = 10)
#'
#' @importFrom caret createFolds
#' @importFrom stats model.matrix
#' @importFrom stats predict
#' @export

optimize_lambda <- function(formula, data, lambda_vals = seq(0, 1, by = 0.005), k = 10){
  folds <- createFolds(1:nrow(data), k = k)
  pred_err <- NULL
  for (j in 1:length(lambda_vals)){
    err <- 0
    for (i in 1:k){
      testid <- folds[[i]]
      trainid <- setdiff(1:nrow(data), testid)
      train_set <- data[trainid, ]
      test_set <- data[testid, ]
      fit.ridge <- ridge_regression(formula, train_set, lambda_vals[j])
      y_hat <- predict(fit.ridge, test_set)
      y <- as.matrix(test_set[all.vars(formula)[1]])
      err <- err + predict_err(y, y_hat)
    }
    pred_err <- c(pred_err, err)
  }
  optim_id <- which.min(pred_err)
  lambda_vals[optim_id]
}
