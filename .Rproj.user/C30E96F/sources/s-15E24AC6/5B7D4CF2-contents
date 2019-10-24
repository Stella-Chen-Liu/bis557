#' Ridge Regression
#'
#' @description This function performs ridge regression.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data an data frame containing the variables in the model.
#' @param lambda the hyperparameter of the ridge regression. Default value is 0.
#'
#' @return A matrix containing the regression coeffeicients with the regression formula as an attribute.
#'
#' @examples
#' data(iris)
#' scaled_iris <- as.data.frame(scale(iris[, -5], TRUE, TRUE))
#' fit.ridge_regression <- ridge_regression(Sepal.Length ~ .,
#' scaled_iris, lambda = 0.5)
#'
#' @importFrom stats model.matrix
#'
#' @export

ridge_regression <- function(formula, data, lambda = 0) {
  X <- model.matrix(formula, data)
  Y <- as.matrix(data[all.vars(formula)[1]])
  ret <- solve( t(X) %*% X + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
  attributes(ret)$formula <- formula
  class(ret) <- c(class(ret), "ridge_regression")
  return(ret)
}
