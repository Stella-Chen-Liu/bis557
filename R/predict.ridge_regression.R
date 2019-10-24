#' @importFrom stats model.matrix
predict.ridge_regression <- function(object, testX) {
  X <- model.matrix(attributes(object)$formula, testX)
  X %*% object
}

predict_err <- function(y, y_hat){
  sqrt(mean((y - y_hat)^2))
}
