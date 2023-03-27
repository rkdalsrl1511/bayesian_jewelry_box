#' Returns the Bayes factor and proceeds with hypothesis testing.
#' @title bayesian hypothesis test
#' @param W predictor matrix
#' @param z response variance
#' @param iteration Number of iterations
#' @param a,b rejection sampler parameter
#' @param s xi's hyperparameter
#' @param time_check Output run time
#' @param iteration_check Output iteration
#' @param beta the initial value of beta
#' @param xi the initial value of Global shrinkage parameter
#' @param Sigma the inial value of Sigma
#' @param w Sigma's hyperparameter
#' @return posterior samples
#' @importFrom MASS mvrnorm
#' @importFrom invgamma rinvgamma
#' @export
bjb_hypothesis_test <- function(distribution){

  # 어떻게 해야 최대한 쉽게 자동화할 수 있을까?











}
