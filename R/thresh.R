#' Find threshold scale value based on ROC or PRC
#'
#' @param x vector of predictor values
#' @param y vector of binary outcome values
#' @param prc Logical, determines whether to base threshold on PRC
#' @param scale.max largest value of the predictor, or leave as default
#' @param cutoff Character string. Define how to classify cases relative to the
#'   predicted probability cutoff
#' @param round how many decimal places to round the cutoff and predicted
#'   probabilities
#'
#' @return A value representing the largest value that would not be classified
#'   in the positive class
#' @export
#'
#' @examples
#' 1 + 1
thresh <- function(x, y, prc = FALSE, scale.max = 'default', cutoff = '>', round = 5){
  stopifnot(length(scale.max) == 1)
  if(identical(scale.max, 'default')){scale.max <- max(x)}
  values <- 0:scale.max
  dat <- data.frame(y, x)
  b <- coef(glm(y ~ x, dat, family = binomial))
  curve <- ROCcurve(y = y, X = x, plot = FALSE, prc = prc, cutoff = cutoff)
  predprobs <- function(v, b){
    pp <- b[1] + b[2] * v
    return(unname(exp(pp)/(1 + exp(pp))))
  }
  k <- round(curve$optimal[1], round)
  pp <- round(sapply(values, predprobs, b = b), round)
  out <- values[max(which(pp < k)) + 1]
  #if(any(pp == k)){
  #  out <- values[which(pp == k)]
  #} else {
  #  out <- values[max(which(pp < k)) + 1]
  #}
  names(out) <- paste0(ifelse(prc, 'PRC', 'ROC'), '_thresh')
  return(out)
}
