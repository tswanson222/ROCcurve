#' Compare two AUC values from ROC or PRC curves
#'
#' @param data dataset to fit ROC or PRC curves to
#' @param y Character string name of outcome
#' @param x1 Character string or vector containing predictor name(s) for first
#'   model
#' @param x2 Character string or vector containing predictor name(s) for second
#'   model
#' @param nboot Number of bootstrap iterations
#' @param seed Numeric value for seed
#' @param stratified Logical. Determines whether to retain class proportions
#'   during bootstrapped resampling
#' @param verbose Logical. Determines whether to show progress bar
#' @param ... Additional argument for \code{ROCcurve}
#'
#' @return D-statistic and associated p-value
#' @export
#'
#' @examples
#' 1 + 1
auc_test <- function(data, y, x1, x2, nboot = 1000, seed = NULL,
                     stratified = TRUE, verbose = TRUE, ...){
  stopifnot(length(y) == 1)
  auc1 <- ROCcurve(data[, y], data[, x1], plot = FALSE, ...)$AUC
  auc2 <- ROCcurve(data[, y], data[, x2], plot = FALSE, ...)$AUC
  if(!is.null(seed)){set.seed(seed)}
  if(verbose){pb <- txtProgressBar(max = nboot, style = 3)}
  auc_diffs <- sapply(1:nboot, function(i){
    if(stratified){
      k <- unique(data[, y])
      y0 <- data[which(data[, y] == k[1]), ]
      y1 <- data[which(data[, y] == k[2]), ]
      dati <- rbind(y0[sample(1:nrow(y0), replace = TRUE), ],
                    y1[sample(1:nrow(y1), replace = TRUE), ])
    } else {
      dati <- data[sample(1:nrow(data), replace = TRUE), ]
    }
    a1 <- ROCcurve(dati[, y], dati[, x1], ...)$AUC
    a2 <- ROCcurve(dati[, y], dati[, x2], ...)$AUC
    if(verbose){setTxtProgressBar(pb, i)}
    return(a1 - a2)
  })
  d <- (auc1 - auc2)/sd(auc_diffs)
  p <- pnorm(abs(d), lower.tail = FALSE) * 2
  out <- c(D = d, pvalue = p)
  return(out)
}
