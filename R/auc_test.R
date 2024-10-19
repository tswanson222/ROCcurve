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
#' @param prc Logical, determines whether to fit PR or ROC curves. If set to
#'   \code{"both"}, then the AUC test will be performed for both ROC and PR
#'   curves
#' @param stratified Logical. Determines whether to retain class proportions
#'   during bootstrapped resampling
#' @param verbose Logical. Determines whether to show progress bar
#' @param alternative Character, determines what kind of alternative hypothesis
#' @param cutoff Character string. Define how to classify cases relative to the
#'   predicted probability cutoff
#' @param FUN1 Model function that takes "data", "y", and "x1" as arguments 
#'   and returns a vector of predicted probabilities.
#' @param FUN2 Model function that takes "data", "y", and "x2" as arguments
#'   and returns a vector of predicted probabilities. If "x2" is missing, 
#'   "x1" will be used to define it.
#'
#'
#' @return D-statistic and associated p-value
#' @export
#'
#' @examples
#' auc_test(mtcars, 'am', 'mpg', 'disp', nboot = 100, seed = 91)
auc_test <- function(data, y, x1, x2, nboot = 1000, seed = NULL,
                     prc = FALSE, stratified = TRUE, verbose = TRUE,
                     alternative = c('two.sided', 'less', 'greater'),
                     cutoff = '>', FUN1 = NULL, FUN2 = NULL){
  if(is(data, 'list')){
    call <- as.list(match.call())[-1]
    out <- do.call(rbind, lapply(data, function(i){
      calli <- replace(call, 'data', list(i))
      do.call(auc_test, calli)
    }))
    if(!is.null(names(data)) & !identical(prc, 'both')){
        rownames(out) <- names(data)
    }
    return(out)
  } else if(identical(prc, 'both')){
    call <- as.list(match.call())[-1]
    out <- do.call(rbind, lapply(c(FALSE, TRUE), function(i){
      calli <- replace(call, 'prc', i)
      do.call(auc_test, calli)
    }))
    return(out)
  }
  if(!is.null(FUN2) & missing(x2)){
    x2 <- x1
  }
  stopifnot(is.character(y) & is.character(x1) & is.character(x2))
  stopifnot(length(unique(data[, y])) == 2)
  alternative <- match.arg(alternative)
  if(!is.null(FUN1)){
    preds1 <- FUN1(data = data, y = y, x1 = x1)
    auc1 <- ROCcurve(data[, y], preds = preds1, plot = FALSE, prc = prc, cutoff = cutoff)$AUC
  } else {
    auc1 <- ROCcurve(data[, y], data[, x1], plot = FALSE, prc = prc, cutoff = cutoff)$AUC
  }
  if(!is.null(FUN2)){
    preds2 <- FUN2(data = data, y = y, x2 = x2)
    auc2 <- ROCcurve(data[, y], preds = preds2, plot = FALSE, prc = prc, cutoff = cutoff)$AUC
  } else {
    auc2 <- ROCcurve(data[, y], data[, x2], plot = FALSE, prc = prc, cutoff = cutoff)$AUC
  }
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
    if(!is.null(FUN1)){
      p1 <- FUN1(data = dati, y = y, x1 = x1)
      a1 <- ROCcurve(dati[, y], preds = p1, plot = FALSE, prc = prc, cutoff = cutoff)$AUC
    } else {
      a1 <- ROCcurve(dati[, y], dati[, x1], plot = FALSE, prc = prc, cutoff = cutoff)$AUC
    }
    if(!is.null(FUN2)){
      p2 <- FUN2(data = dati, y = y, x2 = x2)
      a2 <- ROCcurve(dati[, y], preds = p2, plot = FALSE, prc = prc, cutoff = cutoff)$AUC
    } else {
      a2 <- ROCcurve(dati[, y], dati[, x2], plot = FALSE, prc = prc, cutoff = cutoff)$AUC
    }
    if(verbose){setTxtProgressBar(pb, i)}
    return(a1 - a2)
  })
  d <- (auc1 - auc2)/sd(auc_diffs)
  if(alternative == 'two.sided'){
    p <- pnorm(abs(d), lower.tail = FALSE) * 2
  } else {
    p <- pnorm(d, lower.tail = isTRUE(alternative == 'less'))
  }
  out <- data.frame(type = ifelse(prc, 'PRC', 'ROC'), auc1, auc2,
                    sd_diff = sd(auc_diffs), D = d, pvalue = p)
  attributes(out)[c('nboot', 'stratified', 'alternative')] <- list(nboot, stratified, alternative)
  attr(out, 'auc_diffs') <- auc_diffs
  if(length(x1) == 1 & length(x2) == 1){
    colnames(out)[startsWith(colnames(out), 'auc')] <- c(x1, x2)
  }
  return(out)
}
