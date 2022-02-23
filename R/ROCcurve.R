#' ROC and PRC curves
#'
#' These are ROC and PRC curves
#'
#' @param y Vector with data for binary outcome
#' @param X Data matrix or vector of predictors
#' @param model Optional - provide logistic regression model
#' @param plot Logical
#' @param optPoint Logical
#' @param grid Logical
#' @param grid_lty Numeric
#' @param grid_lwd Numeric
#' @param grid_col Character
#' @param midline Logical
#' @param midline_lty Numeric
#' @param midline_lwd Numeric
#' @param midline_col Character
#' @param pt_pch Numeric
#' @param pt_border Character
#' @param pt_col Character
#' @param thresh Logical
#' @param roc_lty Numeric
#' @param roc_lwd Numeric
#' @param roc_col Character
#' @param prc Logical
#' @param plot_na Logical
#'
#' @return An ROCcurve object or plot
#' @export
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom graphics abline axis lines points
#' @importFrom methods is
#' @import stats
#'
#' @examples
#' roc <- ROCcurve(mtcars$vs, mtcars$mpg)
#' prc <- ROCcurve(mtcars$vs, mtcars$mpg, prc = TRUE)
ROCcurve <- function(y, X = NULL, model = NULL, plot = FALSE, optPoint = TRUE,
                     grid = FALSE, grid_lty = 3, grid_lwd = 1.5, grid_col = "lightgray",
                     midline = TRUE, midline_lty = 2, midline_lwd = 2, midline_col = "red",
                     pt_pch = 23, pt_border = "black", pt_col = "green", thresh = TRUE,
                     roc_lty = 1, roc_lwd = 2, roc_col = "black", prc = FALSE, plot_na = FALSE){
  stopifnot(all(sapply(list(prc, plot, plot_na), is.logical)))
  if(is(y, 'ROCcurve')){
    sens <- y$results$sens
    spec <- y$results$spec
    ppv <- y$results$ppv
    opt <- ifelse(prc, which.max(ppv + sens), which.max(sens + spec))
  } else {
    if(!missing(y) & is.null(model)){
      if(is(y, 'glm')){
        model <- y
        y <- unname(model$y)
      }
    } else if(!is.null(model) & missing(y)){
      y <- unname(model$y)
    }
    stopifnot(dim(table(y)) == 2)
    if(is.factor(y) | is.character(y)){
      y <- factor(y)
      levels(y) <- 0:1
      y <- as.numeric(as.character(y))
    }
    stopifnot(all(names(table(y)) %in% c("0", "1")))
    if(is.null(model)){
      stopifnot(!is.null(X))
      X <- as.data.frame(X)
      model <- glm(y ~ ., data = X, family = binomial)
      predProbs <- predict(model, type = "response")
    } else if(is(model, 'train')){
      predProbs <- predict(model, X, 'prob')[, 2]
    } else if(is(model, 'glinternet')){
      predProbs <- predict(model, X, type = 'response')[, 2]
    } else if(is(model, 'glm') | is(model, 'glmboost') | is(model, 'gbm')){
      predProbs <- predict(model, type = "response")
    } else if(is(model, 'numeric')){
      stopifnot(length(model) == length(y))
      predProbs <- model
    }
    p <- unname(sort(predProbs))
    if(thresh | prc){
      t1 <- (c(-Inf, p) + c(p, +Inf))/2
      t2 <- (c(-Inf, p)/2 + c(p, +Inf)/2)
      p <- ifelse(abs(t1) > 1e+100, t2, t1)
    }
    preds <- list()
    tp <- tn <- fp <- fn <- c()
    for(i in 1:length(p)){
      preds[[i]] <- ifelse(predProbs > p[i], 1, 0)
      Y <- cbind(y, preds[[i]])
      tn[i] <- sum(Y[Y[, 1] == 0, 1] == Y[Y[, 1] == 0, 2])
      tp[i] <- sum(Y[Y[, 1] == 1, 1] == Y[Y[, 1] == 1, 2])
      fn[i] <- sum(Y[Y[, 2] == 0, 1] != Y[Y[, 2] == 0, 2])
      fp[i] <- sum(Y[Y[, 2] == 1, 1] != Y[Y[, 2] == 1, 2])
    }
    sens <- tp/sum(y)
    spec <- tn/(length(y) - sum(y))
    npv <- tn/(tn + fn)
    ppv <- tp/(tp + fp)
    opt <- ifelse(prc, which.max(ppv + sens), which.max(sens + spec))
    optCut <- p[opt]
    optSens <- sens[opt]
    optSpec <- spec[opt]
    optPPV <- ppv[opt]
    optNPV <- npv[opt]
  }
  if(prc){
    sx <- c(1, sens)
    sy <- c(0, ppv)
    syna <- 0
    if(any(is.na(sy))){
      if(sum(is.na(sy)) == 1){
        sy <- na.omit(sy)
      } else {
        syna <- which(is.na(sy))
        sy[syna] <- sx[syna]
      }
    }
    syextra <- length(sy) < length(sx)
    if(syextra){sy <- c(sy, 1)}
    height <- sy[-1] - sy[-length(sy)]
    width <- (sx[-1] + sx[-length(sx)])/2
  } else {
    sx <- c(0, spec)
    sy <- c(1, sens)
    height <- (sy[-1] + sy[-length(sy)])/2
    width <- sx[-1] - sx[-length(sx)]
  }
  AUC <- sum(height * width)
  if(!plot){
    if(is(y, 'ROCcurve')){
      p <- y$results$cutoff
      npv <- y$results$npv
      optCut <- p[opt]
      optSens <- sens[opt]
      optSpec <- spec[opt]
      optPPV <- ppv[opt]
      optNPV <- npv[opt]
    }
    out <- list(results = data.frame(cutoff = p, sens = sens, spec = spec, ppv = ppv, npv = npv),
                optimal = unlist(list(cutoff = optCut, sensitivity = optSens,
                                      specificity = optSpec, PPV = optPPV,
                                      NPV = optNPV)), AUC = AUC)
    class(out) <- c('ROCcurve', 'list')
    attr(out, 'type') <- ifelse(prc, 'PRC', 'ROC')
    return(out)
  } else {
    xlabel <- ifelse(prc, 'Recall', '1 - Specificity')
    ylabel <- ifelse(prc, 'Precision', 'Sensitivity')
    main <- ifelse(prc, paste0('PR Curve\nAUC = ', round(AUC, 3)),
                   paste0('ROC Curve\nAUC = ', round(AUC, 3)))
    plot(0, 0, type = "n", ylim = c(0, 1), xlim = c(0, 1), axes = FALSE,
         xlab = xlabel, ylab = ylabel, main = main)
    if(grid != FALSE){
      if(grid == TRUE){grid <- 1}
      if(grid == 1){
        grid(NA, 5, lty = grid_lty, lwd = grid_lwd, col = grid_col)
      } else if(grid == 2){
        grid(lty = grid_lty, lwd = grid_lwd, col = grid_col)
      }
    }
    axis(1); axis(2)
    if(!plot_na & prc){
      if(syextra){
        sy <- sy[-length(sy)]
        sx <- sx[-length(sx)]
      }
      if(!identical(syna, 0)){
        sx[syna] <- NA
        sy[syna] <- NA
      }
    }
    if(prc){sx <- 1 - sx}
    lines(1 - sx, sy, lty = roc_lty, lwd = roc_lwd, col = roc_col)
    if(midline != FALSE){
      if(midline == TRUE){midline <- 1}
      if(midline == "grey" | midline == "gray"){midline_lty <- 1; midline_col = "grey"}
      if(midline == 2){midline_lty <- 1; midline_col = "grey"}
      prctrue <- as.numeric(prc)
      abline(a = prctrue, b = 1 - (2 * prctrue), lty = midline_lty,
             lwd = midline_lwd, col = midline_col)
    }
    if(optPoint != FALSE){
      if(optPoint == "black" | optPoint == 2){pt_pch <- 8; pt_col <- "black"}
      if(!pt_pch %in% c(21:25)){pt_border <- pt_col}
      points((1 - sx)[opt + 1], sy[opt + 1], pch = pt_pch, bg = pt_col, col = pt_border)
    }
  }
}


#' Plotting function for ROC and PRC curves
#'
#' @param x ROCcurve object
#' @param type Set to default or type of curve to plot
#' @param ... Additional arguments
#'
#' @return An ROC or PRC plot
#' @export
#'
#' @examples
#' roc <- ROCcurve(mtcars$vs, mtcars$mpg)
#' prc <- ROCcurve(mtcars$vs, mtcars$mpg, prc = TRUE)
#'
#' plot(roc)
#'
#' plot(prc)
plot.ROCcurve <- function(x, type = c('default', 'roc', 'prc'), ...){
  type <- match.arg(tolower(type), c('default', 'roc', 'prc'))
  if(identical(type, 'default')){
    prc <- isTRUE(attr(x, 'type') == 'PRC')
  } else {
    prc <- isTRUE(type == 'prc')
  }
  ROCcurve(y = x, plot = TRUE, prc = prc, ...)
}

#' Summarize optimal ROC and PRC results
#'
#' @param object ROCcurve object
#' @param type Indicate \code{"roc"}, \code{"prc"}, or both
#' @param ... Additional arguments
#'
#' @return The optimal parameter values for the ROC or PRC curve
#' @export
#'
#' @examples
#' roc <- ROCcurve(mtcars$vs, mtcars$mpg)
#' prc <- ROCcurve(mtcars$vs, mtcars$mpg, prc = TRUE)
#'
#' summary(roc)
summary.ROCcurve <- function(object, type = c('roc', 'prc'), ...){
  type <- match.arg(tolower(type), c('roc', 'prc'), several.ok = TRUE)
  if(length(type) == 1){
    obj <- ROCcurve(y = object, plot = FALSE, prc = isTRUE(type == 'prc'), ...)
    out <- data.frame(type = attr(obj, 'type'), AUC = obj$AUC, as.list(obj$optimal))
  } else {
    obj1 <- ROCcurve(y = object, plot = FALSE, prc = FALSE, ...)
    obj2 <- ROCcurve(y = object, plot = FALSE, prc = TRUE, ...)
    out1 <- data.frame(type = 'ROC', AUC = obj1$AUC, as.list(obj1$optimal))
    out2 <- data.frame(type = 'PRC', AUC = obj2$AUC, as.list(obj2$optimal))
    out <- rbind(out1, out2)
  }
  return(out)
}
