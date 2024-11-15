#' Function: Load recruitment dataset
#' @param data Main dataset
#' @param date Date column
#' @param enrolled Enrolled column
#' @return NULL
#' @export
#' @examples LoadData(gripsIM, ScreenDt, Enrolled)
LoadData <- function(data, date, enrolled) {
  if (is.null(data)) stop("data is NULL")
  if (!("data.frame" %in% class(data))) stop("data must be a dataframe")
  cargs <- getCall();
  date <- checkArgs(date)
  enrolled <- checkArgs(enrolled)
  the$raw <- data.frame(date, enrolled)
  the$datWeeks <- days2weeks(date, enrolled);
  the$train = the$TrainVector <- the$datWeeks$enrolled
  the$Trainfilled <- fillEmptyWeeks(the$TrainVector, the$datWeeks$cnt)
  the$cppModule <- methods::new(rct, the);
  exportModuleMethods(the$cppModule)
  LoadSuccess(cargs)
}

#' Function: Simulate number of weeks needed to recruit a given number of
#'     subjects
#' @param nSub Number of subjects to recruit (default = 50)
#' @param nSim Number of simulations to run (default = 1e4)
#' @param fillGaps Whether to fill gaps in the data (default = FALSE)
#' @param cauchyWt Whether to use Cauchy weights for sampling (default = FALSE).
#'    If FALSE, binomial weights are used. 
#' @param coeff A coefficient to apply to the recruitment rate (default = 1)
#' @return A list with two elements. The first element `weeks` is an integer
#'     vector with length equal to `nSim` containing the results of the
#'     simulation. The second `CI` shows the median and the 95%CI.
#' @export
#' @examples
#' LoadData(gripsIM, ScreenDt, Enrolled)
#' res <- time2Nsubjects()
time2Nsubjects <- \(nSub = 50, nSim = 1e4, fillGaps = FALSE, cauchyWt = FALSE,
                    coeff = 1) {
  checkExportedFunctionsArgs()
  useFilled(fillGaps)
  the$useCauchy(cauchyWt)
  applyCoeff(coeff)
  out <- the$weeks2Nsubjects(nSim, nSub)
  log(msgL$enrollWeeks, bold(nSub, 28), bold(out$CI[[2L]], 28))
  print(round(out$CI))
  invisible(out)
}
#' Function: Calculate CI of Euclidean distance of predicted recruitment with
#'     actual recruitment
#' @param target A vector with the actual recruitment by week
#' @param fillGaps Whether to fill gaps in the data
#' @param nSim Number of simulations to run
#' @return A list with two elements. The first element `dist` is a numeric
#'     vector with length equal to `nSim` containing the simulated Euclidean
#'     distance. The second `CI` shows the median and the 95%CI Euclidean 
#'     distance.
#' @export
#' @examples
#' LoadData(gripsIM, ScreenDt, Enrolled)
#' target <- days2weeks(gripsYR2$ScreenDt, gripsYR2$Enrolled)$enrolled
#' res <- simDistance(target)
simDistance <- function(target, fillGaps = FALSE, nSim = 1e4L) {
  if (is.null(the$TrainVector)) stop("TrainVector not loaded")
  the$train <- if (fillGaps) the$Trainfilled else the$TrainVector
  len <- length(the$train)
  if (length(target) < len) stop("target is smaller")
  if (length(target) > len) target <- target[seq.int(len)]
  target <- cumsum(target);
  dummyFun <- \(x) getDistance(the$train, target, the$probs)
  dist <- vapply(seq.int(nSim), dummyFun , numeric(1L), USE.NAMES = FALSE)
  CI <- stats::quantile(x = dist, probs = c(.025, .5, .975))
  print(round(CI))
  invisible(list(dist = dist, CI = CI))
}

#' Function: Calculate median recruitment with CI for the next 52 weeks
#' @param nSim Number of simulations to run
#' @param fillGaps Whether to fill gaps in the data
#' @param cauchyWt Whether to use Cauchy weights for sampling (default = FALSE).
#'    If FALSE, binomial weights are used. 
#' @param coeff A coefficient to apply to the recruitment rate (default = 1)
#' @return An 52x3 matrix with the 2.5%, 50% and 97.5% percentiles for each week  
#' @export
#' @examples
#' LoadData(gripsIM, ScreenDt, Enrolled)
#' getWeeksPredCI()
getWeeksPredCI <- \(nSim = 1e4L, fillGaps = FALSE, cauchyWt = FALSE, coeff = 1) {
  checkExportedFunctionsArgs()
  useFilled(fillGaps)
  the$useCauchy(cauchyWt)
  applyCoeff(coeff)
  out <- the$PredCIbyWk(nSim) |> rbind(rep(0, 3), ... = _)
  rownames(out) <- 0:(nrow(out) - 1)
  utils::head(out) |> logPrint();
  utils::tail(out) |> logPrint();
  invisible(out);
}



