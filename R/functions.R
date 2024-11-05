#' Function: Load recruitment data
#' @param data Main dataset
#' @param date Date column
#' @param enrolled Enrolled column
#' @return NULL
#' @export
#' @examples LoadData(gripsIM, ScreenDt, Enrolled)
LoadData <- function(data, date, enrolled) {
  if (is.null(data)) stop("data is NULL")
  if (!("data.frame" %in% class(data))) stop("data must be a dataframe")
  qReg <- "^(\"|\')(.+)(\"|\')$"
  dataStr <- the$dataStr <- deparse(substitute(data))
  the$enStr <- deparse(substitute(enrolled)) |> gsub(qReg, "\\2", x = _)
  the$dtStr <- deparse(substitute(date)) |> gsub(qReg, "\\2", x = _)
  date <- checkArgs(dataStr, the$dtStr) |> fixDate()
  enrolled <- checkArgs(dataStr, the$enStr) |> fixEnrolled()
  the$raw <- data.frame(date, enrolled)
  the$datWeeks <- days2weeks(date, enrolled);
  the$TrainVector <- the$datWeeks$enrolled
  the$Trainfilled <- fillGaps(the$TrainVector, the$datWeeks$cnt)
  the$TrainVectorN <- stats::setNames(the$TrainVector, the$datWeeks$week)
  the$TrainfilledN <- stats::setNames(the$Trainfilled, the$datWeeks$week)
  enStr <- fmt(the$enStr, 28, 0, 1)
  dtStr <- fmt(the$dtStr, 28, 0, 1)
  log("\n%s and %s were successfully loaded", enStr, dtStr)
}

#' Function: Simulate number of weeks needed to recruit a given number of
#'     subjects
#' @param nSub Number of subjects to recruit
#' @param fill_gaps Whether to fill gaps in the data
#' @param nSim Number of simulations to run
#' @param startWK The week of the year to start from
#' @return A list with two elements. The first element `weeks` is an integer
#'     vector with length equal to `nSim` containing the results of the
#'     simulation. The second `CI` shows the median and the 95%CI.
#' @export
#' @examples
#' LoadData(gripsIM, ScreenDt, Enrolled)
#' res <- simAllWt(50L)
simAllWt <- function(nSub = 50L, fill_gaps = FALSE, nSim = 1e4L, startWK = 1L) {
  if (is.null(the$TrainVector)) stop("TrainVector not loaded")
  the$finalVector <- if (fill_gaps) the$Trainfilled else the$TrainVector
  weeks <- vapply(seq.int(nSim), function(x) sim1wt1(nSub, startWK), 0L)
  CI <- stats::quantile(x = weeks, probs = c(.025, .5, .975))
  n <- fmt(nSub, 28, 0, 1)
  m <- fmt(CI[[2L]], 28, 0, 1)
  log("Enrolling %s subjects requires %s weeks\n\n", n, m)
  print(round(CI))
  invisible(list(weeks = weeks, CI = CI))
}
#' Function: Calculate CI of Euclidean distance of predicted recruitment with
#'     actual recruitment
#' @param target A vector with the actual recruitment by week
#' @param fill_gaps Whether to fill gaps in the data
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
simDistance <- function(target, fill_gaps = FALSE, nSim = 1e4L) {
  if (is.null(the$TrainVector)) stop("TrainVector not loaded")
  the$finalVector <- if (fill_gaps) the$Trainfilled else the$TrainVector
  len <- length(the$finalVector)
  if (length(target) < len) stop("target is smaller")
  if (length(target) > len) target <- target[seq.int(len)]
  target <- cumsum(target);
  dummyFun <- \(x) getDistance(the$finalVector, target, the$probs)
  dist <- vapply(seq.int(nSim), dummyFun , numeric(1L), USE.NAMES = FALSE)
  CI <- stats::quantile(x = dist, probs = c(.025, .5, .975))
  print(round(CI))
  invisible(list(dist = dist, CI = CI))
}

#' Function: Calculate median recruitment with CI for the next 52 weeks
#' @param fill_gaps Whether to fill gaps in the data
#' @param nSim Number of simulations to run
#' @return A 52x3 matrix with the 2.5%, 50% and 97.5% percentiles for each week  
#' @export
#' @examples
#' LoadData(gripsIM, ScreenDt, Enrolled)
#' getWeeksPredCI()
getWeeksPredCI <- function(nSim = 1e4L, fill_gaps = FALSE) {
  if (is.null(the$TrainVector)) stop("TrainVector not loaded")
  vec = the$finalVector <- if (fill_gaps) the$Trainfilled else the$TrainVector
  out = PredCIbyWk(vec, the$probs, nSim, c(0.025, 0.5, 0.975))
  round(do.call(rbind, out))
}




