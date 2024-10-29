#' Load recruitment data
#'
#' @param data Main dataset
#' @param date Date column
#' @param enrolled Enrolled column
#'
#' @return NULL
#' @export
#'
#' @examples LoadData(gripsIM, ScreenDt, Enrolled)
LoadData <- function(data, date, enrolled) {
  if (is.null(data)) stop("data is NULL")
  if (!("data.frame" %in% class(data))) stop("data must be a dataframe")
  qReg <- "^(\"|\')(.+)(\"|\')$"
  dataStr = the$dataStr <- deparse(substitute(data))
  enStr = the$enStr <- deparse(substitute(enrolled)) |> gsub(qReg, "\\2", x = _)
  dtStr = the$dtStr <- deparse(substitute(date)) |> gsub(qReg, "\\2", x = _)
  date <- checkArgs(dataStr, dtStr) |> fixDate()
  enrolled <- checkArgs(dataStr, enStr) |> fixEnrolled()
  the$raw = data.frame(date, enrolled)
  the$datWeeks <- days2weeks()
  the$TrainVector <- the$datWeeks$enrolled
  the$Trainfilled <- fillGaps(the$TrainVector)
  the$TrainVectorN <- setNames(the$TrainVector, the$datWeeks$week)
  the$TrainfilledN <- setNames(the$Trainfilled, the$datWeeks$week)
  enStr = fmt(enStr, 28, 0, 1)
  dtStr = fmt(dtStr, 28, 0, 1)
  msg = sprintf("'%s' and '%s' were successfully loaded", enStr, dtStr)
  cat(msg)
}

#' Title Simulate number of weeks needed to recruit a given number of subjects
#'
#' @param nSub Number of subjects to recruit
#' @param fillGaps Whether to fill gaps in the data
#' @param nSim Number of simulations to run
#' @param startWeek The week of the year to start from
#' @return A list with two elements. The first element `weeks` is an integer
#'     vector with length equal to `nSim` containing the results of the
#'     simulation. The second `CI` shows the median and the 95%CI.
#' @export
#' @examples
#' LoadData(gripsIM, ScreenDt, Enrolled)
#' simAllWt(50L)
simAllWt <- function(nSub = 50L, fill_Gaps = FALSE, nSim = 1e4L, startWeek = 1L) {
  if (is.null(the$TrainVector)) stop("TrainVector not loaded")
  the$finalVector =  if (fill_Gaps) the$Trainfilled else the$TrainVector
  weeks <- vapply(seq.int(nSim), function(x) sim1wt1(nSub, startWeek), 0L)
  CI <- stats::quantile(x = weeks, probs = c(.025, .5, .975))
  list(weeks = weeks, CI = CI)
}
