desc <- "Accurate prediction of subject recruitment for randomized clinical 
trials (RCT) remains anongoing challenge. Previous prediction models rely on 
parametric assumptions. We present functions for non-parametric RCT recruitment 
prediction under several scenarios."

def <- use_description_defaults("RCTrecruit", fields = list(
  Title = "Non-parametric Recruitment Prediction for Randomized Clinical Trials",
  Description = gsub("\\n", "", desc),
  lazyData = TRUE,
  Version = "0.1.0"
))

use_description(def)
use_mit_license()
use_package("Rcpp")
use_package("Rcpp", type = "LinkingTo")
use_package("tis")
use_package("lubridate")

aut <- c(
  getOption("usethis.description")[[1]],
  person("Alejandro", "Villasante-Tezanos", email = "alvillas@utmb.edu", 
         role = c("aut")),
  person("Christopher", "Kurinec", email = "chkurine@utmb.edu", role = c("aut")),
  person("Xiaoying", "Yu", email = "xiyu@utmb.edu", role = c("aut"))
)
m <- read.dcf("DESCRIPTION") |> as.data.frame() |> lapply(\(x) gsub("\\n", "", x))
m$`Authors@R` <- aut


use_description(m)
the$datWeeks


LoadData(gripsIM, ScreenDt, Enrolled);
datWeeks <- the$datWeeks

labs <- list(
  week = "Calendar week",
  year = "Calendar year",
  enrolled = "Number of people enrolled that week",
  holiday = "Number of federal holidays that week",
  cnt = "Number of days in that week when recruitment was active"
)

for (x in names(datWeeks)) {
  attr(datWeeks[[x]], "Description") <- labs[[x]]
}


dpath <- system.file("DESCRIPTION", package = "accrual")

read.dcf(dpath) |> as.data.frame() |> lapply(\(x) gsub("\\n", "", x))


# library(tis)
library(lubridate)

load_all()
dfile <- "../R-package/source/GRIPS code/GRIPS_log_by_day_with_gaps.csv"
gripsIM <- read.csv(dfile)
gripsIM$N.Rows <- NULL
names(gripsIM) <- c("ScreenDt", "Screened", "MetCriteria", "Enrolled")
gripsIM <- gripsIM[gripsIM$Screened > 0, ]
gripsIM$ScreenDt <- as.Date(gripsIM$ScreenDt,  format = "%m/%d/%Y")
minDt <- min(gripsIM$ScreenDt)
gripsIM <- gripsIM[gripsIM$ScreenDt < (minDt + 365L), ]


# grips$ScreenDt[[5]] <- "a235"

LoadData(gripsIM, ScreenDt, Enrolled)


grips <- grips[order(ScreenDt)]
grips$gaps <- diff(grips$ScreenDt) |> as.integer() |> (\(x) c(x - 1L, 0L))()

nn <- nrow(grips)
for (i in seq_len(nn)) {
  gap <- grips[i, gaps]
  if (gap) {
    for (k in seq_len(gap)) {
      l <- list(grips[i, ScreenDt] + k, 0L, 0L, 0L, 0L)
      grips <- rbindlist(list(grips, l))
    }
  }
}

grips <- grips[, gaps := NULL][order(ScreenDt)]
grips[,`:=`(
  week = isoweek(ScreenDt),
  year = isoyear(ScreenDt),
  holiday = tis::isHoliday(ScreenDt, TRUE, TRUE) * 1L
)]

gripsWkAll <- grips[, -c("ScreenDt")][, lapply(.SD, sum), by = .(year, week)]
gripsWk1yr <- gripsWkAll[seq_len(52L)]
gripsWk2yr <- gripsWkAll[seq.int(53L, 104L)]





grips <- grips |> within(expr = {
  Screening_Date <- Screening_Date |> as.Date(format = "%m/%d/%Y")
  week <- isoweek(Screening_Date)
  year <- year(Screening_Date)
  isholiday <- isHoliday(Screening_Date)
  notactiveenrolm <- `N Rows` == 1
})

form <- cbind(
  Enrolled,
  MetCriteria,
  Screened,
  notactiveenrolm,
  isholiday
) ~ week + year


gripsweek <- grips |> aggregate(form, sum)
grips1sty = gripsweek[2:53, ]
grips2ndy = gripsweek[54:105, ]
gripsy2 = gripsweek[2:105, ]
gripsr = gripsweek[106:143, ]



usethis::use_data(grips, gripsweek, grips1sty, grips2ndy, overwrite = T)

binomWt <- dbinom(0:51, 51, 0.5) |> (\(x)  x / sum(x))()
indxWt <- \(t) ((0:51 + 26 - t) %% 52) + 1
fixIndx <- \(i) if (i < 53L) i else (i - 1L) %% 52L + 1L
probVec <- \(t) binomWt[indxWt(t)]
probs <- lapply(1L:52L, probVec)

usethis::use_data(probs, internal = TRUE)



devtools::document()

usethis::use_rcpp()


Rcpp::cppFunction('CharacterVector samplefunc() {
  CharacterVector v = {"Cat", "Dog", "Fox", "Fish", "Lion"} ;
  NumericVector v3 {1.0, 2.0, 3.0};
  CharacterVector v2 = sample(v, 1) ;
  return v2 ;
}')


Rcpp::cppFunction(
'NumericVector raek() {
  NumericVector v3 {1.0, 2.0, 3.0};
  return v3;
}')


Rcpp::cppFunction(
  'NumericVector s1(List probs, int n) {
  NumericVector p = probs[n - 1];
  return p;
}')




require(rbenchmark)
loadTrainVector(grips1sty$Sum_Enrolled)


benchmark(
  "sim1wt" = lapply(seq_len(1e4L), \(x) sim1wt(50L)),
  "sim1wt1" = lapply(seq_len(1e4L), \(x) sim1wt1(50L)),
  "simall" = simAll(),
  replications = 1)



simulciwt_IM <- function(data, colName, nos, nsimul, cl = c(0.025, 0.5, 0.975)) {
  vec <- data[[colName]]
  vecLen <- length(vec)
  onesimulationwt_IM <- function(vec, nsubjects) {
    ns <- 0L
    i <- 0L
    while (ns <= nsubjects) {
      i <- i + 1L
      pvec <- probs[[fixIndx(i)]]
      id <- .Internal(sample(vecLen, 1L, FALSE, pvec))
      ns <- ns + vec[[id]]
    }
    return(i)
  }
  weeks <- vapply(seq.int(nsimul), function(x) onesimulationwt_IM(vec, nos), 0L)
  list(weeks = weeks, quantiles = quantile(weeks, probs = cl))
}


results <- benchmark(
  "Initial" = {simulciwt_IM(data = grips1sty, nos = 50L, nsimul = 1000L,
                            colName = "Sum_Enrolled")},
  "Updated" = {simulciwt_IM1(nos = 50L, nsimul = 1000L)},
  replications = 10
)

simAllWt <- function(nSim = 1e4L, nSub = 50L, cl = c(.025, .5, .975)) {
  weeks <- vapply(seq.int(nSim), function(x) sim1wt1(nSub), 0L)
  return(list(weeks = weeks, quantiles = quantile(weeks, probs = cl)))
}






sel <- function(trained, drop = FALSE){
  nms <- names(trained)
  score <- 
    1.0 * nchar(gsub("[^%]", "", nms)) + 
    1.5 * grepl("%Y", nms, fixed = T) + 
    1.6 * grepl("%y(?!%)", nms, perl = T) + 
    .31 * grepl("%[Bb]", nms) + 
    .30 * grepl("%Om", nms) + 
    .30 * grepl("%Op", nms) + 
    .32 * grepl("%Ob", nms)
  n0 <- trained != 0
  assign("score", score, envir = globalenv())
  if (drop) {
    score <- score[n0]
    trained <- trained[n0]
  }
  else {
    score[!n0] <- -100
  }
  assign("nms", names(trained), envir = globalenv())
  assign("trained", trained, envir = globalenv())
  assign("score", score, envir = globalenv())
  names(trained)[order(score, trained, decreasing = T)]
}



checkArgs <- function(data, name) {
  qReg <- "^(\"|\').+(\"|\')$"
  dStr = deparse(eval(substitute(substitute(data)), parent.frame()))
  name <- eval(substitute(substitute(name)), parent.frame())
  nStr <- deparse(name)
  if (grepl(qReg, nStr)) nStr <- name
  if (!hasName(data, nStr)) {
    msg <- sprintf('"%s" not found in data = "%s"', nStr, dStr)
    stop(msg)
  }
  nStr
}
  

LoadData <- function(data = NULL, train, date) {
  if (!is.null(data)) {
    if (typeof(data) != "list") stop("data must be a list/data.frame")
    tStr <- checkArgs(data, train)
    dtStr <- checkArgs(data, date)
    tStr
  }
}

LoadData(grips, Sum_Enrolled, Screening_Date)


