desc <- "Accurate prediction of subject recruitment for randomized clinical
trials (RCT) remains anongoing challenge. Previous prediction models rely on
parametric assumptions. We present functions for non-parametric RCT recruitment
prediction under several scenarios."

ttl <- "Non-parametric Recruitment Prediction for Randomized Clinical Trials"
def <- use_description_defaults("RCTrecruit", fields = list(
  Title = ttl,
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
  person(
    "Alejandro",
    "Villasante-Tezanos",
    email = "alvillas@utmb.edu",
    role = c("aut")),
  person(
    "Christopher",
    "Kurinec",
    email = "chkurine@utmb.edu",
    role = c("aut")),
  person("Xiaoying", "Yu", email = "xiyu@utmb.edu", role = c("aut"))
)


m <- read.dcf("DESCRIPTION") |>
  as.data.frame() |>
  lapply(\(x) gsub("\\n", " ", x))
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



# (base::requireNamespace("knitr", quietly = TRUE)) {







load_all()


dfile <- "../R-package/source/GRIPS code/GRIPS_log_by_day_with_gaps.csv"
grips <- read.csv(dfile)
grips$N.Rows <- NULL
names(grips) <- c("ScreenDt", "Screened", "MetCriteria", "Enrolled")
grips <- grips[grips$Screened > 0, ]
grips$ScreenDt <- as.Date(grips$ScreenDt,  format = "%m/%d/%Y")
grips <- grips[c("ScreenDt", "Enrolled")]

library(lubridate)
minDtyr1 = minDtyr2 <- min(grips$ScreenDt)
year(minDtyr2) = year(minDtyr2) + 1
maxDtyr1 = maxDtyr2 <- minDtyr2 - 1
year(maxDtyr2) = year(maxDtyr2) + 1


gripsYR1 <- grips[grips$ScreenDt <= maxDtyr1 , ]
rownames(gripsYR1) <- NULL

gripsYR2 <- grips[grips$ScreenDt >= minDtyr2 & grips$ScreenDt <= maxDtyr2, ]
rownames(gripsYR2) <- NULL

yr2Cum <- with(gripsYR2, days2weeks(ScreenDt, Enrolled))$enrolled |> cumsum() 




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


require(rbenchmark)
loadTrainVector(grips1sty$Sum_Enrolled)


benchmark(
  "sim1wt" = lapply(seq_len(1e4L), \(x) sim1wt(50L)),
  "sim1wt1" = lapply(seq_len(1e4L), \(x) sim1wt1(50L)),
  "simall" = simAll(),
  replications = 1)

sel <- function(trained, drop = FALSE) {
  nms <- names(trained)
  score <-
    1.0 * nchar(gsub("[^%]", "", nms)) +
    1.5 * grepl("%Y", nms, fixed = TRUE) +
    1.6 * grepl("%y(?!%)", nms, perl = TRUE) +
    .31 * grepl("%[Bb]", nms) +
    .30 * grepl("%Om", nms) +
    .30 * grepl("%Op", nms) +
    .32 * grepl("%Ob", nms)
  n0 <- trained != 0
  assign("score", score, envir = globalenv())
  if (drop) {
    score <- score[n0]
    trained <- trained[n0]
  } else {
    score[!n0] <- -100
  }
  assign("nms", names(trained), envir = globalenv())
  assign("trained", trained, envir = globalenv())
  assign("score", score, envir = globalenv())
  names(trained)[order(score, trained, decreasing = TRUE)]
}




