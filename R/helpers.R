# Initial setup
the <- new.env(parent = emptyenv())
load("R/sysdata.rda")
the$probs <- probs
the$color = .Platform$GUI %in% c("RStudio", "RTerm")

# Check input is of correct type
checkIntNumType <- function(x) {
  if (is.null(x)) stop("x is NULL")
  if (!(class(x) %in% c("integer", "numeric")))
    stop("x must be an integer/numeric vector")
}

# Check for invalid input values
checkInvalidValues <- function(x) {
  if (any(is.nan(x))) stop("x has NaN values")
  if (any(is.infinite(x))) stop("x has Inf values")
  if (any(is.na(x))) stop("x has NAs")
  if (any(x < 0L)) stop("x has negative values")
}

# Check if a variable exists in a data frame and return it
checkArgs <- function(datStr, name) {
  dat <- get(datStr, parent.frame())
  if (!utils::hasName(dat, name)) {
    msg <- sprintf('"%s" not found in data = "%s"', name, datStr)
    stop(msg)
  }
  dat[[name]]
}

# Try parse string to date format
fixDate <- function(dateVar) {
  if (is.null(dateVar)) stop("strDate is NULL")
  type <- typeof(dateVar)
  if (type == "character") {
    fmt <- c(
      "Ymd", "Ymd HM",  "Ymd HMS",
      "mdY", "mdY IMp", "mdY IMSp",
      "dmY", "dmY HM",  "dmY HMS"
    )
    out <- dateVar |> lubridate::parse_date_time(fmt) |> as.Date()
  } else if (type %in% c("integer", "numeric", "double")) {
    out <- as.Date(dateVar, origin = "1970-01-01")
  } else {
    stop("strDate must be a character or numeric vector")
  }
  NAs <- is.na(out)
  if (any(NAs)) {
    ids <- which(NAs)
    len <- length(ids)
    ids <- ids[seq_len(min(10L, len))]
    nn <-  deparse(substitute(dateVar))
    for (i in ids) {
      msg = fmt(sprintf("%s[[%dL]]: %s", the$dtStr, i, dateVar[[i]]), bold = 7)
      warning(msg, call. = FALSE, immediate. = TRUE)
    }
    if (len > 10) warning("...")
    stop(fmt(sprintf("Cannot parse %d date(s)", len), bold = 7), call. = FALSE)
  }
  out[order(out)]
}

# Fix the input vector to be integer and without invalid values
fixEnrolled <- function(TrainVector) {
  checkIntNumType(TrainVector)
  TrainVector <- as.integer(TrainVector)
  checkInvalidValues(TrainVector)
  TrainVector
}

# Aggregate the data by week
days2weeks <- function() {
  date <- the$raw$date
  nn <- length(date)
  gaps <- c(as.integer(diff(date)) -1L, 0L)
  slen <- seq_len(sum(gaps))
  dlist = lapply(slen, \(x) list(date = NULL, enrolled = integer(1L)))
  j <- 0L
  for (i in seq_len(nn)) {
    gap <- gaps[[i]]
    if (gap) {
      for (k in seq_len(gap)) {
        j <- j + 1L
        dlist[[j]][["date"]] <- date[[i]] + k
      }
    }
  }
  cnt <- stats::setNames(integer(53L), seq_len(53L))
  the$tab = tab <- table(lubridate::isoweek(date))
  cnt[names(tab)] <- tab
  the$cnt = cnt
  
  dlist <- do.call(rbind.data.frame,  dlist)
  dat <- rbind(the$raw, dlist)
  dat <- dat[order(dat$date), ]
  dat <- within(dat, {
    week <- lubridate::isoweek(date)
    year <- lubridate::isoyear(date)
    holiday <- tis::isHoliday(date, TRUE, TRUE) * 1
  })
  datw <- stats::aggregate(cbind(enrolled, holiday) ~ week + year, dat, sum)
  datw$cnt <- 0L
  datw$cnt <- cnt[datw$week]
  if (datw$week[1L] == datw$week[nrow(datw)]) {
    datw = datw[-1L, ]
    rownames(datw) <- NULL
  }
  return(datw)
}

# Unit simulation function
# @param nsubjects Number of subjects to recruit
# @return Number of Weeks required to recruit subjects
sim1wt1 = function(nsubjects, startWeek = 1L) {
  the <- the
  TrainVector <- the$finalVector
  probs <- the$probs
  out = sim1(TrainVector, probs, nsubjects, startWeek)
  out
}

# Utility to colorize text
fmt <- \(str, fg = 0, bk = 0, bold = NULL) {
  if (the$color) {
    if (fg == bk && bk == 0 && is.null(bold)) return(str)
    larg <- list(
      if (is.null(bold)) bold else sprintf("\033[%sm", bold),
      if (fg) sprintf("\033[38;5;%sm", fg) else NULL,
      if (bk) sprintf("\033[48;5;%sm", bk) else NULL,
      str,
      "\033[0m"
    )
    do.call(paste0, larg)
  } else str
}

# Fill gap weeks with values sampled from non-zero weeks
fillGaps <- function(x) {
  zeroIdx = which(x == 0)
  nonZeroVals = x[-zeroIdx]
  for (i in zeroIdx) {
    x[i] = sample(nonZeroVals, 1)
  }
  x
}



