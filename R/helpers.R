# Initial setup
the <- new.env(parent = emptyenv())
load("R/sysdata.rda")
the$probs <- probs
the$color <- .Platform$GUI %in% c("RStudio", "RTerm")

log <- \(x, ...) do.call(sprintf, c(x, list(...))) |> cat()
err <- \(x, ...) do.call(sprintf, c(x, list(...))) |> stop(call. = FALSE)
wrn <- \(x, ...) do.call(sprintf, c(x, list(...))) |> 
  warning(call. = FALSE, immediate. = TRUE)

isMarkdown <- \() {
  if (base::requireNamespace("knitr", quietly = TRUE)) {
    knitr::is_html_output() || knitr::is_latex_output() 
  } else {
    FALSE
  }
}

# Utility to colorize text
fmt <- \(str, fg = 0, bk = 0, fx = NULL) {
  if (!the$color || isMarkdown()) return(str)
  larg <- list(
    if (is.null(fx)) fx else sprintf("\033[%sm", fx),
    if (fg) sprintf("\033[38;5;%sm", fg) else NULL,
    if (bk) sprintf("\033[48;5;%sm", bk) else NULL,
    str,
    "\033[0m"
  )
  do.call(paste0, larg)
}

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
    nams <- lapply(c(name, datStr), fmt, 160, 0, "1;4")
    do.call(log, c("%s not found in data = %s\n", nams))
    stop("invalid input", call. = FALSE)
  }
  dat[[name]]
}

# Try parse string to date format
fixDate <- function(dateVar) {
  if (is.null(dateVar)) stop("strDate is NULL")
  type <- typeof(dateVar)
  if (type == "character") {
    fmtDate <- c(
      "Ymd", "Ymd HM", "Ymd HMS", "mdY", "mdY IMp", 
      "mdY IMSp", "dmY", "dmY HM", "dmY HMS"
    )
    out <- dateVar |> lubridate::parse_date_time(fmtDate) |> as.Date()
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
    msg <- "%s[[%sL]]: %s"
    for (i in ids) {
      vals <- c(msg, lapply(c(the$dtStr, i, dateVar[[i]]), fmt, 208))
      do.call(wrn, vals)
    }
    if (len > 10) warning("...")
    stop("Invalid date values")
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
# days2weeks <- function() {
#   date <- the$raw$date
#   nn <- length(date)
#   gaps <- c(as.integer(diff(date)) - 1L, 0L)
#   slen <- seq_len(sum(gaps))
#   dlist <- lapply(slen, \(x) list(date = NULL, enrolled = integer(1L)))
#   j <- 0L
#   for (i in seq_len(nn)) {
#     gap <- gaps[[i]]
#     if (gap) {
#       for (k in seq_len(gap)) {
#         j <- j + 1L
#         dlist[[j]][["date"]] <- date[[i]] + k
#       }
#     }
#   }
#   cnt <- stats::setNames(integer(53L), seq_len(53L))
#   tab <- table(lubridate::isoweek(date))
#   cnt[names(tab)] <- tab
# 
#   dlist <- do.call(rbind.data.frame,  dlist)
#   dat <- rbind(the$raw, dlist)
#   dat <- dat[order(dat$date), ]
#   dat <- within(dat, {
#     week <- lubridate::isoweek(date) # nolint: object_usage_linter.
#     year <- lubridate::isoyear(date) # nolint: object_usage_linter.
#     holiday <- tis::isHoliday(date, TRUE, TRUE) * 1L # nolint
#   })
#   datw <- stats::aggregate(cbind(enrolled, holiday) ~ week + year, dat, sum)
#   datw$cnt <- 0L
#   datw$cnt <- cnt[datw$week]
#   rownames(datw) <- NULL
#   
#   return(datw[1L:52L, ])
# }


# Aggregate the data by week
days2weeks <- function(date, enrolled) {
  dat <- data.frame(date, enrolled);
  nn <- length(date)
  gaps <- c(as.integer(diff(date)) - 1L, 0L)
  slen <- seq_len(sum(gaps))
  dlist <- lapply(slen, \(x) list(date = NULL, enrolled = integer(1L)))
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
  tab <- table(lubridate::isoweek(date))
  cnt[names(tab)] <- tab
  
  dlist <- do.call(rbind.data.frame,  dlist)
  dat <- rbind(dat, dlist)
  dat <- dat[order(dat$date), ]
  dat <- within(dat, {
    week <- lubridate::isoweek(date) # nolint: object_usage_linter.
    year <- lubridate::isoyear(date) # nolint: object_usage_linter.
    holiday <- tis::isHoliday(date, TRUE, TRUE) * 1L # nolint
  })
  datw <- stats::aggregate(cbind(enrolled, holiday) ~ week + year, dat, sum)
  datw$cnt <- 0L
  datw$cnt <- cnt[datw$week]
  rownames(datw) <- NULL
  
  return(datw[1L:52L, ])
}





# Unit simulation function
# @param nsubjects Number of subjects to recruit
# @return Number of Weeks required to recruit subjects
sim1wt1 <- function(nsubjects, startWeek = 1L) {
  the <- the
  TrainVector <- the$finalVector
  probs <- the$probs
  out <- sim1(TrainVector, probs, nsubjects, startWeek)
  out
}

# Fill gap weeks with values sampled from non-zero weeks
fillGaps <- function(x, id0) {
  zeroIdx <- which(id0 == 0)
  nonZeroVals <- x[-zeroIdx]
  for (i in zeroIdx) {
    x[i] <- sample(nonZeroVals, 1)
  }
  x
}






