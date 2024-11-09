# Initial setup
the <- new.env(parent = emptyenv())
Rcpp::loadModule(module = "mod", TRUE, env =  environment())
load("R/sysdata.rda")
the$probs = the$binomWt <- wts[["binomial"]]
the$cauchyWt <- wts[["cauchy"]] 
the$color <- .Platform$GUI %in% c("RStudio", "RTerm")

exportModuleMethods <- \(instance) {
  cl <- substitute(instance);
  if (!methods::is(the$cpp, "Rcpp_rct")) stop("For internal use only")
  mn <- rct@methods |> names()
  mthds <- lapply(mn, as.name)
  fx <- (\(x, y) lapply(y, \(z) {z <- z; x <- x; substitute(x$z)}))
  fx(cl, mthds) |> lapply(eval, topenv())
  lapply(mn, \(m) the[[m]] <- get(m, instance)) |> invisible()
}


useFilled <- function(fill = FALSE, env = the) {
  the$cpp$train = env$train <- if (fill) env$Trainfilled else env$TrainVector
}

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
#' @export
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
  TrainVector <- the$train
  probs <- the$probs
  out <- sim1(TrainVector, probs, nsubjects, startWeek)
  out
}

# Fill gap weeks with values sampled from non-zero weeks
fillEmptyWeeks <- function(x, id0) {
  zeroIdx <- which(id0 == 0)
  nonZeroVals <- x[-zeroIdx]
  for (i in zeroIdx) {
    x[i] <- sample(nonZeroVals, 1)
  }
  x
}

refill <- function() {
  pre <- sum(the$Trainfilled);
  zeroIdx <- which(the$datWeeks$cnt == 0)
  nonZeroVals <- the$TrainVector[-zeroIdx]
  for (i in zeroIdx) {
    the$Trainfilled[i] <- sample(nonZeroVals, 1)
  }
  post <- sum(the$Trainfilled);
  cat(pre, "->", post, "\n")
}

logPrint <- \(x) {
  print(x) |>
    capture.output() |>
    cat(... = _, "", sep = "\n")
}



PredCI <- \(nSim = 1e4L, fillGaps = FALSE, cauchyWt = FALSE) {
  if (is.null(the$TrainVector)) stop("TrainVector not loaded")
  useFilled(fillGaps)
  the$useCauchy(cauchyWt)
  out <- the$cpp$PredCIbyWk(nSim) |> rbind(rep(0, 3), ... = _)
  rownames(out) <- 0:(nrow(out) - 1)
  head(out) |> logPrint();
  tail(out) |> logPrint();
  invisible(out);
}

measure <- \(x) {
  str <- Sys.time(); 
  x <- substitute(x)
  eval(x, topenv()) |> invisible()
  Sys.time() - str
}


notScalar <- \(x) length(x) != 1
isNotBool <- \(x) !is.logical(x)
isNotNum <- \(x) !(typeof(x) %in% c("integer", "double"))

argsTests <- list(
  nSim = \(x) {
    if (isNotNum(x)) stop(msgList[["nSim"]], call. = FALSE)
    else if (notScalar(x)) stop('nSim must have length = 1', call. = FALSE)
    else if (x < 1 || x > 1e4)  stop(msgList[["nSim"]], call. = FALSE)
  },
  fillGaps = \(x) {
    if (isNotBool(x)) stop(msgList[["fillGaps"]])
    else if (notScalar(x)) stop('fillGaps must have length = 1', call. = FALSE)
  },
  cauchyWt = \(x) {
    if (isNotBool(x)) stop(msgList[["cauchyWt"]])
    else if (notScalar(x)) stop('cauchyWt must have length = 1', call. = FALSE)
  },
  nSub = \(x) {
    maxN <- sum(the$TrainVector) * 10L 
    if (isNotNum(x)) stop("nSub must numeric or integer", call. = FALSE)
    else if (notScalar(x)) stop("nSub must have length = 1", call. = FALSE)
    else if (x < 1 || x > maxN) 
      stop(sprintf(msgList[["nSub"]], maxN), call. = FALSE)
  }
)

msgList <- list(
  Load = 'Please, use "LoadData" function to load your data',
  nSim = 'nSim must be an integer/numeric between 1 and 10,000',
  fillGaps = 'fillGaps must be TRUE or FALSE',
  cauchyWt = 'cauchyWt must be TRUE or FALSE',
  nSub = 'nSub must be between 1 and %s'
)


checkExportedFunctionsArgs <- \() {
if (is.null(the$TrainVector)) stop(msgList[["Load"]])
  x <- names(formals(sys.function(sys.parent())))
  env <- sys.frame(sys.nframe() - 1L)
  funArgs <- mget(x, env)
  for (argName in names(funArgs)) {
    argValue <- funArgs[[argName]]
    argsTests[[argName]](argValue)
  }
}

testArgs <- \(nSim = 1e4L, fillGaps = FALSE, cauchyWt = FALSE) {
  checkExportedFunctionsArgs()
}


