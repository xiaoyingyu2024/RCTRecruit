## code to prepare `probs` dataset goes here
indxWt <- \(t) ((0L:51L + 26L - t) %% 52L) + 1L
binomWt <- stats::dbinom(0L:51L, 51L, 0.5) |> (\(x)  x / sum(x))()
binProbVec <- \(t) binomWt[indxWt(t)]
cauchyWt <- seq(-3L, 3L, 6 / 51) |> stats::dcauchy() |> (\(x)  x / sum(x))()
cauchyProbVec <- \(t) cauchyWt[indxWt(t)]

wts <- list()
wts[["binomial"]] <- lapply(1L:52L, binProbVec)
wts[["cauchy"]] <- lapply(1L:52L, cauchyProbVec)

usethis::use_data(probs, wts, overwrite = TRUE, internal = TRUE)


