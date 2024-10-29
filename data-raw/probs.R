## code to prepare `probs` dataset goes here
binomWt <- stats::dbinom(0:51, 51, 0.5) |> (\(x)  x / sum(x))()
indxWt <- \(t) ((0:51 + 26 - t) %% 52) + 1
probVec <- \(t) binomWt[indxWt(t)]
probs <- lapply(1L:52L, probVec)
usethis::use_data(probs, overwrite = TRUE, internal = TRUE)

