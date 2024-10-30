# https://jsta.rbind.io/blog/automated-roxygen-documentation-of-r-package-data/
# modified from http://r-pkgs.had.co.nz/man.html
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))
  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))
  cols <- lapply(df, format, ...)
  argContents <- c(cols, list(sep = " \\tab ", collapse = "\\cr\n"))
  contents <- do.call("paste", argContents)
  paste(
    "\\tabular{", paste(col_align, collapse = ""), "}{\n",
    contents, "\n}\n",
    sep = ""
  )
}

get_table_metadata <- function(csvDatName) {
  path <- paste0("data-raw/", csvDatName, ".csv")
  dt <- utils::read.csv(path, stringsAsFactors = FALSE)
  paste0(readLines(textConnection(tabular(dt))))
}

genDataDoc <- function(dat) {
  nams <- names(dat)
  contents <- lapply(seq_along(nams), function(i) {
    nn <- nams[[i]]
    c(paste0("\\[,", i,  "\\]"),
      nn,
      paste0("\\code{", typeof(dat[[nn]]), "}"),
      attr(dat[[nn]], "Description")
    ) |> paste(... = _, collapse = " \\tab ")
  }) |>
    paste(... = _, collapse = "\\cr\n")
  paste("\\tabular{llll}{\n", contents, "\n}\n", sep = "")
}
