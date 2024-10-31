# Setup variables
citationPath = "private/citations/"

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

bibname <- \(x) paste0(citationPath, x, ".bib") 

addRef <- \(fname) {
   bibname(fname) |> 
    bibtex::read.bib() |>
    utils:::format.bibentry()
}

mendeley2bib <- \(fname) {
  ref <- readClipboard()
  if (grepl("^@article", ref[[1L]])) {
    if (length(ref) > 4L) {
      write(ref[-2L], file = bibname(fname))
      bibs <- list.files(citationPath, "*\\.bib", full.names = TRUE)
      cat("\033[31m", bibs, "\033[0m\n", ref[-2L])
    } else stop ("Wrong length")
    } else stop("Invalid input")
}

readDesc <- function() {
  read.dcf("DESCRIPTION") |> 
    as.data.frame() |> 
    lapply(\(x) gsub("\\n", "", x))
}
