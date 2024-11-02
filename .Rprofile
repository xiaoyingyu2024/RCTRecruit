local({
  pckgs <- c("Rcpp", "devtools", "usethis", "knitr", "lubridate", "tis", "bibtex")
  chckpg <- \(x) !base::requireNamespace(x, quietly = TRUE)
  pckgs[pckgs |> sapply(chckpg)] |> 
    (\(x) if (length(x)) install.packages(x, dependencies = TRUE))()
  if (!pkgbuild::has_rtools()) {
    Rt <- "https://CRAN.R-project.org/bin/windows/Rtools/"
    cat("\tPlease, click to install Rtools:\n\t\033[32m", 
        cli::style_hyperlink(text = Rt, url = Rt)
    )
  }
})



if (interactive()) {
  suppressMessages(require(devtools))
}
source("tools/dataDocHelpers.R");
options( usethis.overwrite = TRUE )


