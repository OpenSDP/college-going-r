library(rmarkdown)

rmds <- list.files("docs", pattern = ".Rmd", full.names = TRUE)
for (i in rmds) {
  render(i, output_dir = "docs", env = parent.frame(), encoding = "UTF-8")
}
