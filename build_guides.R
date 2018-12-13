# Rebuild the guides

# get names

rmds <- list.files("docs/", pattern = ".Rmd", full.names = TRUE)
# enrl_grad_persist_any in college_going_persistence not found
library(knitr)
for(i in rmds) {
  knit(i)
}