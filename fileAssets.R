getData <- function(...) {
  initial <- read.table(nrows = 100, ...) # initial call to get classes
  classes <- sapply(initial, class)
  data <- read.table(colClasses = classes, ...)
}