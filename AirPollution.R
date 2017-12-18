pollutantmean <- function(directory, pollutant, id = 1:332) {
  oldDir <- getwd()
  setwd(directory)
  total <- 0
  count <- 0
  for (i in 1:length(id)) {
    site <- id[i]
    if (site < 10) {
      fname <- paste0("00", site, ".csv")
    } else if (site < 100) {
      fname <- paste0("0", site, ".csv")
    } else {
      fname <- paste0(site, ".csv")
    }
    data <- read.table(fname, header=TRUE, sep=",")[[pollutant]]
    good_data <- data[!is.na(data)]
    total <- total + sum(good_data)
    count <- count + length(good_data)
  }
  
  setwd(oldDir)
  
  total / count
}

complete <- function(directory, id = 1:332) {
  oldDir <- getwd()
  setwd(directory)
  nobs <- integer(length(id))
  for (i in 1:length(id)) {
    site <- id[i]
    if (site < 10) {
      fname <- paste0("00", site, ".csv")
    } else if (site < 100) {
      fname <- paste0("0", site, ".csv")
    } else {
      fname <- paste0(site, ".csv")
    }
    data <- read.table(fname, header=TRUE, sep=",")
    nobs[i] <- nrow(data[complete.cases(data), ])
  }
  
  setwd(oldDir)
  
  cbind(id, nobs)
}

corr <- function(directory, threshold = 0) {
  oldDir <- getwd()
  setwd(directory)
  
  correlations <- numeric()
  files <- list.files(pattern = "\\.csv$")
  
  for (file in files) {
    data <- read.table(file, header=TRUE, sep=",")
    good_data <- data[complete.cases(data), ]
    
    if (nrow(good_data) > threshold) {
      correlations <- c(correlations, cor(good_data$sulfate, good_data$nitrate))
    }
    
  }
  
  setwd(oldDir)
  
  correlations
}