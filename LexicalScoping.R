makeVector <- function(x = numeric()) {
  m <- NULL
  
  # Sets a vector y into the the variable x cached in the parent environment.
  # Clears out the variable m as well
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  # Sets the value of the mean into the variable m cached in the parent environment.
  setmean <- function(mean) m <<- mean
  
  getmean <- function() m
  
  # Return the list of functions
  list(set = set,
       get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  
  # Get cached data if it exists
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }
  
  # Otherwise calculate the mean and store it in the cache
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  
  m
}