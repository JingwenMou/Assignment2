## Put comments here that give an overall description of what your
## functions do

#functions given by the exercise
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## install and set up the matlib() package contains the Inverse() function.

## Create a matrix which is invertible.

makeCacheMatrix <- function(x = matrix()) {
## Set matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
## Get matrix
    get <- function() x
    setinverse <- function(Inverse) m <<- Inverse
    getinverse <- function() m
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculate the inverse matrix of x.

cacheSolve <- function(x, ...) {
    
    ## get the inverse
    m <- x$getinverse()
    
    if (!is.null(m)){
        message ("getting cached data")
        return (m)
    }
    ## get the original created matrix
    data <- x$get()
    ## calculate the inverse using Inverse() function
    m <- Inverse (data, ...)
    
    x$setinverse(m)
    m
}
