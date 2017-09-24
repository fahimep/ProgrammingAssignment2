## Author:Fahimeh Pouryani
## Date:9/24/2017

## The makeCacheMatrix() function receives a matrix as input and cashes its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcashe <- function(cashe) m <<- cashe
  getcashe <- function() m
  list(set = set, get = get,
       setcashe = setcashe,
       getcashe = getcashe)
  
}






## The cashesolve() function returns the inverse of the input matrix.
## If the inverse is already built, this function retrives and returns it.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  
  m <- x$getcashe()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setcashe(m)
  m
  
  
}

