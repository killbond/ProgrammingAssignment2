## This functions intend for more comfortable work with a matrix
## They provide a methods for caching an matrix and its inverse
## This functions demonstrate R's scoping rules
##
## Example: 
##    > test <- matrix(c(2, 3, 2, 4), nrow = 2, ncol = 2)
##    > cachedMatrix <- makeCacheMatrix(test)
##    > cacheSolve(cachedMatrix)

## Creates a special matrix object
##
## (matrix) m - matrix
##
makeCacheMatrix <- function(m = NULL) {
  # define matrix inverse property
  i <- NULL
  
  # function-initializer of object properties
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  
  # this function returns source value matrix
  get <- function() m
  
  # initialize value matrix inverse
  setInverse <- function(inverse) i <<- inverse
  
  # returns matrix inverse
  getInverse <- function() i
  
  # creates a list object with all functions above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function return the inverse
## of previously cached matrix (m)
## 
## (makeCacheMatrix) m - previously cached matrix
##
cacheSolve <- function(m) {
  # gets the inverse of m
  inverse <- m$getInverse()
  
  # check that inverse is realy contain matrix
  if(!is.null(inverse)) {
    message("getting cached data")
    
    # return inverse
    return(inverse)
  }
  
  # calculate inverse matrix of m
  inverse <- solve(m$get())
  
  # set inverse matrix to m
  m$setInverse(inverse)
  
  # return inverse
  inverse
}
