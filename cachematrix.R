#' Calcuating the inverse of a matrix can be an expensive operation.
#' The functions in this file provide the ability to cache
#' the inverse of a matrix and return the cached value on 
#' subsequent retrievals of the inverse.   

#' Creates a cache matrix. 
#' A cache matrix stores the value of a matrix 
#' and can cache the inverse of the matrix in conjunction with
#' the cacheSolve function
#' 
#' @param x A matrix that is assumed to be inversible 
#' @return a cache matrix object
#' @examples
#' makeCacheMatrix(matrix(c(4,1,3,1),nrow=2,ncol=2))
#'  
makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) xInverse <<- inverse
  getInverse <- function() xInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#' Caculates the inverse of the matrix stored in a Cache Matrix object
#' 
#'  This function will return a cached value for the inverse if has
#'  already been caculated.   If the value of the matrix changes 
#'  the iverse will be recalcuated.  
#'  
#'   @param x the cache matrix to calculate the inverse for
#'   @param ... additional parameters to pass to the solve function
#'   @return the inverse of x 
#' 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInverse <- x$getInverse()
  if (!is.null(xInverse)) {
    message("getting cached data")
    return (xInverse)
  }
  data <- x$get()
  xInverse <- solve(data,...)
  x$setInverse(xInverse)
  xInverse
}
