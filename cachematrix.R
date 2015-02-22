## These functions are used to compute the inverse of a square matrix. They can be implemented to first check if the
## inverse of the matrix passed to the function is saved in the cache, then if the inverse does not already exist, the
## inverse will be computed.


## This function sets elements of the matrix, gets the elements of the matrix, sets the elements of the inverse
## of the matrix, and gets the elements of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
     
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) inv <<- solve
     getInverse <- function() inv
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function is used to return a matrix that is the inverse of the matrix 'x', passed to the function. If the
## inverse has already been calculated (and the matrix not changed), it will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     base <- x$get()
     inv <- solve(base,...)
     x$setInverse(inv)
     inv
}
