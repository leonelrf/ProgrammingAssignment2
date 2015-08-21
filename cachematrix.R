## Two functions to calculate the inverse of a matrix and cache it to save 
## computation time in case of repeated calculations :
## * makeCacheMatrix
## * cacheSolve

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The object returned is in fact a list containing functions to
## * set the value of the matrix
## * get the value of the matrix
## * set the value of the inverse
## * get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      ## the "<-" operator assigns into the environment in which it is evaluated
      ## setting the inverse to NULL as a placeholder for a future value
      set <- function(y) {
      ## sets the matrix, x, to a new matrix, y, and resets the inverse, inv, to
      ## NULL
            x <<- y
            ## The "<<-" operator causes a search to made through parent 
            ## environments for an existing definition of the variable being
            ## assigned. If such a variable is found then its value is redefined,
            ## otherwise assignment takes place in the global environment.
            inv <<- NULL
      }
      get <- function()
      ## Returns the matrix x
            x
      setinverse <- function(inverse)
      ## sets the inverse, inv, to inverse
            inv <<- inverse
      getinverse <- function()
      ## returns the inverse, inv
            inv
      list(
            set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse
      )
      ## return the list of functions
}


## cacheSolve will first check for a cached inverse of the matrix. If no cached
## inverse exists, it will calculate the inverse of the matrix passed to 
## makeCacheMatrix, cache it and return it.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## 'x' is the list returned by makeCacheMatrix
      inv <- x$getinverse()
      ## if 'inv' has never been calculated before, then 'inv' is NULL
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
            # If 'inv' has been calculated, return its value
            # 'return' ensures an early exit of the function 
      }
      data <- x$get()
      ## 'data' contains the matrix passed to the makeCacheMatrix function
      inv <- solve(data, ...)
      ## If inverse of matrix has not been calculated before, calculate using
      ## function solve
      x$setinverse(inv)
      ## cache the calculated inverse
      inv
      ## return inverse
}

## Example
## > rm(list = ls()) # clear working environment
## > source("cachematrix.R") # Define functions
## > matrix(c(4,2,7,6),2,2)
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## > x <- makeCacheMatrix(matrix(c(4,2,7,6),2,2))
## > class(x)
## [1] "list"
## > cacheSolve(x) # First run
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > cacheSolve(x) # Next runs - "getting cached data" message will be printed
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > x$get() %*% cacheSolve(x)
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## Identity matrix returned - cacheSolve is really returning the inverse