## This function is returning a list values of input parameter matrix x, 
## which store values of the input matrix x and the inverse of matrix x = NULL.
## To access those values use $get() and $getinverse().

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function gets an input parameter list x from the makeCacheMatrix function output.
## First checking if the $getinverse value of the x is NULL, if it is NOT NULL, the inverse
## matrix was previously cached, then the function is returning the value. If the $getinverse 
## value of x is NULL, then the inverse matrix will be calculated and saved for
## next time when the function being called with the same object x.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached inversed matrix")
          return(inv)
     }
     matrix <- x$get()
     inv <- solve(matrix, ...)
     x$setinverse(inv)
     inv
}


## Sample of output by running two script above
x <- matrix(sample(25),5,5) # creating matrix x
rm(cachex) # removing cachex from memory
cachex <- makeCacheMatrix(x) # create the cache list object variable cachex
cachex # to show the cache list cachex variable
cachex$get() # to show the cache matrix value from variable cachex
cachex$getinverse() # to show the initial chace inverse matrix, the value should be NULL
cacheSolve(cachex) # getting the inverse matrix first time
cacheSolve(cachex) # getting the inverse matrix second time, "getting cached inversed matrix" message should be shown.


     

