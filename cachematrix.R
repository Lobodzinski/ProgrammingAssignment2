## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# function makeCacheMatrix defines a list of actions 
# (functions) for caching of results of operations 
# on an input matrix x

makeCacheMatrix <- function(x = matrix()) {

     # removes the matrix m from cache 
     # (deactivated ) 
     # m <- NULL

     # clean-up the environment:
     # setup the matrix x if exists in external environments
     # and delete the matrix m
     set <- function(y) {
         x <<- y
         m <<- NULL
     }

     # return the matrix x
     get <- function() x

     # set the matrix m
     setmatrix <- function(matrix) m <<- matrix

     # return the matrix m
     getmatrix <- function() m

     # set the list with named elements:
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)

}


## Write a short comment describing this function

# the function "cacheSolve" returns the inverse of 
# an input matrix x using the functional environment 
# defined in the function "makeCacheMatrix" 
   
cacheSolve <- function(x, ...) {

     ## Return a matrix that is the inverse of 'x'
     # get the matrix m from the list element "getinverse" if the matrix m exists
     m <- x$getmatrix()

     # otherwise use cached data
     # (true if we remove the function "set" in makeCacheMatrix):
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }

     # and calculate the matrix m:
     # get input matrix
     data <- x$get()

     # find the inverse matrix "m" of the input matrix "data"
     m <- solve(data, ...)

     # set the inverse matrix "m"
     x$setmatrix(m)

     # return the inverse matrix "m"
     m
}
