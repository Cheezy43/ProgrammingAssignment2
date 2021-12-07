## The cachematrix.R file contains two functions, "makeCacheMatrix()" and "cacheSolve()".
##
## makeCacheMatrix() creates an R object that stores a matrix and the inverse of the matrix.
##
## cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to retrieve
## or create the inverse matrix from the cached value stored in makeCacheMatrix() objects environment.



## The makeCacheMatrix initiates and stores the objects containing the original and inverse matrices,
## and returns a fully formed object with named elements and "getter" and "setter" functions for adjusting object values.

makeCacheMatrix <- function(x = matrix()) {   ## Initializing "x" as an empty matrix - to be filled when initiating the function
      mtrx <- NULL           ## Initializing "mtrx" object as a "NULL" value clearing any cached data

      set <- function(y) {   ## set() - redefines the matrix to be inverted 
            x <<- y          ##  - assigns the value "y" to the "x" object assigned in the parent environment    
            mtrx <<- NULL    ##  - assigns the value of "NULL" to the "mtrx" object assigned in the parent environment to clear any cached values    
      }
      get <- function() {x}                           ##  defines the getter for "x" which is retrieved from the parent environment of "makeCacheMatrix()"
      setinvmtrx <- function(inv) {mtrx <<- inv}      ##  defines the setter for the inverse matrix - assign the input argument to the value of "mtrx" in the parent environment
      getinvmtrx <- function() {mtrx}                 ##  defines the getter for the inverse matrix - assign to the value of "mtrx" in the parent environment
      list(set = set, get = get, setinvmtrx = setinvmtrx, getinvmtrx = getinvmtrx)  ##  assigns each of these functions as an element within a list(), and returns it to the parent environment - allows us to use the "$" operator to access the functions by name
}

## cacheSolve is required to populate and/or retrieve the inverse of the matrix from an object of type "makeCacheMatrix()"

cacheSolve <- function(x, ...) {    ## Return a matrix that is the inverse of 'x'
      mtrx <- x$getinvmtrx()        ## attempts to retrieve an inverted matrix from the object "getinvmtrx()" 
      if(!is.null(mtrx)){           ## checks to see whether the result is "NULL" or if we have a valid, cached matrix
            message("getting cached data")  ## message identifying we have a cached matrix
            return(mtrx)            ## returns the value of the matrix to the parent environment by printing the mean object
      }
      data <- x$get()         ## retrieve the value of "x" from the parent environment and assign to "data" in the current environment 
      mtrx <- solve(data)     ## execute the "solve()" function to invert the matrix and assign to the "mtrx" object of the parent environment
      x$setinvmtrx(mtrx)      ## set the inverse matrix as the current "mtrx" value
      mtrx
}

