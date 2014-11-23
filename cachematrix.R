#This function caches the inverse of a function

makeCacheMatrix <- function(x = matrix()) {
	#inverse of matrix
	inv <- NULL
  
  # set the matrix and initialize the inverse of it to NULL
	set <- function(y) {
     x <<- y 
     inv <<- NULL 
  }
  
  #getter for the matrix
  get <- function() x
  
  #set the inverse (cache it)
  setinv <- function(inverse) inv <<- inverse 
  
  #get the inverse
  getinv <- function() inv
  
  #return a list of functions for the set, get, setinv, and getinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function computes the inverse of the matrix.  Before it is calculated, it checks if it
# was already cached, and if so, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv() 
  
  
        # If the inverse was already cached, use the cached value 
        if (!is.null(inv)) { 
             message("getting cached data") 
             return(inv) 
        } 
 
  
        # The inverse was not previously calculated, so we calculate it 
        data <- x$get() 
        inv <- solve(data, ...) 
 
  
        # Cache the inverse 
         x$setinv(inv) 
 
  
         # Return the inverse
       inv 
  
}
