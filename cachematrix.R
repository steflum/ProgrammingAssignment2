## These functions solve for the inverse of an inputed matrix.
## If a matrix had been previously solved, the inverse to that
## that matrix will be stored and the cached result will be
## returned when the inverse for that matrix is called again.
## This is a technique similar to that of memoization.

## This function will initialize an empty matrix and create a 
## list of functions that do the following:
## 1. Sets the matrix
## 2. Gets the matrix
## 3. Sets the inverse
## 4. Gets the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL 
      set <- function(y) {
            x <<- y
            inv <- NULL
      }
      get <- function () x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, 
           setinv = setinv, getinv = getinv)
}




## This function will calculate the inverse of the matrix
## inputed in the makeCacheMatrix function. If the inverse
## for that matrix had already been calculated, then it will 
## return the cached inverse and skip the compuation. 
## Otherwise, it will find the inverse and cache that value.

## The argument 'x' is the list from the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)      
        x$setinv(inv)
        inv
}
