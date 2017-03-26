#My assignment is to write a pair of functions that cache the inverse of a matrix.

#Example to run the program:
#a<-matrix(c(2,4,3,1,5,7,4,8,9), nrow=3, ncol=3, byrow=TRUE)
#k<-makeCacheMatrix(a)
#cacheSolve(k)
#cacheSolve(k)

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#makeCacheMatrix is really a list containing a function to set the value of the matrix,
#get the value of the matrix, set the value of the matrix, get the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#cacheSolve calculates the inverse of the special matrix created with the the solve function in R.
#if X is a square invertible matrix, then solve(X) returns its inverse.
#However, it first checks to see if the inverse matrix has already been done. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse 
#in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
