## The function makeCacheMatrix stores a matrix into memory, 
#and creates a list of objects that will allow one to store the solution to a matrix  (the inverse), 
#once it has been computed. It does not solve the matrix.
#The function cacheSolve solves the matrix (calculates the inverse of the matrix), and caches it into memory 
#if it has not already been cached. Then you can return to your previous function and call the inverse of the matrix using makeCacheMatrix.

## This function creates a cache for a supplied matrix, and creates a list that will eventually store the inverse of the matrix once it has been computed (below).


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




# This function solves the matrix (i.e. calculates the inverse of the matrix). 
# Then, if the matrix m is NOT null, it retrieves the cache for m.
# If the matrix m is null, then the function caches the solution (i.e. stores the inverse of the matrix as m, which was NULL above).

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

