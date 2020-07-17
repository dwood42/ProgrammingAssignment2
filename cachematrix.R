## These functions calculate and cache the inverse of a matrix passed to 
## makeCacheMatrix(), then retrieve (or, if not cached, create) the inverse
# with cacheSolve().

## makeCacheMatrix() takes an input matrix x, sets and gets its value for this
## iteration of the function. It takes x and m from the parent environment and
## calculates the inverse of x. The values and the functions used to get them
## are stored in a list.

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
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

## Passing the result of makeCacheMatrx() to cacheSolve(), the function then
## checks if an inverse of the matrix has already been calculated and stored in
## m. If so, it retrieves that data. If not, the matrix inverse is calculated.

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