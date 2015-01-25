## These functions take an inversible matrix as the input and then
## return the inverse of that matrix. The first fucntion stores the
## inverse as a cache and the second function then checks to see if
## there is a cache of the inverse of the original matrix and if 
## there is then it returns the value stored in the cache otherwise
## it then calculates the inverse of the matrix by calling the 
## first function


## The makeCacheMatrix function takes an inversible matrix and it 
## returns a list of functions defined in this makeCacheMatrix 
## fucntion. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- solve(x)
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve matrix takes an input that is a list of the same
## functions that are returned in the makeCacheMatrix function. It
## then checks that list to see if there is a cache of the inverse
## of the matrix that is passed into the makeCacheMatrix function 
## and if there is it returns that cache, otherwise it solves for 
## the inverse and returns that inverse

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
