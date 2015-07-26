## Caching the Inverse of a Matrix:
## Matrix inversion can be a consuming computation and it may be
## beneficial to caching the inverse of a matrix rather than compute it repeatedly.
## Below are two functions that create a special object that stores a matrix
## and caches its inverse.


## This first function creates a special "matrix" object whoch cachea its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## This next function computes the inverse of the special "matrix" created earlier 
## by makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

> matrix_a <- makeCacheMatrix(matrix(2:5, 2, 2))

> matrix_a$get()
[,1] [,2]
[1,]    2    4
[2,]    3    5
> matrix_a$getInverse()
NULL
> cacheSolve(matrix_a)
[,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1

> cacheSolve(matrix_a)
getting cached data
[,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1
> matrix_a$getInverse()
[,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1


matrix_a$set(matrix(c(2, 2, 1, 4), 2, 2))
> matrix_a$get()
[,1] [,2]
[1,]    2    1
[2,]    2    4
> matrix_a$getInverse()
NULL
> cacheSolve(matrix_a)
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> cacheSolve(matrix_a)
getting cached data
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> matrix_a$getInverse()
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333