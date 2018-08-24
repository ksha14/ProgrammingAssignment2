## The following functions calculate inverse of a matrix & saves it
## to cache so that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function() x
  
  setinverse <- function(inverse) inv <<- inverse 
  
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv

}


## Test Run:Caching the Inverse of a Matrix

test_matrix <- makeCacheMatrix(matrix(data = c(1:4), nrow = 2, ncol = 2, byrow = TRUE ))
test_matrix$get()

## Inverser data return from computation
cacheSolve(test_matrix)

## inverse data return from the cache in the second run
cacheSolve(test_matrix)
