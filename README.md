### Introduction

This second programming assignment will require you to write an R
function that is able to cache potentially time-consuming computations.
For example, taking the mean of a numeric vector is typically a fast
operation. However, for a very long vector, it may take too long to
compute the mean, especially if it has to be computed repeatedly (e.g.
in a loop). If the contents of a vector are not changing, it may make
sense to cache the value of the mean so that when we need it again, it
can be looked up in the cache rather than recomputed. In this
Programming Assignment you will take advantage of the scoping rules of
the R language and how they can be manipulated to preserve state inside
of an R object.

### Caching the Inverse of a Matrix

In this example we introduce the `<<-` operator which can be used to
assign a value to an object in an environment that is different from the
current environment. Below are two functions that are used to create a
special object that stores a numeric vector and caches its mean.

The first function, `makeCacheMatrix` creates a special "matrix" object that will cache inverse 

1.  set the value of the matrix
2.  get the value of the matrix
3.  set the value of the inverse
4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) 
{
  
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
        

CacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

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


### Test Run: Caching the Inverse of a Matrix
test_matrix <- makeCacheMatrix(matrix(data = c(1:4), nrow = 2, ncol = 2, byrow = TRUE ))
test_matrix$get()

     [,1] [,2]
[1,]    1    2
[2,]    3    4

### Inverser data return from computation
cacheSolve(test_matrix)

    [,1] [,2]
[1,] -2.0  1.0
[2,]  1.5 -0.5

### inverse data return from the cache in the second run
cacheSolve(test_matrix)

getting cached data.
     [,1] [,2]
[1,] -2.0  1.0
[2,]  1.5 -0.5
