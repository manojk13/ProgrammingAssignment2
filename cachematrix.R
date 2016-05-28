## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object 
makeCacheMatrix <- function(x = matrix()) {
 inv_m <- NULL
  set <- function(y){
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_m <<- inverse
  getInverse <- function() inv_m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getInverse()
  if(!is.null(inv_m))
  {
    message("Cached data")
    return(inv_m)
  }
  data <- x$get()
  if(nrow(data)==ncol(data)) {
    #unlist(determinant(data))
    if(det(data)==0){
      message("Determinant Value is 0, Matrix not invertible !!")
    } else {
      inv_m <- solve(data)
      x$setInverse(inv_m)
      inv_m
    }
  } else {
    message("Not a square matrix, Matrix not invertible !! ")
  }
}