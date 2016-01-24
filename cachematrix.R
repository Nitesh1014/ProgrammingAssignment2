## makeCacheNatrix creates a list of functions to
## 1. Set the value to a matrix
## 2. Get the value of the matrix
## 3. Set h inverse f the matrix
## 4. get the inverse of the matrix
## CacheSolve gets the inverse of the matrix if it exists in 
## cache else calculates the value


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function () inv 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}




cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  inv
        
}
