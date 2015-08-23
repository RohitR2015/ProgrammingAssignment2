# Function creates a Inverse *cache* matrix object which has methods which can be used to create a set, get inverse 
# matrix's in different enviornment thus mocking a cache. 

makeCacheMatrix <- function(x = matrix()) 
{ 
  inv <- NULL 
  # Set function sets the value for cache enviornment 
  set <- function(y) 
  { 
    x <<- y 
    inv <<- NULL 
  } 
  # get - Returns the object 
  get <- function() 
  {
    x
  }
  # Caches the inverse value 
  setinv <- function(inverse) 
  {
    inv <<- inverse 
  }
  # returns the cahed value 
  getinv <- function() 
  {
    inv 
  }
  # function returns a list of methods for object x 
  
  list(set = set, get = get, setinverse = setinv, getinverse = getinv) 
  
} 
  # CacheSolve checks for cached value of inverse matrix and returns inverse matrix Else creates inverse matrix,caches and 
  # returns its inverse 
  cacheSolve <- function(x, ...) 
  {
    # check if cache for Inverse exists, If yes return then the cached value 
    inv <- x$getinv() 
    if (!is.null(inv)) 
      { 
        message("Receiving cached data") 
        return(inv) 
      } 
  # If caching was not done earlier lets do it now 
  # Assign data to variable mat (matrix) 
    matris <- x$get() 
  # Calculate the inverse value 
    inv <- solve(matris) 
  # Set the inverse value to cache memory 
    x$setinv(inv) 
  # return the inverse value 
    return(inv) 
  }
  


