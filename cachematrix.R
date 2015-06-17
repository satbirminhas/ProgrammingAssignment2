## These functions provide a mechanism to cache the Inverse of a given matrix by 
## utilizing the scoping rules in R

## This function creates a special matrix that contains a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse
## 4. get the value of inverse


makeCacheMatrix <- function(x = matrix()) 
{
  inv <<- NULL
  
  set <- function (y)
  {
    x <<- y           #Set the value of matrix
  }
  
  get <- function ()    #Get the value of matrix
  {
    x
  }
    
  setinv <- function (inverse) 
  {
    inv <<- inverse
  }
  
  getinv <- function ()
  {
    inv
  }
  
#Return a list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function would compute the inverse of the matrix. However, if the 
## inverse is already available in the cache, the value from cache is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
#Get inverse value from the cache  
  inv <- x$getinv()
  
  if(!is.null(inv))   #inverse is available in cache
  {
    message("Getting inverse from cache")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)  #compute the inverse
  x$setinv(inv)
  inv
}
