# The following functions create a special matrix with the ability to 
# cache its inverse and then retrieve the cached value.

# `makeCacheMatrix`: This function creates a special "matrix" object
#                    that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL
    #set the value of the special matrix
    set <- function(y) 
    {
        x <<- y 
        i <<- NULL
    }
    
    #get the value of the special matrix
    get <- function() x     
    
    #set the inverse of the special matrix
    setInverse <- function(inverse) i <<- inverse
    
    #get the inverse of the special matrix
    getInverse <- function() i
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#`cacheSolve`: This function computes the inverse of the special
# matrix returned by `makeCacheMatrix` above. If the inverse has
# already been calculated and the matrix has not changed, then
# `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) 
{
      i <- x$getInverse()
      
      if(!is.null(i)) 
      {
              message("getting cached matrix")
              return(i)
      }
     
      matrix <- x$get()
      i <- solve(matrix)
      x$setInverse(i)
      ## Return a matrix that is the inverse of 'x'
      i
}
