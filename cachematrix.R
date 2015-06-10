##These two functions will calculate and cache an inverse of a matrix
##makeCacheMatrix mainly create a special matrix object then cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  #simply set the value y to x
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  #simply return the value x being set
  get <- function()x 
  
  #set the inverse of matrix into m
  setInverse <- function(matrix) m <<- matrix
  
  #get the stored value of m
  getInverse <- function()m  
  
  list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
}

##cacheSolve will attempt to get the cached value of the inversed matrix which is x
##If the cache is empty, then it will calculate the inverse of x then set it into cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    #if the cache is not empty
    if(!is.null(m))
    {
      message("getting cached inverse matrix")
      return(m)
      
    }    
    data <- x$get()
    
    #set m as inverse of matrix value
    m <- solve(data, ...)
      
    #set m into the cache
    x$setInverse(m)
  
    m
}
