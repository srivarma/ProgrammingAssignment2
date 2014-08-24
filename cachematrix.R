## makeCacheMatrix function creates a matrix object 

makeCacheMatrix <- function(x = matrix()) {
    mat_inv = NULL
    
    #set matrix function
    set <- function(y) {
      x <<- y
      mat_inv <<- NULL       ##initializing the inverse matrix to null
    }
    
    #Get matrix function
    get <- function() x
    
    #Set inverse of matrix function
    setinverse<- function(inverse) mat_inv <<-inverse
    
    #Get Inverse of matrix function
    getinverse <- function() mat_inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The function cacheSolve returns inverse of matrix
## If inverse is avaialbe in cache, cacheSolve function retreives it
## else computes it and sets the cache

cacheSolve <- function(x) {
  
  ## Returns the inverse of matrix x 
  xinv = x$getinverse()
  
  #Checks if cache has any value
  if (!is.null(xinv)) {
    message("Displaying cached inverse of the matrix")
    return(xinv)
  } 
  else {
    xinv = solve(x$get())
    x$setinverse(xinv)
    return(xinv)
  }
}


# Below code can be used to verify the results


# test=makeCacheMatrix(matrix(c(1:4),2,2))
# cacheSolve(test)
# test$get()
# test$getinverse()
# cacheSolve(test) 
