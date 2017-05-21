#ARS 05/19/2017. Coursera R-Programming 

#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
invr = NULL
  set = function(y) {
    x <<- y ;  invr <<- NULL
  }
  get = function() x
  setinverse = function(inverse) invr <<- inverse 
  getinverse = function() invr
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
 #ARS 05/19/2017. Coursera R-Programming
  # inverses the given matrix
invr = x$getinverse()
  
  # check the cache
  if (!is.null(invr)){
    return(invr)
  }
  
  # create inverse
  mat.data = x$get()
  invr = solve(mat.data, ...)
  
   x$setinverse(invr)
  
  return(invr)
}




       

