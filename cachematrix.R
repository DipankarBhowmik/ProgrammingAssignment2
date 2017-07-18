## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                                 #get the value of the matrix
  setInverse <- function(inverse) inv <<- inverse     #set the value of the invertible matrix
  getInverse <- function() inv                        #get the value of the invertible matrix
  ##create list with methods for get/set of both original matrix 
  ##and its inverse and return to list of parent environment. 
  ##this allows use of $ operator to access each function from list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){                                  #if inverse is not NULL
    message("getting cached data")                    #Type message: Getting Cached Matrix
    return(inv)                                       #return invertible matrix
  }
  data <- x$get()                                     #get the original matrix data
  inv <- solve(data)                                  #use solve() to inverse the matrix
  x$setInverse(inv)                                   #set the invertible matrix
  inv                                                 #return a matrix that is the inverse of x
}
