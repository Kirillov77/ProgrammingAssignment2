## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the inverse matrix value
  inverse <- NULL
  # set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # get the data of matrix
  get <- function() x
  
  # set inverse of the matrix
  setinverse <- function(t) inverse <<- t
  
  # get the inverse of the matrix
  getinverse <- function() inverse
  
  # return the Cachematrix,a list of 4
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x  , ...){
        ## Return a matrix that is the inverse of 'x'
  i  <- x$getinverse()
  
  # gets the inverse via getinverse function from the cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # calculates the inverse and sets the value of the inverse
  # get the raw matrix data
  data <- x$get()
  # calculate the inverse of the matrix
  i <- solve(data, ...)
  # save it to the cache
  x$setinverse(i)
  # return the inverse
  i
}

###prova  bbf
x <- matrix(1:4, nrow=2, ncol=2)
m <- makeCacheMatrix(x)
s <- cacheSolve(m)
print(s)
