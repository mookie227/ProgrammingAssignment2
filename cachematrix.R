## R Programming Assignment 2. Solve a matrix to get the inversion 
##and cache the results

## Create a matrix object and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #set matrix to null
  set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
  } #Can be used to set up a new matrix with different parameters
  get <- function() x   # Return the original matrix
  setinverse <- function(inverse) {m<<-inverse} #solve the matrix to get the inverse and save (cache) it
  getinverse <- function() m #recall cached solved matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #sets up all these functions within the larger function
                                #to be called by cacheSolve as needed
}


## Calculate the inverse of the matrix created using makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse() #try to recall cached solved matrix; if there is one cached
  if(!is.null(inverse)) {   #return it with the note that cached data is used
        message("getting cached data")
        return(inverse)
  }
  data <- x$get() #If there is is not a cached version, solve for inverse, cache it and return value
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
