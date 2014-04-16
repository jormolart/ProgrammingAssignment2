makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   
# defining the function to store the matrix to memory
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
# defining the function to retrieve the matrix from memory
   get <- function() x
# defining the function to calculate and store the inverse matrix
   setinverse <- function(solve) m <<- solve
# defining the function to retieve the inverse matrix from memory
   getinverse <- function() m
# creating the list of functions
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
# retrieving the inverse matrix from memory
   m <- x$getinverse()
# if the inverse matrix exists in memory send a message and return it
   if(!is.null(m)) {
      message("getting cached matrix")
      return(m)
   }
# retrieve matrix from memory
   data <- x$get()
# calculating the inverse matrix
   m <- solve(data, ...)
# storing the inverse matrix into memory
   x$setinverse(m)
# returning the inverse matrix
   m
}