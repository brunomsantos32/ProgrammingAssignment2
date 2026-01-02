#MakeCacheMatrix#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # Store the cached inverse here (starts empty)

  set <- function(y) {
    x <<- y      # Replace the saved matrix
    inv <<- NULL # Clear the inverse because the matrix changed
  }

  get <- function() x  # Return the current matrix

  setinverse <- function(inverse_value) inv <<- inverse_value  # Save the inverse
  getinverse <- function() inv                                # Return the saved inverse

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#CacheSolve#
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()          # Check if an inverse is already stored
  if (!is.null(inv)) {
    message("getting cached data")  # Use the cached result if it exists
    return(inv)
  }

  mat <- x$get()                 # Otherwise, get the matrix
  inv <- solve(mat, ...)         # Compute the inverse using solve()
  x$setinverse(inv)              # Save it for next time
  inv
}

#Example#
a <- diag(5, 3)
a

cachedMatrix <- makeCacheMatrix(a)
cacheSolve(cachedMatrix)

b <- diag(2, 6)
b

cachedMatrix <- makeCacheMatrix(b)
cacheSolve(cachedMatrix)

cacheSolve(cachedMatrix)  # getting cached data
