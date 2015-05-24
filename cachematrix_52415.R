#--------------------------------------------------------------------------------------------------------
#   Prog Ass 2:cachematrix.R :Focus areas - Use of <<, GIT/GITHUB, cache and Peer Eval.
#   FUnction Description:
#   Matrix Inversion requires to transose the rows into columns and divide
#   each element by its determinant . The critical housepeeking factors
#   for the function to successfully return an inverted matrix are:
#   1.The input matrix MUST be a square matrix 
#   2. The determinant MUST be  a non-zero.
#   Cost considerations of running the function mandate the caching of the result than repeated computes.With this in mind
#   the two functions - MakeCacheMatrix and CacheSolve handshake to ensure a function list and caching are accomplished.
#------------------------------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x=matrix()) {
  m <- NULL  # Initialize the final return value of the inverted matrix for first run.
  set <- function(y) {  # Setting the value of the vector in this function.
    x <<- y    
    m <<- NULL
  }
  get <- function() {   # Getting the value of the input matrix that requires to be inverted.
    x
  }
  setinverse <- function(inverse) { # A non-redundant compute & assign of the inverse of the matrix to a variable m via a <<
    m <<- inverse
  }
  getinverse <- function() { # Capture the return value of the inverse of the matrix
    m
  }
  # Define a list as a container for the functions set,get, setinverse and getinverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) { # Store the inverse in  the cache
  m <- x$getinverse()  # Run the getinverse on the input matrix - x
  if(!is.null(m)) {    # Focus on non-zero values and return to cache only the non-zero inverts.
    message("getting cached data")
    return(m)
  }
  data <- x$get()  # populate the variable data with te matrix ready for inversion.
  
  m <- solve(data, ...)  # A non-redundant inverse compute of the special "matrix" returned 
                         # by makeCacheMatrix above
  x$setinverse(m)
  m                      # Return the inverted matrix m
  
}
