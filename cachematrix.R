## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #ensures that once anything from Makecachematrix is called, the matrix is reset
    set <- function(y) {
        inv <<- NULL 
        x <<- y 
    }
    #set is called to set the matrix and store it in the Cache. 
    #It also clears the inverted matrix in the parent environment.
    #Because there is a new matrix, the value of the inverted matrix needs to be recalculated
    get <- function() x
#Returns the matrix    
    setinv <- function(inverse) inv <<- inverse
#sets the inverse    
      getinv <- function()  inv
#retrieves the inverse if it's been set. Used by Cachesolve    
    list(set = set, get = get,
      setinv = setinv,  getinv = getinv)
#Makes the output of the function a list with each of the enclosed functions as named entities
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  #Calls the matrix created through MakeCacheMatrix to check for a previously stored inverse
  if(!is.null(inv)) {
 #If we already have inv saved, we call it from cache instead of computing it again
    message("Getting Cached Data")
    return(inv)
#We return the cached version of inverse if we have it saved
  }
  
  data <- x$get()
#Use the extract operator to retrieve the data used as an input for MakeCacheMatrix()
  inv <- solve(data,...) 
#solve the matrix for it's inverse, save that to inv 
   x$setinv(inv)
#assign inv in the parent environment (because it was called through the extract operator)   
  inv
#has the function output the inverse at the end  
}
