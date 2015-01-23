## ============================================================##
## This functions calculate the inverse of a matrix and saves it
## to the cacheed matrix. So the next time you run the function 
## the previously saved value of matrix inverse is returned instead of
## repeating the calculation.

makeCacheMatrix <- function(x = matrix()) { ## this create a matrix object x and its components
  cmt <- NULL    ## sets the cached cmt to NULL
  set <- function(z) {
    x <<- z     ## assign the input matrix z to the variable x 
    cmt <<- NULL 
  }
  get <- function() x         ## get/return the matrix x from the fubction
  setinverse <- function(inverse) cmt <<- inverse ## set the cached cmt to inverse of matrix x
  getinverse <- function() cmt ## return the cached inverse of x
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## This function calculates the inverse of the a "matrix" by first checking if the inverse
## is already calculated. If yes it gets from the chaced cmt, otherwise calculate the inverse
## as new & sets the valuse to the chace for later use (to avoid recalculating later)

cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x'       
  cmt <- x$getinverse()
  if(!is.null(cmt)) {
    message("getting cached data")  # desplaying "getting cached data" message
    return(cmt)
  }
  data <- x$get()
  cmt <- solve(data, ...)  # solves the inverse if not found in the chace
  x$setinverse(cmt)   # sets the value to the chace for later use
  cmt
}

## ============================================================##

