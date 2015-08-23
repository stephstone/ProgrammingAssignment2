## makeCacheMatrix initializes a list of functions to retrieve and calc matrix inverse, and initializes top level variables.
## cacheSolve retrieves invserse of user-entered matrix if previously cached, or calls function to calc inverse and set new cached result.

## Create list of functions to retrieve and calculate inverse of input matrix

makeCacheMatrix <- function(input=data.frame(input)) {
  x <<- input  ##initialize global value of x (user matrix)
  get <- function() x   ##returns stored value of x 
  setinv <- function(x) ##calculate inverse of matrix x and set inv = result
  {inv <<- solve(x)}
  getinv <- function() inv  ##returns stored inverse of matrix x
  cmds <<- list(get = get, setinv = setinv, getinv = getinv)  ##generate list of functions for use by other functions
}


## calculate inverse of a square matrix, if not already cached
## if already cached (for same matrix), skip the calculation and retrieve cached value

cacheSolve <- function(y, ...) {
y <- as.data.frame(y)
x <- as.data.frame(cmds$get())  ##retrieve matrix used to calculate cahced inverse
test <- all.equal(x,y) ## check if cached matrix and user-entered matrix are equal
if(test[1] == TRUE) {  ## if matrices are equal (and therefore inv previously calc'd), wait to return the cached inverse matrix
  message("getting cached data")
  } else {
    message("new data cached")  ## calculate inverse of user matrix if not previously cached
    makeCacheMatrix(y)  ##set cached matrix to user-entered value
    inv <<- cmds$setinv(y) ##calculate inverse of newly cached matrix, and cache result
  }
cmds$getinv() ##retrieve cached value of matrix inverse
}
