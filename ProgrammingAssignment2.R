## In this first step, we must create a cached Matrix
## by setting the starting values of the matrix, as well as the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
ma <- NULL
set <- function(y) {
x <<- y
ma <<- NULL
}
get <- function() x
sinverse <- function(inv) ma <<- inv
ginverse <- function() ma
list(
set = set,
get = get,
sinverse = sinverse,
ginverse = ginverse
)
}


## The purporse of the next function is to calculate the inverse of a matrix
## as defined by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
ma <- x$ginverse()
if(!is.null(ma)) {
message("getting cached data")
return(ma)
}
m <- x$get()
ma <- solve(m, ...)
x$sinverse(ma)
ma
}



## These are the commands to test that the inverse matrix is being calculated correctly

myMatrix=makeCacheMatrix(matrix(1:4, nrow=2, ncol=2, byrow=FALSE))
cacheSolve(myMatrix)
