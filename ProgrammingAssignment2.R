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





myMatrix=makeCacheMatrix(matrix(1:4, nrow=2, ncol=2, byrow=FALSE))
cacheSolve(myMatrix)