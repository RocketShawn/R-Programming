makeCacheMatrix<- function(x = matrix()) {
  ma <- NULL
  set <- function(y) {
    x <<- y
    ma <<- NULL
  }
  get <- function() x
  createinverse <- function(inv) ma <<- inv
  retrieveinverse <- function() ma
  list(
    set = set,
    get = get,
    createinverse = createinverse,
    retrieveinverse = retrieveinverse
  )
}




cacheSolve <- function(x, ...) {
  ma <- x$createinverse()
  if(!is.null(ma)) {
    message("getting cached data")
    return(ma)
  }
  m <- x$get()
  ma <- solve(m, ...)
  x$createinverse(ma)
  ma
}