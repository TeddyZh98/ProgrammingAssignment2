## This is a pair of function that cache the inverse of a matrix.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse later.

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize and store the inverse
    m<-NULL
    ## Set the matrix
    set<-function(y){
    x<<-y
    m<<-NULL
    }
    ## Get the matrix
    get <- function() return(x);
    ## Set and get the inverse of the matrix
    setinverse <- function(inverse) {
    m <<- inverse
    }
    getinverse <- function() return(m);
    ## Return a list of former 4 methods
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## Return the inverse if it has already been calculated
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        ## Get the matrix
        data <- x$get()
        ## Calculate the inverse with the solve function
        m <- solve(data, ...)
        ## Set and return the inverse of the matrix
        x$setinverse(m)
        m
}
