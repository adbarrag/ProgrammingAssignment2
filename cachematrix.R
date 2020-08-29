## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix is a function that create a object that will store the matrix that
##cache it inverse. The steps are setting and getting the matrix and then setting and getting the inverse

makeCacheMatrix <- function(x = matrix()) { 
 m <- NULL
set <- function(y) {
                x <<- y
                m <<- NULL
        }
get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
##The second function  calculates the mean of the special matrix created with the makeCacheMatrix function.
##First checks if the INVERSE has already been calculated and if this is true it gets the inverse from the cache and skips the computation. 
##And if it false it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
