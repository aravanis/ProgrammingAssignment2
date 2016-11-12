## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

## The second function, `cacheSolve` retuns the inverse of a cached matrix object 
## by firs checking to see if the cache if present. If not, it calculates the inverse
## and then stores this in the cache matrix so that it does not need to be recalcuated
## the next time the inverse is requested.

## <!-- -->


makeCacheMatrix <- function(x = matrix()) {
        ## Returns a list contating a function to 
        ## 1.  set the value of the matrix
        ## 2.  get the value of the matrix
        ## 3.  set the value of the inverse matrix
        ## 4.  get the value of the inverse matrix that contains a matrix that is the inverse of 'x'
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


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
