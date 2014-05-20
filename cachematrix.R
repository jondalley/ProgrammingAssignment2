## make CacheMatrix is an edit of makeVector function provided
## `makeVector` created a special "vector", 
## which was really a list containing a function to
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the mean
## 4.  get the value of the mean

## the function now works in a similar way for 1 and 2
## 3 and 4 now respectively set and get the value of the 
## solved/inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(s) {
                x <<- s
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## edit of cachemean function provided
## This function checks whether the inverse of the output of
## makeCacheMatrix has previously been cached. If found then 
##it returns the result with the comment that it is getting  
## cache data.
## If not then it calculates the inverse of the output of 
## makeCacheMatrix, stores it in cache, and displays the inverse.

cacheSolve <- function(x, ...) {
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}