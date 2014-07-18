## These fuctions calculate the inverse of a matrix and store it in a cache.
## 

## This function creates a list which contains a function to:
## 1 - set the matrix
## 2 - get the matrix
## 3 - set the inverse of the matrix
## 4 - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) i <<- solve
        
        getinverse <- function() i
        
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}





## The following function calculates the inverse of the matrix
## created with the above function. However, it first checks to
## see if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse of the matrix and sets that  
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        
        i <- solve(data, ...)
        
        x$setinverse(i)
        
        i
        
}

