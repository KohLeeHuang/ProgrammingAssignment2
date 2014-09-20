## Develop a pair of functions that creates a special "matrix" object that can cache its inverse

## Creates a special "matrix" object (square and invertible matrix)
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialise an object to store the inverse of the matrix
        m <- NULL
        
        ## Set the matrix
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        
        ## Get the matrix
        get <- function () x
        
        ## Set the inverse of the matrix
        setinverse <- function(solve) m <<- solve
        
        ## Get the inverse of the matrix
        getinverse <- function() m
        
        ## Return a list of the defined variables
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Creates a function which computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, this function will retrieve it from the cache instead.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## If inverse of 'x' has already been calculated, retrieve it from cache 
        if(!is.null(m)) {
                message ("getting cached data")
                return(m)
        }
        
        ## If inverse of 'x' has not been calculated
        ## Get the matrix
        data <- x$get()
        
        ## Calculate the inverse of matrix
        m <- solve(data, ...)
        
        ## Set the inverse of the matrix
        x$setinverse(m)
        
        ## return the inverse of the matrix
        m
}

## Creates a vector that store the above two functions
mcacheinv <- function(x) {
        m <- makeCacheMatrix(x)
        y <- cacheSolve(m)
        y
}
