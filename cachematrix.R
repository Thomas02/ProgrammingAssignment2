## This code first sets and stores a matrix, 
## calculates and stores the inverse of the matrix,
## and then creates a code that checks if the matrix
## inverse has already been calculated and stored 
## if it has then it returns this stored value
## otherwise it calculates and stores the matrix inverse

## This part of the code stores the matrix and its calculated inverse

makeCacheMatrix <- function(x = matrix()){
        
        ## m is initially set to NULL
        m <- NULL
        
        ## this function allows for the cached x value to be reset
        ## in case the matrix has changed and the inverse needs to be recalculated
        set <-function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## this function retrieves the matrix x
        get <- function() x
        
        ## The inverse of the matrix is calculated here and set to solve 
        setinverse <- function(solve) m <<- solve
        
        ## The inverse of the matrix is called
        getinverse <- function () m
        
        ## All the above function are placed in a special list 
        ## so we can call then in later functions
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## This function checks to see if the matrix inverse has already been calculated
## If it has then it returns that cached inverse matrix value
## Otherwise it calculates the inverse and returns the inverse value

cacheSolve <- function(x, ...) {
        
        ## m is set to precalculated inverse if it exists
        m <- x$getinverse()
        
        ## if the cached inverse exists, it is called out here
        if(!is.null(m)){
                message("get cached data")
                return(m)
        }
        
        ## if the cached inverse does not exist then this sets data equal to the matrix
        data <- x$get()
        ## and calculate the inverse of the matrix and save it as m
        m <- solve(data, ...)
        ## and then store the calculated inverse under setinverse in x
        x$setinverse(m)
        ## and return the value of the calculated inverse m
        m
}