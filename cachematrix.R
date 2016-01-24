############ Caching the inverse of a matrix############
# This function creates a special "matrix" object that 
# can cache its inverse. This function contains a list to:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse
# 4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}

# This function take the output of makeCachematrix to
# make a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        
        inv = x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix.data = x$get()
        inv = solve(matrix.data, ...)
        x$setinv(inv)
        return(inv)
}