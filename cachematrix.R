# makeCacheMatrix: This function creates a special
# "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # initially set inverse matrix to NULL
    m <- NULL
    
    # store a matrix
    set <- function(y){
            x <<- y
            # flush the cache
            m <<- NULL
    }
    
    # get the values of stored matrix
    get <- function() x
    
    # cache given argument
    setInvMatrix <- function(imatrix) m <<- imatrix
    
    # get the cached value
    getInvMatrix <- function() m
    
    list(set = set, get = get,
    setInvMatrix = setInvMatrix,
    getInvMatrix = getInvMatrix)
}

# cacheSolve: This function computes the inverse
# of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated
# (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
        m <- x$getInvMatrix()
        
        # does inverse matrix exist?
        if(!is.null(m)){
            message("getting cached data")
            # if so, return cached data
            return(m)
        }
        # otherwise calculate inverse matrix
        # and store it in cache
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setInvMatrix(m)
        m
}

# Unit Test
ut<-makeCacheMatrix()
ut$set(matrix(runif(16, 5.0, 7.5),4,4))
cacheSolve(ut)
cacheSolve(ut)
