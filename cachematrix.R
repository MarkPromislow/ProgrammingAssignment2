## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##   It has methods
##     set: to set the matrix
##     get: to get the matrix
##     setInverse: to set the inverse of the matrix <= ideally this method would not be callable
##     getInverse: to get the inverse of the matrix

makeCacheMatrix <- function(m_matrix = matrix()) {
    m_inverse <- NULL
    set <- function(m) {
        m_matrix <- m
        m_inverse <- NULL;
    }
    get <- function() { m_matrix }
    setinverse <- function(inverse) { m_inverse <<-inverse }
    getinverse <- function() { m_inverse }
    
    list(set = set, get = get,
         setInverse = setinverse, getInverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##   If the inverse has already been calculated (and the matrix has not changed),
##   then cacheSolve should retrieve the inverse from the cache.
##
##   Ideally this method would not be a separate function, but be included in the getInverse function of
##   makeCacheMatrix and the setInverse function would be eliminated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(is.null(inverse)) {
        message("cacheSolve: solve for matrix inverse")
        inverse <- solve(x$get())
        x$setInverse(inverse)
    }
    inverse
}

## create a test matrix with random numbers
n <- 8
testMatrix <- matrix(data = runif(n * n, 0, 10), nrow = n, ncol = n, byrow = TRUE)

cacheMatrix <- makeCacheMatrix(testMatrix)
inverseMatrix <- cacheSolve(cacheMatrix)
identical(inverseMatrix, cacheSolve(cacheMatrix))
identical(cacheMatrix$getInverse(), inverseMatrix)

## verify results
testMatrix
cacheMatrix$getInverse()
testMatrix %*% inverseMatrix

