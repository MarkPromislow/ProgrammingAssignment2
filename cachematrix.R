## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m_matrix = matrix()) {
    m_inverse <- NULL
    set <- function(m) {
        m_matrix <- m
        m_inverse <- NULL;
    }
    get <- function() { m_matrix }
    setinverse <- function(inverse) { m_inverse <<-inverse }
    getinverse <- function() { m_inverse }
    
    list(set = set, get = get, setInverse = setinverse, getInverse = getinverse)
}


## Write a short comment describing this function

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

