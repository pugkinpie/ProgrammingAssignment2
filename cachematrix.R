
 ##Create special matrix object with functions that allow for caching of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    cacheinverse <- NULL
    getmatrix <- function() {
        x
    }
    setinverse <- function(inverse) {
        cacheinverse <<- inverse
    }
    getinverse <- function() {
        cacheinverse
    }
    list(setinverse=setinverse, getmatrix=getmatrix, getinverse=getinverse)

}

 ##Returns an inverse of the matrix stored in special matrix object created in makeCacheMatrix()

cacheSolve <- function(matrixobject, ...) {
    inverse <- matrixobject$getinverse()
    if(!is.null(inverse))  {
        return(inverse)
    }
    data <- matrixobject$getmatrix()
    inverse <- solve(data, ...)
    matrixobject$setinverse(inverse)
    inverse
}
