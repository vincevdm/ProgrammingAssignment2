##two functions are created that caches the inverse of a matrix so as not to recalculate it 
##It's been assumed that the matrix is square and invertible.

##makeCacheMatrix creates the special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(A = matrix()) {
        inv <- NULL
        set <- function(B) {
                A <<- B
                inv <<- NULL
        }
        get <- function() A
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##cacheSolve computes the inverse of a special "matrix" or retrieves the inverse
##if it has already been previously calculated.

cacheSolve <- function(special_A, ...) {
        inv <- special_A$getinv()
        if (!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        M <- special_A$get()
        inv <- solve(M, ...)
        special_A$setinv(inv)
        inv
}
