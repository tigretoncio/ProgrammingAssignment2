## makeCacheMatrix This function creates a special "matrix" object that can cache its inverse.
## it allows to store the inverse value of a matrix using its "setinverse" function, also allows to 
## obtain the inverse value from the "cache" with getinverse.  It also sets up the original
## matrix (with function set) and produces it (with function get)





makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        set <- function(y) {
                # matrix value changes on existing makeCacheMatrix object
                x <<- y
                # if matrix changes we need to remove its previous inverted cached value
                inv <<- matrix()    
        }
        get <- function() x
        setinverse <- function(inverted = matrix()) inv <<- inverted
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## cacheSolve: This function first check if a cached version of the inverted matrix is available,
## using getinverse function from makeCacheMatrix function. If a cache version exists,
## it retrieves it and returns it without further calculation. If not, it calculates the
## inverse and stores that value into the cache value (using function setinverse from makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # we first check if inverse is in cache
        inv <- x$getinverse()  
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
