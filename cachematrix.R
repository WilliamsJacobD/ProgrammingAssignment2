## makeCacheMatrix creates a list of four functions as an argument
## for cacheSolve.
## cacheSolve finds the inverse of a matrix and caches it, unless its
## already been computed and then the cached inverse is returned.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(matrixy) {
                x <<- matrixy
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inversy) {
                inv <<- inversy
        }
        
        getinv <- function() inv
        
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## The outer function makeCacheMatrix creates a list of length four each
## member of which itself another function. The first can be called to store a matrix
## inside the function list, and the second can be used to retrieve that
## matrix. The third function can be used to store the inverse of the original
## matrix, and the fourth can be used to retrieve that inverse This results in
## an object that while technically is a list, can act as a matrix and its 
## inverse. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        if(is.null(inv)) {
                message("Ugh, this could take awhile...")
                matrixy <- x$get()
                inversy <- solve(matrixy)
                x$setinv(inversy)
                return(inversy)
        }
        else {
                message("Ooh! I found a shortcut!")
                return(inv)
        }
        
}
