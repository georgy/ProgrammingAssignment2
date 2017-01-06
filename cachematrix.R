## Computing inverse of a square matrix might be a costly operation. These functions
## allow you to wrap a matrix into a datastructure that computes and memoizes 
## inverse matrix using base::solve() function

# Checks if matrix is square
isSquare <- function(m) {
        dim <- dim(m)
        dim[1] == dim[2]
}

## Wraps matrix into inverse memoizing datastructure
makeCacheMatrix <- function(x = matrix()) {
        inverse_ <- NULL
        
        if (!isSquare(x)) {
                warning("Cannot wrap matrix: it must be square")
                NULL
        } else {
                set <- function(y) {
                        if (!isSquare(y)) {
                                warning("Cannot wrap matrix: it must be square")
                        } else {
                                x <<- y
                                inverse_ <<- NULL
                        }
                }
                get <- function() x
                setinverse <- function(inverse) inverse_ <<- inverse
                getinverse <- function() inverse_
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }
}


## Version of solve() function that checks for memoized result first and returns
## cached copy if there is one
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("Getting cached data for inverse")
        } else {
                original_matrix <- x$get()
                inverse <- solve(original_matrix)
                x$setinverse(inverse)
        }
        inverse
}