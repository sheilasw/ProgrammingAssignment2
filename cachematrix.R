## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m.x = matrix()) {
        # test that the matrix is square
        var.matrixdim <- dim(x)
        if(var.matrixdim[1] != var.matrixdim[2]) {
                message("makeCacheMatrix requires a square matrix (same # rows and # columns")
                exit()
        }
        
        # stores the original matrix used. For comparison to see whether the matrix being
        # supplied matches the matrix that was supplied last time
        set.orig <- function(m.y) {
                matrix.orig <<- m.y
                # if the original matrix is being stored, then it's a new one. Removed cached 
                # inverted matrix
                matrix.inv <<- NULL
        }
        get.orig <- function() matrix.input
        
        set.inv <- function(m.z) {
                matrix.inv <<- m.z
        }
        get.inv <- function() matrix.inv
        
        list(set.orig = set.orig,
             get.orig = get.orig,
             set.inv = set.inv,
             get.inv = get.inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
