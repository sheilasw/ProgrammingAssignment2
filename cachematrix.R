## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m.x = matrix()) {
        # test that the matrix is square
        var.mdim <- dim(m.x)
        if(var.mdim[1] != var.mdim[2]) {
                message("makeCacheMatrix requires a square matrix (same # rows and # columns")
                exit()
        }
        
        # initializing the matrix variables
        m.orig <- matrix()
        m.inv <- matrix()
        
        # stores the original matrix used. For comparison to see whether the matrix being
        # supplied matches the matrix that was supplied last time
        set.orig <- function(m.y) {
                m.orig <<- m.y
                # if the original matrix is being stored, then it's a new one. Removed cached 
                # inverted matrix
                m.inv <<- NULL
        }
        get.orig <- function() m.orig
        
        set.inv <- function(m.z) {
                m.inv <<- m.z
        }
        get.inv <- function() m.inv
        
        list(set.orig = set.orig,
             get.orig = get.orig,
             set.inv = set.inv,
             get.inv = get.inv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
