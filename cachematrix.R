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
        m.orig <<- m.x
        m.inv <- matrix()
        
        # stores the original matrix used. For comparison to see whether the matrix being
        # supplied matches the matrix that was supplied last time. I don't think this is necessary -
        # I can't see how it could be useful in the way this is set up. Remove before final
        set.orig <- function(m.y) {
                m.orig <<- m.y
                # if the original matrix is being stored, then it's a new one. Removed cached 
                # inverted matrix
                m.inv <<- NULL
        }
        
        # get the input matrix for inversion
        get.orig <- function() m.orig
        
        # cache the inverted matrix
        set.inv <- function(m.z) {
                m.inv <<- m.z
        }
        
        # get the inverted matrix
        get.inv <- function() m.inv
        
        list(set.orig = set.orig,
             get.orig = get.orig,
             set.inv = set.inv,
             get.inv = get.inv)
        
}

cacheSolve <- function(l.x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        if(is.list(l.x) == FALSE) {
                message("cacheSolve requires a list as input. Run makeCacheMatrix() and store it 
                        to a variable to use as input to cacheSolve, or wrap cacheSolve around 
                        makeCacheMatrix.")
                exit
        }
        
        # Get the cached matrix
        m.cache.inv = l.x$get.inv()
        if(!is.na(m.cache.inv)) {
                # inverted matrix is cached
                message("Returning cached inverted matrix.")
                return(m.cache.inv)
        }
        
        m.to.invert = l.x$get.orig()
        m.inv.new = solve(m.to.invert)
        l.x$set.inv(m.inv.new)
        m.inv.new
}
