# These functions work in tandem to invert and cache the inverted form of an input matrix.
# makeCacheMatrix stores information on the matrix and performs the caching. cacheSolve takes as its
# input the output from makeCacheMatrix and does one of the following:
# 1) if the inverted matrix is cached, it returns the cached value.
# 2) if the inverted matrix is not cached, it creates the inverted matrix, caches it, and returns it.
# In order to make this possible, makeCacheMatrix has functions to get the original input matrix, 
# to retrieve the cached inverted matrix and to cache the inverted matrix.

# These functions work in tandem to invert and cache the inverted form of an input matrix.
# makeCacheMatrix stores information on the matrix and performs the caching. cacheSolve takes as its
# input the output from makeCacheMatrix and does one of the following:
# 1) if the inverted matrix is cached, it returns the cached value.
# 2) if the inverted matrix is not cached, it creates the inverted matrix, caches it, and returns it.
# In order to make this possible, makeCacheMatrix has functions to get the original input matrix, 
# to retrieve the cached inverted matrix and to cache the inverted matrix.

makeCacheMatrix <- function(m.x = matrix()) {
        
        # test that the input is actually a matrix
        if(!is.matrix(m.x)) {
                return(message("The makeCacheMatrix function requires a matrix as its input."))
        }
        
        # test that the matrix is square
        var.mdim <- dim(m.x)
        if(var.mdim[1] != var.mdim[2]) {
                return(message("makeCacheMatrix requires a square matrix (same # rows and # columns."))
        }
        
        # test that the matrix doesn't include any NAs
        if(sum(is.na(m.x)) != 0) {
                return(message("makeCacheMatrix can't work with a matrix that has NA values."))
        }
        
        # test that the matrix is numeric
        if(!is.numeric(m.x)) {
                return(message("makeCacheMatrix requires a numeric matrix."))
        }
        
        # initialize the matrix variables
        m.orig <<- m.x
        m.inv <- matrix()
        
        # get the input matrix for inversion
        get.orig <- function() m.orig
        
        # cache the inverted matrix
        set.inv <- function(m.z) {
                m.inv <<- m.z
        }
        
        # get the inverted matrix
        get.inv <- function() m.inv
        
        # create the list "matrix"
        list(get.orig = get.orig,
             set.inv = set.inv,
             get.inv = get.inv)
        
}

# Return a matrix that is the inverse of the original matrix
cacheSolve <- function(l.x, ...) {
        # Confirm that the input is a list as expected
        if(is.list(l.x) == FALSE) {
                v.msg = paste("cacheSolve requires a list as input. Run makeCacheMatrix() and store",
                              "it to a variable to use as input to cacheSolve, or wrap cacheSolve",
                              "around makeCacheMatrix.")
                return(message(v.msg))
        }
        
        # Get the cached matrix - currently throwing a warning because it only compares what's in [1,1]
        m.cache.inv = l.x$get.inv()
        
        if(sum(is.na(m.cache.inv) == 0)) {
                # inverted matrix is cached so we return the cached value
                message("Returning cached inverted matrix.")
                return(m.cache.inv)
        }
        
        # The inverted matrix isn't in the cache, so now we invert it
        m.to.invert = l.x$get.orig()
        m.inv.new = solve(m.to.invert)
        # And cache it
        l.x$set.inv(m.inv.new)
        # And return it
        m.inv.new
}

