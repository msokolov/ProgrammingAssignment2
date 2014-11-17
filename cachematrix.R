## Provides a caching matrix inverse

## Creates a wrapper around a matrix that caches the result of the inverse
## Usage: 
#
#  m <- makeCacheMatrix (x) creates a new caching matrix-inverter
#  m$get() returns the matrix
#  m$set(matrix) mutates this object, setting a new matrix and clearing the cached inverse
#  m$getm() returns the matrix inverse, caching it
#  m$setm(inverse) sets the cached inverse.  Beware of setting this to an incorrect value.

makeCacheMatrix <- function(m = matrix(), minv = NULL) {

    # returns the original matrix
    get <- function() m

    # returns the inverse of the original matrix
    getm <- function() {
        if (is.null(minv) && !is.null(m)) {
            # compute the inverse of m and cache it
            # unless m is null, return minv, which will be NULL
            # allow other errors to propagate normally
            minv <<- solve(m)
        }
        minv
    }

    # Note: this function is not really useful since you can always create a new cachingMatrixInverter
    set <- function(y) {
        m <<- y
        minv <<- NULL
    }

    # Note: this function is included only as required by the assignment;
    # I would advise removing it since it exposes internal details of this
    # object that are not required in order for it to fulfil its stated 
    # purpose
    setm <- function(inv) minv <<- inv

    list(set = set, get = get, setm = setm, getm = getm)
}


## Returns the matrix inverse of the caching matrix inverter x.
#  If x is not a caching matrix inverter (or does not have a getm
#  function at any rate), an error will be raised

cacheSolve <- function(x, ...) {
    x$getm()
}
