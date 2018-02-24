## Initialize x and m.
## Assign input to x in parent environment. Assign NULL to m.
## Define the getter for matrix x.
## Define the setter for the inverse.
## Assign functions within array to place in parent environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        array(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Pass above arguments into function.
## Retrieve inverse of passed object.
## Test if NULL.
## If FALSE, calculate new inverse.  IF TRUE, return cached value.

cacheSolve <- function(x, ...) {
        m <- getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- get()
        m <- solve(data, ...)
        setsolve(m)
        m
}

