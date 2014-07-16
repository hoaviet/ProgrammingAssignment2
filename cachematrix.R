## A pair of functions to support "cached matrices", whose inverses are cached
## for better performance when the inverse is used repeatedly


## makeCacheMatrix creates a cached matrix with 4 methods 
## (set, get, setinv, getinv) and a variable (inv) to cache its inverse

makeCacheMatrix <- function(m = matrix())
{
    inv <- NULL

    set <- function(newvalue)
    {
        m <<- newvalue
        inv <<- NULL     ## inverse needs to be recomputed next time
    }

    get <- function() m

    setinv <- function(newinv) inv <<- newinv

    getinv <- function() inv

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(m, ...)
{
    inv <- m$getinv()
    if (!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    ## else recompute and cache inverse
    data <- m$get()
    inv <- solve(data)
    m$setinv(inv)
    inv
}

