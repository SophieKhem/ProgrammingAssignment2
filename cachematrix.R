## Function to create a special matrix that caches its inverse and
## to retrieve inverse if inverse has already been computed and matrix is the same.



makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that can cache its inverse.
        m <- NULL 
        set <- function(y) { # set matrix in cache
                x <<- y
                m <<- NULL
        }
        get <- function() x # get matrix from cache
        setinverse <- function(solve) m <<- solve # computes inverse of matrix, and set in cache
        getinverse <- function() m # gets value of the inverse from cache
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        ## If the inverse has already been calculated (and the matrix has not changed), 
        ## then the cachesolve should retrieve the inverse from the cache.
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()
                if(!is.null(m)) { # retrieves inverse from cache if matrix is the same
                        message("getting cached data")
                        return(m)
                }
                data <- x$get() # returns inverse
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }

