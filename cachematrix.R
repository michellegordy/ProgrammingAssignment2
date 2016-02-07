#Michelle Gordy


## Put comments here that give an overall description of what your
## functions do

#The corresponding functions create a cache of an inverted matrix and saves computing time if the inverse of the matrix has already been calculated

## Write a short comment describing this function

#This function creates the cache 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

#This function looks to see if the inverse of the matrix has already been calculated and found in the cache, if so, it returns the value from the cache with the text "getting cached data" and if not, calculates new inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix)
        x$setinverse(m)
        m
}
