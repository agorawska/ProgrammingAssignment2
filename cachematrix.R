## Function cacheSolve calculates inverse of a given matrix with assumption that the matrix supplied is always invertible.
## The inverse is caculated and cached, so that with the next call of the cacheSolve funtion
## already cache inversed will be returned (if the matrix has not changed and inversed has been already calculated).
## The cachceSolve function calls makeCacheMatrix that represents a special matrix which can store its inverse.
## This enables to reduce number of costrly computation performed, when their results has been already calculated and can be shared.

##Creates a special matrix, which is a representation that enables storing (caching) its inverse.  

makeCacheMatrix <- function(x = matrix()) {
	    inversedMatrix <- NULL
        set <- function(y) {
                x <<- y
                inversedMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(newInverse) inversedMatrix <<- newInverse
        getinverse <- function() inversedMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return inverse of a matrix: 
## compute inverse when this specific inverse was not calculated previously or returned already cached inversion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inversedMatrix <- x$getinverse()
        if(!is.null(inversedMatrix)) {
                message("getting cached data")
                return(inversedMatrix)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(inversedMatrix)
        inversedMatrix
		
}
