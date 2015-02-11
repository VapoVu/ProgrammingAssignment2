## Put comments here that give an overall description of what your
## functions do.
## The following code computes the inverse of a given matrix
##      temp = makeCacheMatrix(mat)
##      matInv = cacheSolve(temp)


## The makeCacheMatrix function generates a list of functions that are used 
## to get and set value of the matrix (the set() and get() functions), and to 
## set and get value of its inverse (the setinv() and getinv() functions)

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function cacheSolve compute the inverse of a given matrix, if the inverse is alredy computed and 
## saved in the cache, the function will retrieve the inverse without any inverse computation.
## Otherwise, cacheSolve computes the inverse of the matrix using the solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinv(inv)
        
        inv
}
