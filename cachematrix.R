## At this code we are going to develope two functions ("makeCacheMatrix" & "cacheSolve") that
## cache the inverse of a matriz

## "makeCacheMatrix"
## This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x<<-y
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function()inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}

## "cacheSolve"
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv      
}

#TEST
x<- matrix(rnorm(9),3,3)
x1<- makeCacheMatrix(x)
cacheSolve(x1)
