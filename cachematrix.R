#assigment2

## This file has two functions: makeCacheMatrix, cacheSolve. 
## The first function sets and gets the value of the matrix and tis inverse. 
## The second function search in the cache if the inverse of the matrix 
## is already calcualted, if the inverse is the cahce it retrives the results 
##from the cache, otherwise the function calcualtes the inverse of the matrix.

## This function does:
## Creates an special objects that
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)    
}

## This function does:
## Checks if the invesrse of the matrix is already i the cahce.
## If not, it calcualtes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}


m_example <- matrix(c(3,0,0,0,3,0,0,0,3), 3,3)
m_special <- makeCacheMatrix(m_example)
m_inverse <- cacheSolve(m_special)
