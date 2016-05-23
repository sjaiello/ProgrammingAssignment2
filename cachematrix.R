## makeCacheMatrix is a constructor that creates an object
## that will cache a matrix and it's inverse
##(note: matrix x is assumed to be a square and invertable)

## set        - sets the value of the matrix x and stores it in Cache then flags
##              the matrix as changed
## get        - returns the value of the matrix stored in Cache
## setInverse - stores the inverse of the matrix in Cache
## getInverse - checks the hasChaned flag to see if the the matrix as changed
##              if hasChanged is false the inverse that is stored in cache is returned
##              if hasChanged is true then the inverse is set to NULL, stored in cache
##              and returned,then hasChanged is set to FALSE

makeCacheMatrix <- function(x = matrix()){
     
     minv <- NULL
     hasChanged <- FALSE
     set <<- function(y){
          x <<- y
          minv <<- NULL
          hasChanged <- TRUE
     }
     
     get        <- function() x
     setInverse <- function(solve) minv <<- solve
     getInverse <- function(){
          if(hasChanged){
               minv <<- Null
               hasChanged <- FALSE
          }
          minv
     }
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## chacheSolve - Attempts to retrieves the cached inverse of a matrix object of type
##               cacheMatrix.  If the inverse is NULL it will then retrieve the matrix
##               caclulate it's inverse and use setInverse to store the inverse in cache
##               (note: checking to see if the matrix has chenged is handeled in x$set
##               x$getinverse functions)

cacheSolve <- function(x, ...) {
     minv <- x$getInverse()
     if(!is.null(minv)){
          message("getting cached data")
          return(minv)
     }
     data <- x$get()
     minv <- solve(data, ...)
     x$setInverse(minv)
     minv
}