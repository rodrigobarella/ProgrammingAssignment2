## makeCacheMatrix is a function that creates a special matrix object that is able to cache its inverse   
## this function takes an argument x and returns a list composed of four functions:

## 1. set - sets the value of a matrix;
## 2. get - gets the value of the set matrix;
## 3. setInverse - sets the inverse matrix 
## 4. getInverse - gets the inverse matrix 
 
makeCacheMatrix <- function(x = matrix()) { 
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    ##assembling a list of the defined functions
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
    
} 


## cacheSolve function is designed to compute the inverse of an input matrix 
## cashed by the object created using function 'makeCacheMatrix'
## cacheSolve firstly check if there is already an inverse matrix. If the matrix has not changed,
## the function retrieves the cached value. If there is not a calculated inverse or a change
## was made to the original matrix, this function recalculates the inverse


cacheSolve <- function(x, ...) {
    
    ##checking if the inverse matrix is already cached and the matrix has not changed
    
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## calculating inverse matrix in case it is still not cached or there has been an update 
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

## Example

## Defining matrix 'a' to have its inverse computed
a <- matrix(c(1,3,5,7),nrow=2,ncol=2)

## create a special 'matrix' object
x <- makeCacheMatrix()

## set matrix 'a' to 'x' object
x$set(a)

## check the set value
x$get()

## getting the inverse of matrix 'a'  
cacheSolve(x)

## retrieve the inverse of matrix 'a' again to check if function 'cacheSolve'
## is using the cached value by object 'x$getInverse()' or recomputing the inverse 
cacheSolve(x)


#update 'x' object with matrix 'b'
b <- matrix(c(1,4,7,3,1,10,4,3,2),nrow=3,ncol=3)

x$set(b)

## getting the updated matrix values
x$get()

##checks if cacheSolve is retrieving the cached inverse from old matrix 'a' or it firstly checks 
## wheter x$get() was updated or not

cacheSolve(x)

##running 'cacheSolve' again to check if the function will use the new cached value of matrix 'b' inverse
cacheSolve(x)
