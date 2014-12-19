## These two functions demonstrate how caching in R works for potentially
## costly operation, in this case matrix inversion. 

## makeCacheMatrix function creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	invr <- NULL
	set <- function(y){
		x <<- y
		invr <<- NULL
	}
	get <- function() x
	
	setinverse <- function(inverse) invr <<- inverse
	getinverse <- function() invr
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
    invr <- x$getinverse()
    if(!is.null(invr)) {
        message("getting cached data")
        return(invr)
    }
    data <- x$get()
    invr <- solve(data)
    x$setinverse(invr)
    invr
}
