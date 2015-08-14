## makeCacheMatrix makes a "special" matrix object that can store inverse
## of the matrix. Then cacheSolve uses that "special" matrix and prints the 
## inverse of the matrix.

## makeCachematrix is a matrix object with four functions (contains data of
## the matrix and the inverse of the matrix)
## 1) set - set a new value to the matrix and reset the inversion value
## 2) get - returns the value of the matrix
## 3) setinverse - set a new inverse matrix value to the object
## 4) getinverse - returns the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that calculates the inverse of the "special"
## matrix. First it will see if the matrix has any inverse matrix data 
## stored. If it is not NULL it will print out that value (with a report
## that it did so) and stops. If the value is NULL (not yet calculated)
## then it will calculate a new value and set it to matrix, so next time
## it will have one. Returns the inverse matrix.

cacheSolve <- function(x, ...) {
 	  inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}
