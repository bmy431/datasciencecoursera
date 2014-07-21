## These two functions created a cached version of the inversed 
##of an inputted matrix that will 
##be called on by the second function in lieu 
##of recalculating the inverse of the same input matrix


## makeCacheMatrix is a function that creates a list of
## 4 functions that set and hold values for the inverse, solve for the
## inverse and retrieve the cached inverse

makeCacheMatrix <- function(X = matrix()){
        inv <- NULL   ## Set placeholder for inverse matrix
        set <- function(Y){   ## Sets matrix values
               X <<- Y
               inv <<- NULL
        }     
        get <- function() X 
        ## Creates a function that gets the matrix
        setinv <- function(solve) inv <<- solve 
        ##Creates a function that solves the matrix for inverse
        getinv <- function () inv 
        ## Creates a function that gets inverse matrix
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv) 
        ## Puts all of the functions in a list that will
        ## be the input for cacheSolve()
      
}

## cacheSolve is a function that retrieves the cached version
## a matrix if available (instead of recalcuting it)
## or solves forthe inverse of a matrix. It takes the output (od class List
## of the makeCacheMatrix function. 

cacheSolve <- function(X, ...){
        inv <- X$getinv()
        if (!is.null(inv)){
                message("getting cached inverse matrix")
              return(inv)  
        } 
        ## checks to see if a cached inverse can be retrieved
        data <- X$get()
        inv <- solve(data, ...)
        X$setinv(inv)
        inv 
        ## solves for a new inverse and 
        ##caches it for future use
}