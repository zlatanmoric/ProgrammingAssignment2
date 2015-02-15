## Calculating inverse of matrix and store it in cache for leter use

## Function creates matrix for cache inverse. 

# sets x to empty matrix
makeCacheMatrix <- function(x = matrix()) {
    
    # inicialisation of TmpInverse
    TmpInverse <- NULL
    
    # set function
    set <- function(y){
        
        # assigns argument of function to x
        x <<- y
        
        # Reset TmpInverse to NULL
        TmpInverse <<- NULL
    }
    
    # get function returns matrix
    get <- function() {
        x
    }
    
    # setInverse function assign argument to TmpInverse
    setInverse <- function(solve){
        TmpInverse <<- solve
    } 
    
    # getInverse function returns the Inverse
    getInverse <- function() {
        TmpInverse
    }
    
    # list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 


## Function computes inverse of matrix. If inverse already exist and matrix not changed return it from cache. 

cacheSolve <- function(x, ...) {
    
    # Recent value from inverse
    TmpInverse <- x$getInverse()
    
    # If value of Inverse is NOT NULL returns that value
    if(!is.null(TmpInverse)){
        message("getting cached data")
        return(TmpInverse)
    }
    else {
        message("calculating inverse")
        data <- x$get()
        TmpInverse <- solve(data, ...)
        x$setInverse(TmpInverse)
        TmpInverse
    }
} 
