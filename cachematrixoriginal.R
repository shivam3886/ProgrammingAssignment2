## Put comments here that give an overall description of what your
## functions do

## make matrix assign to variable x, and initialize m to NULL

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {           =
        x <<- y                    
        m <<- NULL                
    }
    get <- function() x
    setInvmatrix <- function(InvMatrix) m <<- InvMatrix
    getInvmatrix <- function() m
    list(set = set, get = get,
         setInvmatrix = setInvmatrix,
         getInvmatrix = getInvmatrix)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    m <- x$getInvmatrix()              
    if(!is.null(m)) {           
        message("getting cached data")  
        return(m)              
    }
    data <- x$get()             
    m <- solve(data, ...)       
    x$setInvmatrix(m)            
    m                           
}
