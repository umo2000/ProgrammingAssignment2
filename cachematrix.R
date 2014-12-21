## This script will provide you with the inverse of a square matrix
## which is stored in the cache of system to avoid using up memory

## This function will take in your square matrix x, set the inverse to null,
## write over an existing inverse with null if passing in a new matrix,
## then returns the value of matrix that was passed in, 

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL    
    set <- function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x  
    setinv <- function(solve) inv<<-solve 
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    inv
}
