## This file contains two functions.
## The first function makeCacheMatrix expects a square matrix as a parameter 
## and creates a cached version of the matrix
## The second function cacheSolve accepts the output of makeCacheMatrix and will compute
## the inverse of the matrix and store it in the cache.  The function first tests for the
## exsitence of the inverted matrix and will only calculate it if it does not exist.


## Create a cached matrix from the matrix passed as parameter x
## Return a list of functions for setting and getting the cached matrix
makeCacheMatrix <- function(x = matrix()) {
    ## Create a NULL variable to assign the required functions
    inv <- NULL
    
    ## the set function creates the cached matrix using the <<- operator
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## The get function is used to retrieve our cached matrix
    get = function() x
    
    ## The setinv function will store the inverted matrix in the cached variable
    setinv = function(inverse) inv <<- inverse 
    
    ## The getinv function will retrieve the inverted matrix 
    getinv = function() inv
    
    ## The return value is a list containing all the functions defined above
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve will test for the existence of a cached matrix.  If it exists it will return
## the cached matrix
## if the cached matrix does not exist, the inverse of the passed matrix will be calculated
## using the solve function and stored in the cache
## This function expects a square matrix as its parameter.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## use the getinv function defined above to retrieve any cached matrix
    inv = x$getinv()
    
    # if getinv returns a non NULL value, then the inverse has already been calculated
    if (!is.null(inv)){
        # The inverse was already calculated so return it and skip computing again. 
        return(inv)
    }
    
    # getinv() returned a NULL value so we need to calculate the inverse using solve()
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    # Set the value of the calculated inverse to the cached matrix.
    x$setinv(inv)
    
    ## Return the inverted matrix
    return(inv)
    
}
