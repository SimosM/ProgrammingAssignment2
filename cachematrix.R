## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## cached_inv is the cached value where we assign the inverse matrix
    cached_inverse_matrix <- NULL
    
    ## SET function: 
    set <- function(arg1_set) {
        ## assign the matrix "arg1_set" to the variable "x"
        x <<- arg1_set 
        ## assign NULL to the variable "cached_inverse_matrix"
        cached_inverse_matrix <<- NULL 
    }
    
    ## GET function: retrieve the matrix
    get <- function() {
        x
    }
    
    ## SETINVERSE function: 
    ## assign the matrix "arg1_setinverse" to the variable "cached_inverse_matrix"
    setinverse <- function(arg1_setinverse) {
        cached_inverse_matrix <<- arg1_setinverse
    }
    
    ## GETINVERSE function: when called it retrieves the cached inverse matrix variale value
    getinverse <- function() {
        cached_inverse_matrix
    }
    ## return list of 4 functions
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## The argument x is a list which is created calling the makeCacheMatrix function
    
    ## look whether the inverse matrix is cached
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## Otherwise , calculate now thw inverse matrix and generate the cache
    else {
        ## first, retrieve the matrix and assign it to the "my_matrix" variable 
        my_matrix <- x$get()
        ## calculate the inverse matrix using the "solve()" function
        inv <- solve(my_matrix)
        ## call the setinverse() function to generate the cached inverse matrix
        x$setinverse(inv)
        ##return value
        return(inv)
    }
}

