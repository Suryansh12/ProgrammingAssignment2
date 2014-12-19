##makeCacheMatrix() and cachesolve() are collectively used to calculate
##and cache the inverse of a matrix.


##makeCacheMatrix function is used to create a special "matrix" object that can 
##cache its inverse. The created object is of type "list" and store 2 things i.e.
##original matrix's & cached inverse value. set,get,setinverse,getinverse functions
##are used to read(get) and change(set) the 2 values.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(M_inv) {
                inv <<- M_inv
        }
        getinverse <- function(){
                inv     
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##cachesolve function computes the inverse of the special "matrix" object returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve retrieves the inverse
##from the cache.
cachesolve <- function(mat, ...) {
        M_inv <- mat$getinverse()
        if(!is.null(M_inv)) {
                message("obtaining cached data")
                return(M_inv)
        }
        
        M <- mat$get()
        M_inv <- solve(M)
        mat$setinverse(M_inv)
        M_inv
}
