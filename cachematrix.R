## matrix inversion is usually a costly coputation and there
## may be some benefit to caching the inverse of a matrix rather 
# than computing it repeatedly. These pairs of functions will cache 
# the inverse of a matrix

## This function creats a special "matrix" object that can cache its inverse

makeCacheMatrix<- function(x=matrix()) {
        rev<- NULL
        set<- function(y) {
                x <<- y
                rev <<- NULL
        }
        get<- function() x
        setReverse<- function(reverse) rev<<- reverse
        getReverse<- function() rev
        list(set=set, get=get,
             setReverse=setReverse, getReverse=getReverse)
}


## This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then cacheSolve should retrieve the 
# inverse from the cache

cacheSolve<- function(x, ...) {
        rev<- x$getReverse()
        if (!is.null(rev)){
                message("getting cached data")
                return(rev)
        }
        data<- x$get()
        rev<- solve(data,...)
        x$setReverse(rev)
        rev
}
