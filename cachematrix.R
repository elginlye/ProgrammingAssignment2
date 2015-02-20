## makeCacheMatrix creates a special object that contains a matrix, 
## 4 helper functions, and a vector containing those 4 helper functions 
##
## cacheSolve returns the inverse of the input parameter x, 
## which is a special matrix created by makeCacheMatrix.


## func makeCacheMatrix  
## - takes in 1 input parameter, matrix x 
## - defines a variable m to store the cached matrix
## - defines 4 funcs: set, get, setMatrix, getMatrix
## - returns a vector containing these 4 funcs  
makeCacheMatrix <- function(x = matrix()) {
    ## define variable m to store the cached matrix
    m <- NULL    
    
    ## 1st func, assigns variables x,m in the parent env to y & NULL respectively
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## 2nd func, return variable x defined in parent env, i.e. func makeCacheMatrix 
    get <- function() x
    
    ## 3rd func, assign m (in parent env) to passed-in variable matrix
    setMatrix <- function(matrix) m <<- matrix
    
    ## 4th func, returns m defined in parent env
    getMatrix <- function() m
    
    ## create a list of 4 members, each member is assigned to 1 of the 4 functions
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## func cacheSolve 
## - defines input parameter x, (x is to be created by makeCacheMatrix)
## - returns the cached value if previous computation exists, 
##   otherwise compute the inverse & assign it to variable m and
##   returns the inverse   
cacheSolve <- function(x, ...) {
    ## get the matrix in x
    m <- x$getMatrix()
    
    ## returns inverse if previously cached. 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## computes inverse as no previous cached value exists
    data <- x$get()
    m <- solve(data, ...)
         
    x$setMatrix(m)   ## caches computed inverse in x
    m   ## returns computed inverse
}
