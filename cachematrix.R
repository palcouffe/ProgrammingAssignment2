################################################################################
## makeCacheMatrix fuction creates a special "matrix" object that can cache its
## inverse.
##
##  More specifically, the general goal of makeCacheMatrix is double
##   1/ declare 2 variables 
##      x (actually the makeCacheMatrix arg) : a matrix
##      CachedInvMatrix : an object to cache (initialized to NULL)
##                        there is actually nothing forcing its type to be matrix
##   2/ declare and return set/get functions to manage these 2 variables
##      The 2 variables being declared in the parent environment of the functions 
##      they will be set by those functions using the <<- operator
##
################################################################################
## cacheSolve function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix`. If the inverse has already been calculated (and the matrix
## has not changed), then `cacheSolve` will retrieve the inverse from the cache
##
## To do so cacheSolve :
##   1/ checks in the "cache matrix" if an inverted matrix has been cached and if
##      so returns it as result. This is done using the getInvMatrix function 
##      from the special "matrix" object
##   2/ if no inverse matrix has been cached
##      - uses solve to compute the inverse matrix of the matrix in the "special
##        matrix
##      - caches the result (inverse matrix) in the "special matrix" using the
##        setInvMatrix function from the special "matrix" object
##      - returns the result
##
################################################################################

################################################################################
## makeCacheMatrix fuction creates a special "matrix" object that can cache its
## inverse.
##
##  makeCacheMatrix function
##   1/ takes as argument a matrix defaulted to matrix()
##   2/ returns a list of 4 functions
##       set : set the value of the matrix 
##       get : get the value of the matrix
##       setInvMatrix : set the value of an object to cache
##       getInvMatrix : get the value of a cached object
################################################################################

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize Cache and return list of 4 functions set/get
        ## to manipulate matrix and cached object
        
        ## Init phase
        ## Initialize to NULL the variable that will cache the inverted matrix
        ## This variable will be set/get using setInvMatrix and getInvMatrix
        CachedInvMatrix <- NULL
        
        ## Define the function to set the matrix to invert and initialize the cache
        ## The matrix will be actually assigned to x in the function parent env
        set <- function(y) {
                x <<- y
                CachedInvMatrix <<- NULL
        }
        
        ## Define the function to get the matrix to invert
        ## This will return the value of x from the fonction parent env
        get <- function() x
        
        ## Define the function to cache a matrix 
        ## by assigning it to the variable CachedInvMatrix from the 
        ## function parent env
        setInvMatrix <- function(InvMatrix) CachedInvMatrix <<- InvMatrix
        
        ## Define the function to get a cached a matrix 
        ## by return the value of CachedInvMatrix from the function parent env
        getInvMatrix <- function() CachedInvMatrix
        
        ## Return the 4 defined functions in a list as result
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}

################################################################################
# cacheSolve function computes the inverse of the special "matrix" returned by 
# `makeCacheMatrix`. If the inverse has already been calculated (and the matrix
# has not changed), then `cacheSolve` should retrieve the inverse from the cache
#
# cacheSolve function
#   1/ takes as argument a "cache matrix", that is a "special matrix" built
#      through the makeCacheMatrix function
#   2/ returns the inversed matrix of the matrix set in the "special matrix"
################################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Get the Matrix cached in "cache matrix" x - if any
        GotCachedInvMatrix <- x$getInvMatrix()
        
        ## If there is a non null value there 
        ## no need to compute just return that value
        if(!is.null(GotCachedInvMatrix)) {
                message("getting cached data")
                return(GotCachedInvMatrix)
        }
        
        ## There is no value cached
        ## Get the matrix to be inversed using the get function from the "cache matrix"
        MatrixToInvert <- x$get()
        
        ## Do compute the inverse matrix using solve
        ResultInvMatrix <- solve(MatrixToInvert,MatrixToInvert, ...)
        
        ## Cache the result in the "cache matrix" using its set function
        x$setInvMatrix(ResultInvMatrix)
        
        ## Return the inverse matrix
        ResultInvMatrix
}

############################### TEST SCRIPT ####################################
## > my_cache_matrix <- makeCacheMatrix()
## > my_cache_matrix$set(matrix(1:4,2,2))
## > my_cache_matrix$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > my_cache_matrix$getInvMatrix()
## NULL
## > cacheSolve(my_cache_matrix)
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > cacheSolve(my_cache_matrix)
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > 
################################################################################