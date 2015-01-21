##
## Programming Exercise 2 - JH DataScience Specialization
##
## This code shows an example of how the lexical scoping behavior
## of R can be used to implement functions that can save state to
## provide useful behavior. If you are familiar with more formal
## Object Oriented Languages like C++ and Java, this code shows
## how to get the same effect as a "class" with private data, which 
## exposes its features through functions ("methods" in OO parlance).
##
## The specific functionality implemented here is to provide a
## "super" Matrix object that has the ability to cache the calculated
## inverse of a given matrix. This may be useful in an application which
## needs to make repeated use of the inverse of a particular matrix, since
## computing the inverse of a square matrix is a relatively expensive
## operation, especially for matrices of large dimension.
##

##
## makeCacheMatrix creates an "object" that caches two values:
##
##  1) The matrix passed as the first argument
##  2) The inverse of the matrix or NULL if the inverse has
##     not yet been calculated
##
## Syntax
##    matObj = makeCacheMatrix(x)
##
## Arguments
##    x - normal R square numeric matrix
##
## Return Value
##   List of 4 functions (the exported "methods"):
##     set(xMat)
##     xMat = get()
##     setInverse(xMatInv)
##     xMatInv = getInverse()
##

makeCacheMatrix <- function(x = matrix()) {
    #
    # This variable is local to the instantiation of the
    # makeCacheMatrix function ("object"), so that it persists
    # for the life of that object.
    #
    # The value is either NULL (indicating that there is no
    # value being cached at present) or a valid cached value
    # of the current matrix x.
    #
    cachedInverse <- NULL
    #
    # When a new matrix is set, clear the cached inverse value,
    # which will force the inverse to be recalculated the next
    # time cacheSolve is called.
    #
    # The key thing to note here is the use of the assignment
    # operator "<<-", which means that the assignment will not
    # create a local variable in the current function context,
    # but will search upwards to find it in one of the containing
    # environments. In this case, it will be found in the environment
    # of this makeCacheMatrix object. The "x" value that gets
    # assigned is the original "x" that was passed as an argument
    # to the makeCacheMatrix call. Parameters are "call by value"
    # in R and they are stored as local variables in the function's
    # environment.
    #
    privSet <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    #
    # The get() method just returns the current matrix value
    #
    privGet <- function() x
    #
    # The setInverse() method sets the cachedInverse value
    #
    privSetInverse <- function(inverse) cachedInverse <<- inverse
    #
    # Return the current value of cachedInverse from the environment
    #
    privGetInverse <- function() cachedInverse
    #
    # The return value is a list of the "method" functions of the
    # newly created instance of this object type. The names on the
    # left of the "=" are the externally visible names.
    #
    list(set = privSet, 
         get = privGet, 
         setInverse = privSetInverse,
         getInverse = privGetInverse)
}

## 
## cacheSolve - return the inverse of a matrix stored in a makeCacheMatrix object
##
## Syntax
##     cacheSolve(matrixObject, ...)
##
## Arguments
##     matrixObject - created by a call to makeCacheMatrix() defined above.
##     "..." - additional arguments to pass to solve() if the solution is not cached.
##
## Return Value
##     An R matrix that is the inverse of the defining matrix of the matrixObject.
##     On the first such call to a newly created makeCacheMatrix object, it will
##     use the solve() R library function to compute the inverse matrix and then
##     store it in the matrix object using the setInverse() method.
## 
## Example
##     x <- matrix(rnorm(256), 16, 16)
##     xObj <- makeCacheMatrix(x)
##     xInv <- cacheSolve(xObj)
##     eye <- xInv %*% x          # Should give the identity matrix within epsilon
##     xInv2 <- cacheSolve(xObj)  # Returns the cached value
##     xInv == xInv2              # Should be all TRUE
## 

cacheSolve <- function(x, ...) {
    #
    # Check whether the object already has a cached inverse value
    # and return it, if so.
    #
    xInverse <- x$getInverse()
    if(!is.null(xInverse)) {
        message("Returning cached value for Inverse")
        return(xInverse)
    }
    #
    # The inverse is not yet cached, so compute it and then cache the result.
    # Use the get() method of the underlying object to retrieve the original matrix.
    #
    data <- x$get()
    newInverse <- solve(data, ...)
    x$setInverse(newInverse)

    # The newly computed inverse matrix is returned
    return(newInverse)
}
