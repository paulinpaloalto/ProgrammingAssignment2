#
# Implement the example to understand how it works
#

#
# This is effectively the "class definition" if this
# were a bona fide object-oriented language. Where's Bjarne
# when you need him ...
#
makeVector <- function(x = numeric()) {
    #
    # In c++ this would be private data of the class
    #
    m <- NULL
    #
    # When a new vector is set, clear the cached mean
    #
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, 
         get = get, 
         setmean = setmean,
         getmean = getmean)     
}

#
# In a real OOL, this would be a subclass of the
# base class makeVector
#
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("returning cached value for mean")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
