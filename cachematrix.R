#Basically the functions are identical to the ones supplied in the example. 
#makeCacheMatrix takes as argument a numeric object, here a matrix, and returns a list of 4 functions. 
#With the get() function we can print the matrix, with set(x) we can "override" the existing matrix with a new value, 
# whilst being careful to set the inverse to null (or else the inverse of the initial matrix will remain cached)
# the setinve(inv) and getinv() function are called within the cacheSolve function.

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# The only change we had to make to the example was to set it to calculate the inverse instead of the mean 
# (the variable will be a matrix now, but there is no change needed to the rest of the code)
# The function checks if the inverse of the matrix has been cached. If it has been, then it just returns the mean
# accompanied by a message, if the inverse has not been cached then it calculates it and then caches is via the 
# setinv(inv) function

cacheSolve <- function(x) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

y <- matrix(c(1:3,5,78,4,2,1,7),3,3)
z <- makeCacheMatrix(y)
z$get()
cacheSolve(z)

#The way the 2 functions work is as follows:
# Step 1: create a matrix and evaluate makeCacheMatrix(x) on it. this returns a list of functions.
# Step 2: there are 3 sensible operations that we can do now: 
#              1) print the (cached) matrix via the get() function
#              2) calculate the inverse by evaluating the cachemean() function on the object 
#                     returned by makeCacheMatrix(x)
#              3) input a new matrix either by calling again the makeCacheMatrix on the new matrix
#                 or via the set(x) function. in the latter case the new matrix will be cached and 
#                 the inverse set (cached) to NULL
