#The first function, makeCacheMatrix creates a vector, which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
            x <<- y
             inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
#sample run
# x<-matrix(c(1,2, 4, 3, 1, 5, 7,9,8), nrow=3,ncol = 3)
# m = makeCacheMatrix(x)
# m$get()
#     [,1] [,2] [,3]
#[1,]    1    3    7
#[2,]    2    1    9
#[3,]    4    5    8

#first run of cacheSolve(m)
           # [,1]       [,2]        [,3]
#[1,] -0.56923077  0.1692308  0.30769231
#[2,]  0.30769231 -0.3076923  0.07692308
#[3,]  0.09230769  0.1076923 -0.07692308

#second run of cacheSolve(m)
#getting cached data
            #[,1]       [,2]        [,3]
#[1,] -0.56923077  0.1692308  0.30769231
#[2,]  0.30769231 -0.3076923  0.07692308
#[3,]  0.09230769  0.1076923 -0.07692308


