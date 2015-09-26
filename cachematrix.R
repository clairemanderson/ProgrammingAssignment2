## Coursera - Johns Hopkins - Data Science Specialization - R Programming
## Week 3 - Programming assignment 2

## 1) "makeCacheMatrix" CacheMatrix builder
## "makeCacheMatrix" builds an environment in which "i" and "x" are stored and 
## associates to it a list of 4 functions (set, get, setinv, getinv). The bundle
## of variables and functions are finally implicitly returned. 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## 2)"cacheSolve" the cached matrix inversion function
## "cacheSolve" expects an object created by the "makeCacheMatrix" function.
## If it is not such an object, it will return an error message saying that
## the $ operator is invalid for atomic vectors.
## If it is such an object, it will first ask whether it has something in its
## cache. 
## If it has nothing in its cache:
## - it checks whether the matrix ("data") is invertible - if it isn't it displays 
## an error message and returns "NULL"; (I realised this step was not required in 
## the assignment *after* I had finished it - it does not hurt to check this so i 
## kept it :-) )
## - then it will compute the inverse and then ask the object to store it in its cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	message("cache not found, computing the inverse")
        data <- x$get()
        if(det(data)==0) {
                message("This matrix is not invertible. Take a blue pill Mr Anderson.")
                return(NULL)
        }
        i <- solve(data)
        x$setinv(i)
        i
}

## Below is an example of the outputs for both functions on two test matrices A and B.
## 
## First we create the matrices:
## A <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), nrow = 3, ncol = 3, byrow = TRUE)
## B <- matrix(c(3, 6, 3, 5, 2, 1, 1, 2, 1), nrow = 3, ncol = 3, byrow = TRUE)
## 
## Let's first create two objects solvable by cacheSolve with the "makeCacheMatrix" function:
## Aprime <- makeCacheMatrix(A)
## Bprime <- makeCacheMatrix(B)
## 
## Now let's solve Aprime. Here's what we get:
## > cacheSolve(Aprime)
## cache not found, computing the inverse
## [,1]        [,2]    [,3]
## [1,]  0.08333333 -0.08333333  0.2500
## [2,] -0.33333333  0.33333333  0.0000
## [3,]  0.22916667  0.02083333 -0.0625
## And now this is done, if we call cacheSolve again, here is what we get
## > cacheSolve(Aprime)
## getting cached data
## [,1]        [,2]    [,3]
## [1,]  0.08333333 -0.08333333  0.2500
## [2,] -0.33333333  0.33333333  0.0000
## [3,]  0.22916667  0.02083333 -0.0625
## 
## And if we try to solve Bprime, here is what we get:
## > cacheSolve(Bprime)
## cache not found, computing the inverse
## This matrix is not invertible. Take a blue pill Mr Anderson.
## NULL
## 
