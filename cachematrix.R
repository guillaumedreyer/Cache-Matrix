makeCacheMatrix <- function(x = matrix()){
        ## Creates a special "matrix", which in fact is a list. 
        ## If A <- makeCacheMatrix(x = matrix(...)), each entry of 
        ## the list 'A' is a function. For instance, A$get() 
        ## returns the actual value of the initial matrix 'x'.

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function(){
                x
        }
        
        setinverse <- function(inverse0{
                m <<- inverse
        }
        
        getinverse <- function(){
                m
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
        ## Computes the inverse of a special "matrix" created using "makeCacheMatrix".
        ## 'cacheSolve' is a program designed to make use of the list of function brought by 
        ## 'makeCacheMatrix(x = matrix(...))'. 'cacheSolve' uses these functions 
        ## cleverly, keeping track of whether the inverse of 'x' has been already computed
        ## or not. When called the first time, it computes the inverse of 'x' and uses 'makeCacheMatrix(x)' 
        ## to store the value of the inverse. When called a second time, it skips 
        ## the computation and directly returns the value stored in 'makeCacheMatrix(x)' to avoid 
        ## unnecessary recalculations.
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


