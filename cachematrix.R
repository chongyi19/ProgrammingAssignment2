## Here the functions use the <<- operator to assign a value to an object in an environment that is different from the current environment. 
## The following two functions create a special object that stores a matrix caches its inverse.

## The following function creates a matrix, which is a list containing a function. 
## The function set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
                s<-NULL
                set<-function(y){
                                x<<-y
                                s<<-NULL
                }
                get<-function() x
                setsolve<-function(solve) s<<-solve
                getsolve<-function() s
                list(set=set, get=get,setsolve=setsolve,getsolve=getsolve)
}


## The following function calculates the inverse of the matrix. 
## It first checks if the inverse has already been calcualted and stored in the cache.
## Otherwise, it calculates the inverse of the matrix and set in the cache.

cacheSolve <- function(x, ...) {
                s<-x$getsolve()
                if(!is.null(s)){
                                message("getting cached data")
                                return(s)
                }
                data<-x$get()
                s<-solve(data,...)
                x$setsolve(s)
                s
}
