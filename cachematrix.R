##This function will calculate the inverse for a given matrix. If this matrix had
##been given before, which means the inverse had been calculated, then it will not
##be calculated again. The inverse will be found directly from the old values saved.
##"makeCacheMatrix" will check whether the  matrix had been given before. If yes, 
##retrieve old calcualted m. Otherwise, set m equal to NULL.

makeCacheMatrix<-function(x=matrix()){
         m<-NULL
         set <- function(y){
                x<<-y
                m<<-NULL
         }
         get<-function() x
         setinverse<-function(solve) m<<-solve
         getinverse<-function() m
         list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##"cachesolve" is to calcualte the inverse but depending on whether the given matrix is 
##new or old, the inverse will be directly retrieved from the already saved values or
##calculated.

cachesolve <- function(x, ...) {
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

