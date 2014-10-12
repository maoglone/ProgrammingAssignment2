## First function creates the cache matrix, second returns its inverse.

## This function caches the matrix.

makeCacheMatrix <- function(x = matrix()) { m<-NULL
                                            set<-function(y){
                                              x<<-y
                                              m<<-NULL
                                            }
                                            get<-function() x
                                            setmatrix<-function(solve) m<<- solve
                                            getmatrix<-function() m
                                            list(set=set, get=get,
                                                 setmatrix=setmatrix,
                                                 getmatrix=getmatrix)

}


## This returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
