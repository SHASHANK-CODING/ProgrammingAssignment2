## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  int <- NULL
  set<-function(y){
                    x<<-y
                    int<<-NULL
                    }

  get<-function()x
  setinv<-function(inverse)int<<-inverse
  getinv<-function() { 
                    inver<-ginv(x)
                    inver%*%x
                    }
  list(set = set,get = get,
       setinv = setinv,
       getinv = getinv)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  {
  inv<-x$getinv()
  if(!is.null(inv)) {
                    message("getting catched data!")
                    return(inv)
  }
  data<-x$get()
  int<-solve(data,...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

