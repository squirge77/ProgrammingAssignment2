#makeCacheMatrix reads in x, a square matrix given by the user. It can have any name 
#like try1<-matrix(data=c(5,6,7,8), nrow=2, ncol=2)
#and try1 will be read in as the argument x
#makeCachematrix does not calculate the inverse
#it just stores it
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
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
#cacheSolve checks if the matrix x was previously read and if it was it 
#retrieves the cached value. 
#if the inverse of x was not previously read
#cacheSolve calculates the inverse
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("fetching cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
