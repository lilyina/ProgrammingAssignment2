makeCacheMatrix<-function(x = matrix()) {
  invM<-NULL
  list (
    set = function(y) {
    x<<-y
    invM <<- NULL
  },
  Get = function () { x },
  SetInverse = function(i) { invM <<- i},
  GetInverse = function() {invM})
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$GetInverse()
  if (!is.null(invM)) {
      return(invM)
  }
  m <- x$Get()
  invM <- solve(m, ...)
  x$SetInverse(invM)
  invM
}
