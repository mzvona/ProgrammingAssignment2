
library(digest)


##This function creates a special "matrix" object that can cache its inverse.
cacheSolve <- function(x = matrix, envir= -1) {
      ## Generate the md5 of the input matrix to determine the name of the
      ## variable which should stores it's inverse
      CHKSUM<-digest(x,algo="md5")  
      #If the existing array has previously been solved, return the result
      #which is already stored in the parent environment under the name of the resultant md5
        if(exists(CHKSUM,envir=-1)){ 
          
            #Tell the user it's already been solved, and display the value
            message("Is already here")
            get(CHKSUM, envir= -1)
        #If it hasn't been solved before, solve it by calling makeCacheMatrix display the value 
        } else {
            message("Wasn't here, better solve for it")
            makeCacheMatrix(x)
        }
}
makeCacheMatrix <- function(x = matrix, envir= -1,...) {
      ## Generate the md5 of the parent matrix to name the variable which stores
      ## it's inverse
      CHKSUM<-digest(x,algo="md5")
      ## Return a matrix that is the inverse of 'x'
      ## Per assignment, assuming always invertible
      result<-solve(x)
      ##Assign the inverted matrix value to the variable named
      ##by the MD5 of the parent matrix
      assign(CHKSUM, result, inherits=TRUE)
      ##Display the solution
      get(CHKSUM, envir= -1)

}

