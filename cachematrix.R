 ##makeCacheMatrix creates a matrix that does the following

 ##1.  set the value of the vector using the following commands
              
 ##2.  get the value of the vector using the following command
               
 ##3.  set the value the matrix to the 
 ##        inverse using the following command
             
 ##4.  get the value of the inverse matrix 
 ##         using the following command
              

makeCacheMatrix <- function(inputMatrix = matrix()) {
	#initialize the return inverse matrix to null
	inverseMatrix <- NULL
	
	set <- function(y) {
	        #set input to a y
			inputMatrix <<- y
			#set inverseMatrix to null
			inverseMatrix <<- NULL
	}
	#get the inputMatrix
	get <- function() inputMatrix
	
	#calculate the inverse of the matrix using solve
	setMatrix <- function(solve) inverseMatrix <<- solve
	
	#get the inverseMatrix
	getMatrix <- function() inverseMatrix
	
	#create a list containing the functions to set and get the 
	#value of the matrix, and to get and set the value of 
	#the inverse of the matrix.
	list(set = set, get = get,
		 setMatrix = setMatrix,
		 getMatrix = getMatrix)

}


## the function cacheSolve gets a cached matrix
## and inverts it using the r solve() function.
## A cached matrix is passed into the function as 
## a parameter. 
cacheSolve <- function(x=matrix(), ...) {
           #get the value of the inverse matrix
		   inverseMatrix<-x$getMatrix()
		   
		   #if we have it in cache 
		   #return it.
           if(!is.null(inverseMatrix)) {
              message("getting cached data")
              return(inverseMatrix)
           }
		   #get the inputMatrix
		   data <- x$get()
		   
		   #calculate inverse using solve
           inverseMatrix <- solve(data, ...)
		   
		   #set the matrix to the inverse matrix	   
           x$setMatrix(inverseMatrix)
		   
		   #return the inverse
           inverseMatrix
}
