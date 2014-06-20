## R Programming Class - Assignment Two

## The following code addresses Method Closures.

## The Code Returns the Inverse of a Square Matrix but more importantly is used to teach the concept of Closures and Environmental Variables in R

## Create a function closure which takes a matrix and the matrix inverse allowing getter and setter accessor methods

# How to use this code to understand Closures
# 1. Closures - An object consisting of function(s) and data held in the scope where it is created
#       - Closures - return functions so the data (variables) set maintain their scope within these functions
#       - Closures - data and methods reside within the object that was created and therefore simulates OOP principles to some degree
# 2. This code is added into the R console using the following command
#       - source("cachematrix.R")
#       - this adds the script to the global environment allowing one to call either of the methods below
# 3. Create a matrix
#       - matrixX <- matrix(c(1:20), nrow = 4, ncol = 5) # creates a 4x5 matrix
# 4. Instantiate the makeCacheMatrix closure and store it in a variable
#       - m <- makeCacheMatrix() # running this command creates the object with all internal variables NULL
#       - m <- makeCacheMatrix(matrixX) #running this command creates the object with x set to the passed in matrix which was created in step 3
# 5. Set matrix to makeCacheMatrix object
#       - m$set(matrixX) # sets x to matrix created in step 3
# 6. Solve for the inverse of a matrix (assuming the matrix is indeed invertable)
#       - using our second method, cacheSolve, you will obtain an inverse of the passed in matrix
#       - i <- cacheSolve(matrixX) #sets i to the inverse of matrixX
# 7. Now that you have the inverse matrix, you need to pass this to the makeCacheMatrix object
#       - m$setinverse(i) #sets (caches) inverse of matrixX
# 8. To obtain the cached matrix, call the getinverse method on makeCacheMatrix
#       - m$getinverse

# what this example has demonstrated is that we can create an object and run multiple methods that use stored data
# in an environment where it was created. These varables are not accessable or modifiable outside of the environment internal 
# to the wrapper object created. This allows us to do advanced data manipulation while keeping our code efficient and understandable.




# makeCacheMatrix - a Method to Store the inverse of a matrix to reduce recalculation of the inverse (a processor intensive task)
# Parameters    - This function takes either nothing (which results in an empty matrix stored in x)
#               - or a Matrix which is stored in x
# 
# Accessor Methods  - get - takes no arguments and returns a function that returns the initial (non-inverted) matrix
#                   - set - takes a matrix as an argument, setting x to this matrix (if no matrix is passed in the initiation of makeCacheMatrix, you must set it using this accessor method)
#                   - setinverse - takes a matrix as an argument (calcuated using solve(matrix)), setting this inverted matrix to the variable mi
#                   - getinverse - takes no arguments and returns the inverted matrix stored in the variable mi
#   Examples for using this method can be found above
    
makeCacheMatrix <- function(x = matrix()) {
    # set inverse matrix store to NULL
    mi <- NULL

    # set initial matrix to variable x
    set <- function(y){
        x <<- y
        mi <<- NULL #clear (or NULL) inveser matrix in closure
    }
    
    # get accessor method for inverse of initial matrix
    get <- function(){
        x
    }
    
    setinverse <- function(inverse){
        mi <<- inverse
    }
    
    getinverse <- function(){
        mi
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a matrix as an argument and returns the inverse of that matrix
# you can also pass additional arguments (designated as ...), all of these are the same arguments allowed in the R solve() method
# allowable additional arguments (...) can be found using ?solve in the console

cacheSolve <- function(x, ...) {
    solve(x, ...)
}
