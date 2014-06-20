## R Programming Class - Assignment Two

## The following code addresses Method Closures.

## The Code Returns the Inverse of a Square Matrix but more importantly is used to teach the concept of Closures and Environmental Variables in R

## Thought Process in Code Construction
## ************************************

## As I worked through this assignment, I agree with many on the forums that it was a challenging one. I do not have an issue with this class pushing us in these type of assignments because in the end that is why I am in enrolled in this class.

## There is something I learned from this assignment that in the end would have made things more difficult for some, but I am passing it along in my approach to this assignment.

## In Object-Oriented Programming (for those in the know, I appreciate the fact that R and closures in R are not object-oriented in the strict sense), one attempts to encapsulate code. Both in the example code for Cached means as well as the approach many have taken for the Matrix assignment, you are required to create an initial object (makeCacheMatrix object) and then pass this to the createMatrix method.

## My approach and thinking for real world issues is to look at it from the following perspective. If I have a matrix and am looking to obtain the inverse of this in an optimized way, I want to call a method to do the heavy lifting without me being aware of it. I do not want to have to create an object (closure) and then manipulate this to extract the original matrix to another method that will either return the cached inverted matrix or calculate and store this for repeated calls for the inverse. 

## With this mindset, I modified the createMatrix method so that it would take either a makeCacheMatrix object or a matrix directly. Inside this function, I check to see if the argument passed to the createMatrix method is of class type list (which is the class type returned from the makeCacheMatrix) or of class type matrix. [Please note this is not a great way to check since other lists could be passed to this method and errors or failure will occur but I did not want to make this absolutely applicable due to time constraints]. 

## If the method determines a matrix was passed to the method, I create the makeCacheMatrix object from within this function and then either see if there is a cached inverse of this matrix or create and store the inverse in this object. If an object (list of makeCacheMatrix) was passed, then I extract the matrix and/or inverse from this object and return the inverse.

## In this way, a user of this code only needs to understand that passing a matrix will return the inverse of this matrix in the most efficient manner, either the cached inverse or the newly created which is then stored for later return when the method is called again.

## Hope you understand my thinking and the use of this code. Further instructions can be found below.


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
    
makeCacheMatrix <- function(initial_matrix = matrix()) {
    # set inverse matrix store to NULL
    inverse_matrix <- NULL

    # set initial matrix to variable x
    set <- function(m){
        initial_matrix <<- m
        inverse_matrix <<- NULL #clear (or NULL) inveser matrix in closure
    }
    
    # get accessor method for inverse of initial matrix
    get <- function(){
        return(initial_matrix)
    }
    
    setinverse <- function(inverse){
        inverse_matrix <<- inverse
    }
    
    getinverse <- function(){
        return(inverse_matrix)
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes an object of type makeCacheMatrix as an argument and returns the inverse of that matrix
# you can also pass additional arguments (designated as ...), all of these are the same arguments allowed in the R solve() method
# allowable additional arguments (...) can be found using ?solve in the console
# returns - either the cached inverse matrix (if it is saved) or calculates the inverse and stores it in the makeCacheMatrix object for later use


cacheSolve <- function(x, ...) {
    #initialize inverse matrix variable as NULL
    matrix_inverse = NULL
    
    # test to see if argument pass to cacheSolve is of class matrix
    if(class(x) == "matrix") {
        makeCacheObject <- makeCacheMatrix()
        makeCacheObject$set(x)
        # once created obtain inverse matrix (if none, returns NULL)
        matrix_inverse <- makeCacheObject$getinverse()
    }
    # if argument passed is not matrix, check to see if it is a list (ie passed argument is already an object of makeCacheMatrix)
    # this is a big assumption and if it is not a list of makeCacheMatrix, code will fail
    if(class(x) == "list"){
        makeCacheObject <- x
        matrix_inverse <- x$getinverse() #collect cached inverse matrix
    }
    
    # test to see if matrix_inverse is not NULL (ie already has a cached inverse matrix)
    if(! is.null(matrix_inverse)){
        message("obtianing cached inverse vector")
        return(matrix_inverse)
    }
    #if no inverse matrix stored, create it from pass in matrix
    else {
        # create inverse matrix using solve method built in R
        matrix_inverse <- solve(makeCacheObject$get(), ...)
        # cache the inverse matrix
        makeCacheObject$setinverse(matrix_inverse)
        
        #return the inverse matrix
        return(matrix_inverse)
    }

    # if there are issues, return NULL from this method
    return(NULL)
}
