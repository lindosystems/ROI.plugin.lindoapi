## Example of using the std callback function
## @param sModel The model object
## @param iLoc The location
## @param sData User data
cbFunc <- function(sModel,iLoc,sData=NA) {
  retval <- 0
  iter <- rLSgetProgressIInfo(sModel,iLoc,LS_IINFO_CUR_ITER)
  dobj <- rLSgetProgressDInfo(sModel,iLoc,LS_DINFO_CUR_OBJ)
  sout <- sprintf("iter:%d, obj:%g (loc:%d)",as.integer(iter$pvValue),as.double(dobj$pvValue),as.integer(iLoc))
  print(sout)
  as.integer(retval)
}

## Example of log callback
## @param sModel The model object
## @param sLine The log message
## @param sData User data
logFunc <- function(sModel,sLine,sData=NA) {
  print(sLine)
}

# Black-box functions (helpers)
g1 <- function(X,Y){ return (exp( -`^`(X  ,2)  - `^`(Y+1,2) ))  }   
g2 <- function(X,Y){ return (exp( -`^`(X  ,2)  - `^`(Y  ,2) ))  }
g3 <- function(X,Y){ return (exp( -`^`(X+1,2)  - `^`(Y  ,2) ))  }
f1 <- function(X,Y){ return (`^`(1-X,2)                      )  }  
f2 <- function(X,Y){ return (X/5 - `^`(X  ,3)  - `^`(Y  ,5)  )  }


# Main Black-box function
# @param sModel The model object
# @param sData User data
# @param nRow The row index
# @param padPrimal The decision vector
# @param nJDiff The number of differing variables since last call
# @param dXJBase The base value of the differing variables
# Main Black-box function
funcalc8 <- function(sModel,sData,nRow,padPrimal,nJDiff, dXJBase) {
  dFuncVal <- 0.    
  # local references for current point
  X = padPrimal[1]
  Y = padPrimal[2]
 
  # compute objective's functional value*/
  if (nRow==-1) {        
    dFuncVal = (3*f1(X,Y)*g1(X,Y)) - (10*f2(X,Y)*g2(X,Y)) - (g3(X,Y))/3  

  # compute constaint 0's functional value */
  } else if (nRow==0) {
    dFuncVal = (X*X) + Y - 6.0

  # compute constaint 1's functional value */
  } else if (nRow==1) {
    dFuncVal = X + (Y*Y) - 6.0
    
  } else {
    # cannot happen    
  }  

  # display user data
  #dput(sData)
  
  #return it
  dFuncVal
}
