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

### Callback function to act on rEnv and rModel in 'rLindo' style before the optimization starts
## @param rEnv LINDO enviroment object
## @param rModel LINDO model object
## @param control A list of control parameters.
## @remark This function is called right before the optimization starts, this means
## no other changes are made to the model after this function is called.
on_before_optimize <- function(rEnv, rModel, control)
{
    if ( is.null(rEnv) || is.null(rModel) ) return(invisible(NULL))

    if (!is.null(control$verbose) && control$verbose) {
        cat(">>> on_before_optimize acting on rEnv and rModel\n")
    }    
    ###############################
    ## Insert your code here
    ###############################
    
    # Initialize as an empty list to return results from callback before optimization (cbbo)
    cbbo <<- list()  

    ## e.g. Display model stats
    numVars <- rLSgetIInfo(rModel,LS_IINFO_NUM_VARS)[2]$pnResult
    numCont <- rLSgetIInfo(rModel,LS_IINFO_NUM_CONT)[2]$pnResult
    modelType <- rLSgetIInfo(rModel,LS_IINFO_MODEL_TYPE)[2]$pnResult
    if (!is.null(control$verbose) && control$verbose) {
        cat(sprintf(">>> Model has %d variables, %d continuous variables and has a model-id '%d'\n", numVars, numCont, modelType))
    }

    ## e.g. write an MPS file
    if (0>1) {
        filename <- "on_before_test.mps"
        nErr = rLSwriteMPSFile(rModel, filename, LS_UNFORMATTED_MPS)$ErrorCode    
        if (!is.null(control$verbose) && control$verbose) {    
            if (nErr==0) {
                cat(">>> Model written to file: ", filename, "\n")
            } else {
                cat(">>> Error writing model to file: ", filename, "\n")
            }
        }
    }

    ## e.g. display the parameters which are set to non-default values right before the optimization starts
    if (2>1) {
        cat("\n>>> Displaying non-default parameters\n")
        cnt <- ROI_registered_solver_control(solver)
        for (i in 1:nrow(cnt)) {
            par_key <- as.character(cnt$control[i])
            r <- rLSgetParamMacroID(rEnv, par_key)
            if (r$ErrorCode != 0 && grepl("LS_", par_key)) {
                cat(sprintf(">>> Error getting parameter id for %s\n", par_key))
                next
            } else {
                par_id = r$pnParam
                if (grepl("LS_IPARAM", par_key)) {
                    par_e <- rLSgetEnvIntParameter(rEnv, par_id)
                    par_m <- rLSgetModelIntParameter(rModel, par_id)
                    if (!is.null(par_e$pnValue) && !is.na(par_e$pnValue) && 
                        !is.null(par_m$pnValue) && !is.na(par_m$pnValue) && 
                        par_m$pnValue != par_e$pnValue) {
                        cat(sprintf(">>> %s = %d (default = %d)\n", par_key, par_m$pnValue, par_e$pnValue))
                    }
                } else if (grepl("LS_DPARAM", par_key)) {
                    par_e <- rLSgetEnvDouParameter(rEnv, par_id)
                    par_m <- rLSgetModelDouParameter(rModel, par_id)
                    if (!is.null(par_e$pdValue) && !is.na(par_e$pdValue) && 
                        !is.null(par_m$pdValue) && !is.na(par_m$pdValue) && 
                        par_m$pdValue != par_e$pdValue) {
                        cat(sprintf(">>> %s = %g (default = %g)\n", par_key, par_m$pdValue, par_e$pdValue))
                    }
                }
            }
        }        
    }
    cbbo$numVars <<- numVars
    cbbo$numCont <<- numCont
    cbbo$modelType <<- modelType

    return(cbbo)
}

### Callback function to act on rEnv and rModel in 'rLindo' style after the optimization ends
## @param rEnv LINDO enviroment object
## @param rModel LINDO model object
## @param control A list of control parameters.
## @param result A list of results.
### @remark This function is called right after the optimization ends, this means
## no other changes are made to the model before this function is called.
on_after_optimize <- function(rEnv, rModel, control, result)
{
    if ( is.null(rEnv) || is.null(rModel) ) return(invisible(NULL))

    if (!is.null(control$verbose) && control$verbose) {
        cat(">>> on_after_optimize acting on rEnv and rModel\n")
    }    
    ###############################
    ## Insert your code here
    ###############################
    # Initialize as an empty list to return results from callback after optimization (cbao)
    cbao <<- list()      

    if (result$status == LS_STATUS_INFEASIBLE) {
        cat(">>> Model is infeasible\n")
        res <- find_iis(rModel)
    } else if (result$status == LS_STATUS_UNBOUNDED) {
        cat(">>> Model is unbounded\n")
    } else if (result$status == LS_STATUS_OPTIMAL) {
        cat(">>> Model is optimal\n")
        ## e.g. write a solution file
        solfile <- "on_after_test.sol"
        nErr = rLSwriteSolution(rModel, solfile)$ErrorCode    
        if (!is.null(control$verbose) && control$verbose) {
            if (nErr==0) {
                cat(">>> Solution written to file: ", solfile, "\n")
            } else {
                cat(">>> Error writing solution to file: ", solfile, "\n")
            }        
        }
	  #Get solution information into a global container 
	  cbao$status <<- rLSgetIInfo(rModel,LS_IINFO_MIP_STATUS) # integer type'd info
	  cbao$obj <<- rLSgetDInfo(rModel,LS_DINFO_MIP_OBJ) # double type'd info
	  cbao$x <<- rLSgetMIPPrimalSolution(rModel)
    } else if (result$status == LS_STATUS_FEASIBLE) {
        cat(">>> Model is feasible\n")
    } else {
        cat(">>> Model is not solved\n")
    }

    return(cbao)
}
