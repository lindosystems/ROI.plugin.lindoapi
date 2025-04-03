library(rLindo)
library(slam)

 CHECK_ERR <- function( rEnv, err, STOP=FALSE ) {
    if ( err != 0 ) {
        if ( STOP ) {
            errmsg <- rLSgetErrorMessage(rEnv,err)$pachMessage
            rLSdeleteEnv(rEnv)
            cat("Deleting the LINDO API environment\n")
            stop( paste("Error ", err, ": ", errmsg) )
        } else {
            warning( paste("Error ", err, ": ", rLSgetErrorMessage(rEnv,err)$pachMessage) )
        }
    }
}

### Solve a LINDO-API model object with specified options
## @param rModel LINDO-API model object
## @param control A list of control parameters.
lindoapi_solve_model <- function(rEnv, rModel, control = list()) {
    numVars <- rLSgetIInfo(rModel,LS_IINFO_NUM_VARS)[2]$pnResult
    numCont <- rLSgetIInfo(rModel,LS_IINFO_NUM_CONT)[2]$pnResult
    modelType <- rLSgetIInfo(rModel,LS_IINFO_MODEL_TYPE)[2]$pnResult
    
    # optional callbacks
    on_after_optimize <- control$on_after_optimize  # After optimization callback
    on_before_optimize <- control$on_before_optimize # Before optimization callback

    # LINDO-API native callbacks
    fn_callback_mip <- control$fn_callback_mip  # MIP callback (every time a new MIP solution is found)
    fn_callback_std <- control$fn_callback_std  # Standard callback
    fn_callback_log <- control$fn_callback_log  # Log callback    
    fn_callback_fox <- control$fn_callback_fox  # F(x), Function (objective and constraints)
    fn_callback_jox <- control$fn_callback_jox  # J(x), Jacobian

    ## Enable R-callback log
    #rLSsetModelLogfunc(rModel,fn_callback_log,new.env())

    ## Enable R-callback 
    #rLSsetCallback(rModel,fn_callback_std,new.env())

    ## Enable R-callback for MIP
    #rLSsetMIPCallback(rModel,fn_callback_mip,new.env())    

    use_gop <- control$use_gop          # Use global optimization
    time_limit <- control$time_limit    # Time limit in seconds, overwrites the model's time limit
    method <- control$method            # Optimization method for continuous models
    verbose <- control$verbose          # Verbosity on ROI.plugin.lindoapi end


    if (is.null(use_gop) || is.na(use_gop)) {
        use_gop <- FALSE
    }

    if (is.null(time_limit) || is.na(time_limit)) {
        time_limit <- Inf
    }

    if (is.null(method) || is.na(method)) {
        method <- LS_METHOD_FREE
    }

    if (is.null(verbose) || is.na(verbose)) {
        verbose <- FALSE
    }

    # Set parameters
    for (i in seq_along(control)) {
        id <- 0
        nErr <- 0
        if (grepl("LS_DPARAM", names(control)[i])) {
            id <- rLSgetParamMacroID(rEnv, names(control)[i])$pnParam
            nErr<-rLSsetModelDouParameter(model=rModel,nParameter=id,dValue=control[[i]])$ErrorCode
        } else if (grepl("LS_IPARAM", names(control)[i])) {
            id <- rLSgetParamMacroID(rEnv, names(control)[i])$pnParam
            nErr<-rLSsetModelIntParameter(model=rModel,nParameter=id,nValue=control[[i]])$ErrorCode
        }
        if (verbose == TRUE && id > 0) {
            if (nErr == 0) {
                cat(names(control)[i], " set to ", control[[i]], "\n")
            } else {
                cat("Error ", nErr, " setting parameter ", names(control)[i], " to ",  control[[i]], "\n")
            }            
        }
    }

    cbbo <- NULL
    # Call the before optimization callback, if defined
    if (!is.null(on_before_optimize) && is.function(on_before_optimize)) {
        cbbo <- on_before_optimize(rEnv, rModel, control)
    }

    nfo = list()
    if (use_gop==TRUE) {
        if ( is.finite(time_limit) ) {
            rLSsetModelDouParameter(model=rModel,nParameter=LS_DPARAM_GOP_TIMLIM,dValue=time_limit) # time limit
        }
        r <- rLSsolveGOP(rModel)
    }
    
    if (numCont == numVars) {
        if (use_gop==FALSE) {
            if ( is.finite(time_limit) ) {
                rLSsetModelDouParameter(model=rModel,nParameter=LS_DPARAM_SOLVER_TIMLMT,dValue=time_limit) # time limit
            }            
            r <- rLSoptimize(rModel,method)
            nfo$method <- rLSgetIInfo(rModel,LS_IINFO_METHOD)[2]$pnResult
            nfo$type <- rLSgetIInfo(rModel,LS_IINFO_BASIC_STATUS)[2]$pnResult
        }
        r$x <- rLSgetPrimalSolution(rModel)$padPrimal
        r$pi <- rLSgetDualSolution(rModel)$padDual
        r$status <- rLSgetIInfo(rModel,LS_IINFO_MODEL_STATUS)[2]$pnResult 
        r$objval <- rLSgetDInfo(rModel,LS_DINFO_POBJ)[2]$pdResult
        r$lpstat <- r$status
        r$dj <- rLSgetReducedCosts(rModel)$padReducedCost
        r$slack <- rLSgetSlacks(rModel)$padSlack
    } 
    else {
        if (use_gop==FALSE)  {
            if ( is.finite(time_limit) ) {
                rLSsetModelDouParameter(model=rModel,nParameter=LS_DPARAM_MIP_TIMLIM,dValue=time_limit) # time limit
            }            
            r <- rLSsolveMIP(rModel)
            r$status <- rLSgetIInfo(rModel,LS_IINFO_MIP_STATUS)[2]$pnResult 
            r$objval <- rLSgetDInfo(rModel,LS_DINFO_MIP_OBJ)[2]$pdResult
            nfo$method <- rLSgetIInfo(rModel,LS_IINFO_METHOD)[2]$pnResult
            nfo$type <- rLSgetIInfo(rModel,LS_IINFO_BASIC_STATUS)[2]$pnResult
        } else {
            r$status <- rLSgetIInfo(rModel,LS_IINFO_GOP_STATUS)[2]$pnResult 
            r$objval <- rLSgetDInfo(rModel,LS_DINFO_GOP_OBJ)[2]$pdResult
            nfo$method <- LS_METHOD_GOP
            nfo$type <- rLSgetIInfo(rModel,LS_IINFO_GOP_STATUS)[2]$pnResult
        }
        r$x <- rLSgetMIPPrimalSolution(rModel)$padPrimal
        r$slack <- rLSgetMIPSlacks(rModel)$padSlack
    }
    
    result <- c(r, info = nfo)

    cbao <- NULL
    # Call the after optimization callback, if defined
    if (!is.null(on_after_optimize) && is.function(on_after_optimize)) {
        cbao <- on_after_optimize(rEnv, rModel, control, result)
    }

    # Copy the callback results to the result object
    if (!is.null(cbao)) {
        result$cbao <- cbao
    }

    if (!is.null(cbbo)) {
        result$cbbo <- cbbo
    }    

    return(result)
    
}

### Read a model from a file into a LINDO-API model object
## @param rModel LINDO-API model object just initialized
## @param file character file name
## @param control A list of control parameters.
lindoapi_read_file <- function(rEnv, rModel, file, control = list()) {
    # Read the model from a file
    r <- rLSreadMPSFile(rModel, file, 0)
    if (r$ErrorCode != LSERR_NO_ERROR) {
        R.utils::printf("Error %d while reading as MPS, trying LINDO format\n", r$ErrorCode)
        r <- rLSreadLINDOFile(rModel, file)
        if (r$ErrorCode != LSERR_NO_ERROR) {
            R.utils::printf("Error %d while reading as LINDO, trying LP format\n", r$ErrorCode)
            r <- rLSreadLPFile(rModel, file)
            if (r$ErrorCode != LSERR_NO_ERROR) {
                R.utils::printf("Error %d while reading as LP, trying MPX format\n", r$ErrorCode)
                r <- rLSreadMPXFile(rModel, file)
                if (r$ErrorCode != LSERR_NO_ERROR) {
                    R.utils::printf("Error %d while reading as MPX, trying MPI format\n", r$ErrorCode)
                    r <- rLSreadMPIFile(rModel, file)
                    if (r$ErrorCode != LSERR_NO_ERROR) {
                        R.utils::printf("Error %d while reading as MPI, trying NL format\n", r$ErrorCode)
                        r <- rLSreadNLFile(rModel, file)
                        if (r$ErrorCode != LSERR_NO_ERROR) {
                            rLSdeleteEnv(rEnv)
                            stop(sprintf("Error %d while reading as NL. Terminating..\n", r$ErrorCode))
                        } #
                    } # end if
                } # end if
            } # end if
        } # end if
    }
    if (!is.null(control$verbose) && control$verbose == TRUE) {
        R.utils::printf("Successfully read the model from file '%s'.\n", file)
    }
    #cat("Pausing for 10 seconds...\n")
    #Sys.sleep(20)
    #cat("Resuming execution.\n")

    return(r)
}

### Write a model to a file from a LINDO-API model object
## @param x LINDO-API model object
## @param rEnv LINDO-API environment object
## @param rModel LINDO-API model object
## @param file character file name
## @param ext optional character, specifying the file format ("mps", "ltx", "mpi", "lp", or "nl").
## @param control A list of control parameters.
lindoapi_write_file <- function(x, rEnv, rModel, file, ext = "", control = list()) {
    
    # Function to detect the file extension after removing compression extensions
    get_extension <- function(filename) {
        # Remove compression extensions if present
        filename <- sub("\\.(gz|zip|7z)$", "", filename)
        # Extract the extension after the last dot
        ext <- tolower(tools::file_ext(filename))
        return(ext)
    }

    # Determine the extension to use for writing
    if (ext == "") {
        ext <- get_extension(file)
    }

    # Map the extension to the correct LINDO-API function
    result <- switch(ext,
        "mps" = rLSwriteMPSFile(rModel, file, LS_UNFORMATTED_MPS),
        "ltx" = rLSwriteLINDOFile(rModel, file),
        "mpi" = rLSwriteMPIFile(rModel, file),
        "lp"  = rLSwriteLPFile(rModel, file),
        "nl"  = rLSwriteNLFile(rModel, file),
        stop("Unsupported file extension: ", ext) # If the extension is unsupported
    )

    # Check for errors
    if (result$ErrorCode != LSERR_NO_ERROR) {
        rLSdeleteEnv(rEnv)
        stop(sprintf("Error %d while writing the file in %s format. Terminating..\n", result$ErrorCode, toupper(ext)))
    }

    R.utils::printf("Successfully wrote the model to file '%s' in %s format.\n", file, toupper(ext))
    return(result)
}

### Solve a model from a file
## @param file character file name
## @param time_limit numeric time limit in seconds
lindoapi_solve_file <- function(file, control = list()) {
    solver <- "lindoapi"
   
    #Create LINDO enviroment object
    rEnv <- rLScreateEnv()
    #Create LINDO model object
    rModel <- rLScreateModel(rEnv)

    r <- lindoapi_read_file(rEnv, rModel, file, control)

    sol <- lindoapi_solve_model(rEnv, rModel, control = control)

    #Delete the model and environment
    rLSdeleteModel(rModel)
    #Delete the environment
    rLSdeleteEnv(rEnv)
    
    status_code <- if ( is.null(sol$status$code) ) sol$status else sol$status$code
    status <- tryCatch(ROI:::canonicalize_status(status_code, solver), error = function(e) as.integer(NA))
    structure(list(solution = sol$x, objval = sol$objval, status = status, message = sol), 
        meta = list(solver = solver), class = c(sprintf("%s_solution", solver), "OP_solution"))
}

### Convert a LINDO-API QMATRIX to an ROI Q-constraint
## @param x LINDO-API QMATRIX object
## @param nobj number of variables
lindoapi_to_Q_constraint <- function(x, nobj) {
    Q <- simple_triplet_matrix(i=x$paiQCcols1+1L, j=x$paiQCcols2+1L, v=x$padQCcoef, nrow=nobj, ncol=nobj)
    L <- numeric(nobj)
    L[x$linind + 1L] <- x$linval
    Q_constraint(Q=Q, L=L, dir=map_dir(x$sense), rhs=x$rhs)
}

### Convert a LINDO-API matrix to a simple triplet matrix
## @param A LINDO-API object returned by `rLSgetLPData`
## @param nrow number of rows
## @param ncol number of columns
## @return a simple triplet matrix
## @remarks This function renames native LINDO-API matrix names to those
##  used by ROI
lindoapi_matrix_to_simple_triplet_matrix <- function(A, nrow, ncol) {
    if (is.null(A)) return(A)

    # Rename fields if necessary
    if (any(grepl("qmat", names(A)))) {
        names(A) <- gsub("qmat", "mat", names(A))
    }
    if (any(grepl("paiAcols", names(A)))) {
        names(A) <- gsub("paiAcols", "matbeg", names(A))
    }
    if (any(grepl("paiArows", names(A)))) {
        names(A) <- gsub("paiArows", "matind", names(A))
    }
    if (any(grepl("padAcoef", names(A)))) {
        names(A) <- gsub("padAcoef", "matval", names(A))
    }

    # Validate A$matbeg and A$matind
    if (is.null(A$matbeg) || is.null(A$matind)) {
        stop("A$matbeg or A$matind is NULL. Check the input data.")
    }

    # Calculate A$matcnt
    A$matcnt <- diff(c(A$matbeg, length(A$matind)))

    # Validate A$matcnt
    if (any(A$matcnt < 0)) {
        stop("A$matcnt contains negative values. Check A$matbeg and A$matind.")
    }

    # Define the get_column function
    get_column <- function(j, A) {
        if (A$matcnt[j] <= 0) {
            return(integer(0))  # Return empty vector for invalid columns
        }
        A$matind[seq(from = A$matbeg[j] + 1L, length.out = A$matcnt[j])]
    }

    # Generate row and column indices
    i <- 1L + unlist(lapply(seq_len(ncol), get_column, A = A))
    j <- unlist(mapply(rep.int, seq_along(A$matcnt), A$matcnt, SIMPLIFY = FALSE))

    # Create and return the simple_triplet_matrix
    simple_triplet_matrix(i = i, j = j, v = A$matval, nrow = nrow, ncol = ncol)
}

### Convert direction to `solver` direction
## @param x character vector of directions
map_dir <- function(x) {
    dir_map <- setNames(c('<=', '==', '>='), c('L', 'E', 'G'))
    stopifnot( all(unique(x) %in% names(dir_map)) )
    dir_map[x]
}

### Convert a LINDO-API model to an ROI model
## @param rModel LINDO-API model object
## @return an ROI model
lindoapi_to_roi <- function(rEnv, rModel, control) {
    problem_name <- "LINDO_MODEL"

	pModel <- rLSgetLPData(rModel)
    #print(pModel)
	if (pModel$ErrorCode != 0) {
        CHECK_ERR(rEnv,pModel$ErrorCode,STOP=TRUE)
    }
	
    nobj <- rLSgetIInfo(rModel,LS_IINFO_NUM_VARS)[2]$pnResult
    ncol <- nobj
    A.nrow  <- rLSgetIInfo(rModel,LS_IINFO_NUM_CONS)[2]$pnResult   

    obj.L <- pModel$padC
    Q0 <- rLSgetQCDatai(rModel,-1L)
    
    obj.Q <- NULL
    if (!is.null(Q0$paiQCcols1) && !is.null(Q0$paiQCcols2) && !is.null(Q0$padQCcoef) && Q0$pnQCnnz > 0) {
        obj.Q <- simple_triplet_matrix(i = Q0$paiQCcols1 + 1, j = Q0$paiQCcols2 + 1, v = Q0$padQCcoef, nrow = ncol, ncol = ncol)
    } else {
        obj.Q <- simple_triplet_zero_matrix(nrow = ncol, ncol = ncol)
    }
    obj.names <- NULL

        
    if ( is.null(obj.Q) ) {
        obj <- L_objective(obj.L, names=obj.names)
    } else {
        obj <- Q_objective(obj.Q, obj.L, names=obj.names)
    } 

	dir_map <- setNames(c('<=', '==', '>='), c('L', 'E', 'G'))
	csense <- unlist(strsplit(pModel$pachConTypes, split = ""))

	nqconstrs <- rLSgetIInfo(rModel, LS_IINFO_NUM_QCP_CONS)[2]$pnResult
    if ( is.null(obj.Q) ) {
        nqconstrs <- nqconstrs - 1
    }
	
    Q0 <- rLSgetQCData(rModel)    		
    qrowidx <- unique(Q0$paiQCrows)	    
	
	if ( nqconstrs==0 ) {
        con.Q <- NO_constraint(nobj) 
		if (A.nrow) {   
			con.L <- lindoapi_matrix_to_simple_triplet_matrix(pModel, A.nrow, nobj)			
			con.L.dir <- map_dir(csense)
			con.L.rhs <- pModel$padB
			con.L.names <- NULL		
			if (!is.null(con.L.names)) {
				rownames(con.L) <- con.L.names
			}		
			con.L <- L_constraint(con.L, con.L.dir, con.L.rhs)
		} else {
			con.L <- NO_constraint(nobj)
			pModel <- NULL
		}		       
	} else {        
        con.Q <- vector("list", A.nrow)
        con.L <- vector("list", A.nrow)
        for (k in seq_along(con.Q)) {
            r <- rLSgetLPConstraintDatai(rModel,k-1)
            L_i <- simple_triplet_matrix(i=rep(1L, length(r$paiVar)), j=r$paiVar + 1L, v=r$padAcoef, nrow=1, ncol=nobj)
            con.L[[k]] <- L_constraint(L_i, dir=map_dir(csense[k]), rhs=pModel$padB[k])            
            Q_i <- rLSgetQCDatai(rModel,k-1)
            if ( Q_i$pnQCnnz > 0 ) {
                Q_i$sense <- csense[k]
                Q_i$rhs <- pModel$padB[k]
                Q_i$linind <- r$paiVar
                Q_i$linval <- r$padAcoef
                con.Q[[k]] <- lindoapi_to_Q_constraint(Q_i, nobj)
            }   			
        }        
        con.Q <- do.call(c, con.Q)
        con.L <- do.call(c, con.L)
    } 
    con <- c(con.L, con.Q)

    typ <-  rLSgetVarType(rModel)$pachVarTypes
    if ( is.null(typ) ) {
        typ <- rep("C", nobj)
    } else {
		typ <- unlist(strsplit(typ, split = ""))
	}
    if ( is.null(pModel) ) {
        lb <- NULL
        ub <- NULL
    } else {
        lb <- pModel$padL
        ub <- pModel$padU
    }

    bou <- V_bound(li=seq_along(lb), ui=seq_along(ub), lb=lb, ub=ub, nobj=nobj)

    ## -1 maximize  and 1 minimize
    maximum <- c(TRUE, NA, FALSE)[pModel$pnObjSense + 2L]

    OP(objective=obj, constraints=con, types=typ, bounds=bou, maximum = maximum)
}

lindoapi_to_roi_dups <- function(rEnv, rModel, control) {
    problem_name <- "LINDO_MODEL"

    pModel <- rLSgetLPData(rModel)
    if (pModel$ErrorCode != 0) {
        CHECK_ERR(rEnv, pModel$ErrorCode, STOP = TRUE)
    }

    nobj <- rLSgetIInfo(rModel, LS_IINFO_NUM_VARS)[2]$pnResult
    ncol <- nobj
    A.nrow <- rLSgetIInfo(rModel, LS_IINFO_NUM_CONS)[2]$pnResult

    obj.L <- pModel$padC
    Q0 <- rLSgetQCDatai(rModel, -1L)
    print(Q0)
    obj.Q <- NULL
    if (!is.null(Q0$paiQCcols1) && !is.null(Q0$paiQCcols2) && !is.null(Q0$padQCcoef) && Q0$pnQCnnz > 0) {
        # Combine i, j, and v into a data frame
        df <- data.frame(i = Q0$paiQCcols1 + 1, j = Q0$paiQCcols2 + 1, v = Q0$padQCcoef)

        # Aggregate duplicate (i, j) pairs by summing their values
        df_agg <- aggregate(v ~ i + j, data = df, FUN = sum)

        # Create the simple_triplet_matrix using the aggregated data
        obj.Q <- simple_triplet_matrix(i = df_agg$i, j = df_agg$j, v = df_agg$v, nrow = ncol, ncol = ncol)
    } else {
        obj.Q <- simple_triplet_zero_matrix(nrow = ncol, ncol = ncol)
    }
    obj.names <- NULL

    if (is.null(obj.Q)) {
        obj <- L_objective(obj.L, names = obj.names)
    } else {
        obj <- Q_objective(obj.Q, obj.L, names = obj.names)
    }

    dir_map <- setNames(c('<=', '==', '>='), c('L', 'E', 'G'))
    csense <- unlist(strsplit(pModel$pachConTypes, split = ""))

    nqconstrs <- rLSgetIInfo(rModel, LS_IINFO_NUM_QCP_CONS)[2]$pnResult
    if (is.null(obj.Q)) {
        nqconstrs <- nqconstrs - 1
    }

    Q0 <- rLSgetQCData(rModel)
    qrowidx <- unique(Q0$paiQCrows)

    if (nqconstrs == 0) {
        con.Q <- NO_constraint(nobj)
        if (A.nrow) {
            con.L <- lindoapi_matrix_to_simple_triplet_matrix(pModel, A.nrow, nobj)
            con.L.dir <- map_dir(csense)
            con.L.rhs <- pModel$padB
            con.L.names <- NULL
            if (!is.null(con.L.names)) {
                rownames(con.L) <- con.L.names
            }
            con.L <- L_constraint(con.L, con.L.dir, con.L.rhs)
        } else {
            con.L <- NO_constraint(nobj)
            pModel <- NULL
        }
    } else {
        stop("Not implemented")
        con.Q <- vector("list", A.nrow)
        con.L <- vector("list", A.nrow)
        for (k in seq_along(con.Q)) {
            r <- rLSgetLPConstraintDatai(rModel, k - 1)
            Q_i <- rLSgetQCDatai(rModel, k - 1)
            if (Q_i$pnQCnnz > 0) {
                Q_i$sense <- csense[k]
                Q_i$rhs <- pModel$padB[k]
                Q_i$linind <- r$paiVar
                Q_i$linval <- r$padAcoef
                con.Q[[k]] <- lindoapi_to_Q_constraint(Q_i, nobj)
            } else {
                L_i <- simple_triplet_matrix(i = rep(1L, length(r$paiVar)), j = r$paiVar + 1L, v = r$padAcoef, nrow = 1, ncol = nobj)
                con.L[[k]] <- L_constraint(L_i, dir = map_dir(csense[k]), rhs = pModel$padB[k])
            }
        }
        con.Q <- do.call(c, con.Q)
        con.L <- do.call(c, con.L)
    }
    con <- c(con.L, con.Q)

    typ <- rLSgetVarType(rModel)$pachVarTypes
    if (is.null(typ)) {
        typ <- rep("C", nobj)
    } else {
        typ <- unlist(strsplit(typ, split = ""))
    }
    if (is.null(pModel)) {
        lb <- NULL
        ub <- NULL
    } else {
        lb <- pModel$padL
        ub <- pModel$padU
    }

    bou <- V_bound(li = seq_along(lb), ui = seq_along(ub), lb = lb, ub = ub, nobj = nobj)

    ## -1 maximize  and 1 minimize
    maximum <- c(TRUE, NA, FALSE)[pModel$pnObjSense + 2L]

    OP(objective = obj, constraints = con, types = typ, bounds = bou, maximum = maximum)
}

### Read a model from a file into LINDO-API then convert to an ROI model
## @param fname character file name
## @remarks **Not tested**
lindoapi_read_op <- function(fname, control = list()) {
    #Create LINDO enviroment object
    rEnv <- rLScreateEnv()
    #Create LINDO model object
    rModel <- rLScreateModel(rEnv)

    # Read the model from a file into LINDO-API
    r <- lindoapi_read_file(rEnv, rModel, fname, control)

    # Convert the LINDO-API model to an ROI model
    roi_op <- lindoapi_to_roi(rEnv, rModel, control)

    #Delete the model and environment
    rLSdeleteModel(rModel)
    #Delete the environment
    rLSdeleteEnv(rEnv)
    
    return(roi_op)
}

### Write an ROI model to a file using LINDO-API
## @param x ROI model
## @param file character file name
## @param ext optional character, specifying the file format ("mps", "ltx", "mpi", "lp", or "nl").
lindoapi_write_op <- function(x, file, ext = "", control = list()) {
    #Create LINDO enviroment object
    rEnv <- rLScreateEnv()
    #Create LINDO model object
    rModel <- rLScreateModel(rEnv)

    # Load the object model x to the LINDO-API
    lindoapi_load(x, rEnv, rModel, control)
    
    # Write the model to a file
    r <- lindoapi_write_file(x, rEnv, rModel, file, ext = ext, control)

    #Delete the model and environment
    rLSdeleteModel(rModel)
    #Delete the environment
    rLSdeleteEnv(rEnv)
    
    return(r)
}