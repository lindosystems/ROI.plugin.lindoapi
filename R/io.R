library(rLindo)

### Solve a LINDO API model object with specified options
## @param rModel LINDO API model object
## @param time_limit numeric time limit in seconds
## @param use_gop logical use global optimization 
lindoapi_solve_model <- function(rEnv, rModel, control = list()) {
    numVars <- rLSgetIInfo(rModel,LS_IINFO_NUM_VARS)[2]$pnResult
    numCont <- rLSgetIInfo(rModel,LS_IINFO_NUM_CONT)[2]$pnResult
    modelType <- rLSgetIInfo(rModel,LS_IINFO_MODEL_TYPE)[2]$pnResult

    use_gop <- control$use_gop
    time_limit <- control$time_limit
    method <- control$method
    verbose <- control$verbose


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

    for (i in seq_along(control)) {
        id <- 0
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
        r$pi = rLSgetDualSolution(rModel)$padDual
        r$status <- rLSgetIInfo(rModel,LS_IINFO_MODEL_STATUS)[2]$pnResult 
        r$objval <- rLSgetDInfo(rModel,LS_DINFO_POBJ)[2]$pdResult
        r$lpstat <- r$status
        r$dj = rLSgetReducedCosts(rModel)$padReducedCost
        r$slack = rLSgetSlacks(rModel)$padSlack
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
        r$slack = rLSgetMIPSlacks(rModel)$padSlack        
    }

    c(r, info = nfo)
}

### Read a model from a file into a LINDO API model object
## @param rModel LINDO API model object just initialized
## @param file character file name
lindoapi_read_file <- function(rModel, file) {
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
    return(r)
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

    r <- lindoapi_read_file(rModel, file)

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

### Convert a LINDO API QMATRIX to an ROI Q-constraint
## @param x LINDO API QMATRIX object
## @param nobj number of variables
lindoapi_to_Q_constraint <- function(x, nobj) {
    Q <- simple_triplet_matrix(i=x$paiQCcols1+1L, j=x$paiQCcols2+1L, v=x$padQCcoef, nrow=nobj, ncol=nobj)
    L <- numeric(nobj)
    L[x$linind + 1L] <- x$linval
    Q_constraint(Q=Q, L=L, dir=map_dir(x$sense), rhs=x$rhs)
}

### Convert a LINDO API matrix to a simple triplet matrix
## @param A LINDO API object returned by `rLSgetLPData`
## @param nrow number of rows
## @param ncol number of columns
## @return a simple triplet matrix
## @remarks This function renames native LINDO API matrix names to those
##  used by ROI
lindoapi_matrix_to_simple_triplet_matrix <- function(A, nrow, ncol) {
    if ( is.null(A) ) return(A)
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
    A$matcnt <- diff(c(A$matbeg, length(A$matind)))
    get_column <- function(j, A) {  
        A$matind[seq(from=A$matbeg[j]+1L, length.out=A$matcnt[j])]
    }
    i <- 1L + unlist(lapply(seq_len(ncol), get_column, A=A))
    j <- unlist(mapply(rep.int, seq_along(A$matcnt), A$matcnt, SIMPLIFY=FALSE))
    simple_triplet_matrix(i = i, j = j, v = A$matval, nrow = nrow, ncol = ncol)
}

### Convert direction to `solver` direction
## @param x character vector of directions
map_dir <- function(x) {
    dir_map <- setNames(c('<=', '==', '>='), c('L', 'E', 'G'))
    stopifnot( all(unique(x) %in% names(dir_map)) )
    dir_map[x]
}

### Convert a LINDO API model to an ROI model
## @param rModel LINDO API model object
## @return an ROI model
lindoapi_to_roi <- function(rModel) {
    problem_name <- "LINDO_MODEL"

    nobj <- rLSgetIInfo(rModel,LS_IINFO_NUM_VARS)[2]$pnResult
    A.nrow  <- rLSgetIInfo(rModel,LS_IINFO_NUM_CONT)[2]$pnResult   

    obj.L <- rLSgetObjective(rModel)$pdObj
    Q0 <- rLSgetQCDatai(rModel,-1L)

    obj.Q <- simple_triplet_matrix(i = Q0$paiQCcols1, j = Q0$paiQCcols2, v = Q0$padQCcoef, nrow = ncol, ncol = ncol)
    obj.names <- NULL

    if ( is.null(obj.Q) ) {
        obj <- L_objective(obj.L, names=obj.name)
    } else {
        obj <- Q_objective(obj.Q, obj.L, names=obj.names)
    }    

    dir_map <- setNames(c('<=', '==', '>='), c('L', 'E', 'G'))
    if (A.nrow) {
        m <- rLSgetLPData(rModel)
        con.L <- lindoapi_matrix_to_simple_triplet_matrix(, A.nrow, nobj)
        con.L.dir <- map_dir(m$pachConTypes)
        con.L.rhs <- m$padB
        con.L.names <- NULL
        if (con.L.names) {
            rownames(con.L) <- con.L.names
        }
        con.L <- L_constraint(con.L, con.L.dir, con.L.rhs)
    } else {
        con.L <- NO_constraint(nobj)
        m <- NULL
    }
    Q0 <- rLSgetQCData(rModel)
    rowidx <- unique(Q0$paiQCrows)
    nqconstrs <- rLSgetIInfo(rModel, LS_IINFO_NUM_QCP_CONS)[2]$pnResult
    if ( is.null(obj.Q) ) {
        nqconstrs <- nqconstrs - 1
    }
    if ( nqconstrs ) {
        con.Q <- vector("list", nqconstrs)
        for (k in seq_along(con.Q)) {
            i = rowidx[k]
            if (i>0) {
                Q_i <- rLSgetQCDatai(rModel,i)
                con.Q[[k]] <- lindoapi_to_Q_constraint(Q_i, nobj)
            }
        }
        con.Q <- do.call(c, con.Q)
    } else {
        con.Q <- NO_constraint(nobj)
    }

    ## FIXME
    if ( is.NO_constraint(con.L) & is.NO_constraint(con.Q) ) {
        con <- NULL
    } else if ( is.NO_constraint(con.L) ) {
        con <- con.Q
    } else if ( is.NO_constraint(con.Q) ) {
        con <- con.L
    } else {
        con <- c(con.L, con.Q)
    }

    typ <-  rLSgetVarType(rModel)$pachVarTypes
    if ( is.null(typ) ) {
        typ <- rep("C", nobj)
    }
    if ( is.null(m) ) {
        lb <- NULL
        ub <- NULL
    } else {
        lb <- m$padL
        ub <- m$padU
    }

    bou <- V_bound(li=seq_along(lb), ui=seq_along(ub), lb=lb, ub=ub, nobj=nobj)

    ## -1 maximize  and 1 minimize
    maximum <- c(TRUE, NA, FALSE)[m$pnObjSense + 2L]

    OP(objective=obj, constraints=con, types=typ, bounds=bou, maximum = maximum)
}

## NOTE: lindoapiAPI writes directly to stout therefore we can not capture the error 
##       messages
lindoapi_read_op <- function(fname) {
    #Create LINDO enviroment object
    rEnv <- rLScreateEnv()
    #Create LINDO model object
    rModel <- rLScreateModel(rEnv)

    r <- lindoapi_read_file(rModel, fname)

    roi_op <- lindoapi_to_roi(rModel)

    #Delete the model and environment
    rLSdeleteModel(rModel)
    #Delete the environment
    rLSdeleteEnv(rEnv)
    
    return(roi_op)
}
