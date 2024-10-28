## LINDO API Plugin for the R Optimization Infrastructure (ROI)
## Author: atlihan@lindo.com
## Date  : 2024-10-28
## License: MIT
as_dgCMatrix <- function( x, ... ) {
  if (is.null(x)) return(NULL)
  Matrix::sparseMatrix(i=x$i, j=x$j, x=x$v, dims=c(x$nrow, x$ncol))
}

make_csc_matrix <- function(x) UseMethod("make_csc_matrix")

make_csc_matrix.matrix <- function(x) {
    if(!is.matrix(x))33
        stop("Argument 'x' must be a matrix.")
   
    ind <- which(x != 0, arr.ind = TRUE)    
    matbeg <- c(0L, cumsum(tabulate(ind[, 2L], ncol(x))))
    matind <- ind[, 1] - 1L
    list(beg = matbeg, cnt = diff(c(matbeg, length(matind))),
         ind = matind, val = x[ind])
}

make_csc_matrix.simple_triplet_matrix <- function(x) {
    if(!inherits(x, "simple_triplet_matrix"))
        stop("Argument 'x' must be of class 'simple_triplet_matrix'.")

    ## The matrix method assumes that indices for non-zero entries are
    ## in row-major order, but the simple_triplet_matrix() constructor
    ## currently does not canonicalize accordingly ...
    ind <- order(x$j, x$i)
    matbeg <- c(0L, cumsum(tabulate(x$j[ind], x$ncol)))
    matind <- x$i[ind] - 1L
    list(beg = matbeg, cnt = diff(c(matbeg, length(matind))),
         ind = matind, val = x$v[ind])
}

print_triplet_matrix <- function(x) {
    if(!inherits(x, "simple_triplet_matrix"))
        stop("Argument 'x' must be of class 'simple_triplet_matrix'.")
    cat("i: ", x$i, "\n")
    cat("j: ", x$j, "\n")
    cat("v: ", x$v, "\n")
}

to_dense_vector <- function(x, len, default = 0L) {
    y <- rep.int(default, len)
    if ( is.null(x$ind) ) return(y)
    y[x$ind] <- x$val
    return(y)
}

is_lower_unbounded <- function(x) {
    if ( length(bounds(x)$lower$ind) < x$n_of_variables ) return(FALSE)
    all(bounds(x)$lower$ind == -Inf)
}

is_upper_unbounded <- function(x) {
    isTRUE(length(bounds(x)$upper$ind) == 0L)
}

is_diag_matrix <- function(x) {
    all(x$i == x$j)
}

is_zero_matrix <- function(x) {
    stopifnot(inherits(x, "simple_triplet_matrix"))
    length(x$i) == 0
}

is_mixed_intger <- function(x) {
    var_types <- types(x)
    if (length(var_types) == 0L) return(FALSE)
    any(var_types != "C")
}

map_sense <- function(x) {
    sense_map <- setNames(c('L', 'E', 'G'), c('<=', '==', '>='))
    sense_map[constraints(x)$dir]
}

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

### Solve LP
## @param x An object of class "OP" representing the optimization problem.
## @param control A list of control parameters.
solve_LP <- function(x, control = list()) {
    #Create LINDO enviroment object
    rEnv <- rLScreateEnv()
    #Create LINDO model object
    rModel <- rLScreateModel(rEnv)
    
    # Number of columns in the constraint matrix.
    nCols <- x$n_of_variables
    # Number of rows in the constraint matrix.
    nRows  <- x$n_of_constraints 
    # Single integer value that specifies whether the problem is a minimization or maximization problem.
    lpdir <- (-1)^maximum(x)
    objf <- as.vector(terms(objective(x))$L)

    mat <- make_csc_matrix(constraints(x)$L)
    sense <- map_sense(x)

    rhs <- constraints(x)$rhs
        
    bo <- as.data.frame(bounds(x))
    bo$lower[bo$lower == -Inf] <- -LS_INFINITY
    bo$upper[bo$upper ==  Inf] <-  LS_INFINITY
    # Containing the lower bound on each of the variables.
    lb <- bo$lower
    # Containing the lower bound on each of the variables.
    ub <- bo$upper
    nNz = length(mat$val)
    nErr <- rLSloadLPData(rModel, nRows, nCols, lpdir, 0,objf, rhs,  paste(sense, collapse = ""),
        nNz, mat$beg, mat$cnt, mat$val, mat$ind, lb, ub)$ErrorCode
    CHECK_ERR(rEnv, nErr, STOP=TRUE)


    if ( is_mixed_intger(x) ) {
       nErr = rLSloadVarType(rModel, paste(types(x), collapse = ""))$ErrorCode
       CHECK_ERR(rEnv, nErr, STOP = TRUE) 
    }

    sol <- lindoapi_solve_model(rEnv, rModel, control)
    # str(sol)

    rLSdeleteModel(rModel)
    #Delete the environment
    rLSdeleteEnv(rEnv)
    
    objval <- tryCatch(objective(x)(sol$x), error = function(e) as.numeric(NA))
    ROI_plugin_canonicalize_solution( solution = sol$x, optimum = objval,
        status = sol$status, solver = "lindoapi", message = sol )
}

### Solve QP
## @param x An object of class "OP" representing the optimization problem.
## @param control A list of control parameters.
solve_QP <- function(x, control = list()) {
    #Create LINDO enviroment object
    rEnv <- rLScreateEnv()
    #Create LINDO model object
    rModel <- rLScreateModel(rEnv)
    
    # Number of columns in the constraint matrix.
    nCols <- x$n_of_variables
    # Number of rows in the constraint matrix.
    nRows  <- x$n_of_constraints 
    # Single integer value that specifies whether the problem is a minimization or maximization problem.
    lpdir <- (-1)^maximum(x)
    objf <- as.vector(terms(objective(x))$L)
        
    bo <- as.data.frame(bounds(x))
    bo$lower[bo$lower == -Inf] <- -LS_INFINITY
    bo$upper[bo$upper ==  Inf] <-  LS_INFINITY
    # Containing the lower bound on each of the variables.
    lb <- bo$lower
    # Containing the lower bound on each of the variables.
    ub <- bo$upper
    ## constraints
    is_qcon <- isTRUE(inherits(constraints(x), "Q_constraint", TRUE) == 1L)
    if ( is_qcon ) { # q-constraints
        QL <- terms(constraints(x))$Q
        is_lconstr <- sapply(QL, is_zero_matrix)
        mat <- make_csc_matrix(constraints(x)$L[is_lconstr,])
        as.matrix(constraints(x)$L[is_lconstr,])
        xsense <- map_sense(x)
        xrhs <- constraints(x)$rhs
        nRows <- sum(is_lconstr)
        rhs <- xrhs[is_lconstr]
        sense <- xsense[is_lconstr]
    } else {
        mat <- make_csc_matrix(constraints(x)$L)
        sense <- map_sense(x)
        rhs <- constraints(x)$rhs
    }        

    nNz = length(mat$val)
    nErr <- rLSloadLPData(rModel, nRows, nCols, lpdir, 0,objf, rhs,  paste(sense, collapse = ""),
        nNz, mat$beg, mat$cnt, mat$val, mat$ind, lb, ub)$ErrorCode
    CHECK_ERR(rEnv, nErr, STOP=TRUE)

    ## Q-Objective
    Q0 <- terms(objective(x))$Q
   
    if ( !is.null(Q0) ) {
        nErr = rLSaddQCterms(rModel, length(Q0$i), rep.int(-1L,length(Q0$i)), Q0$i-1, Q0$j-1, as.numeric(Q0$v))$ErrorCode
        CHECK_ERR(rEnv, nErr, STOP=TRUE)
    }

    ## Q-Constraint
    if ( is_qcon ) {
        for ( i in which(!is_lconstr) ) {
            QLi <- QL[[i]]
            lc <- constraints(x)$L[i,]
            paiRows <- c(0L, length(lc$j))
            nErr = rLSaddConstraints(rModel, 1L, xsense[i], NULL, paiRows, lc$v, lc$j - 1L, xrhs[i])$ErrorCode
            CHECK_ERR(rEnv, nErr, STOP=TRUE)
            nErr = rLSaddQCterms(rModel, length(QLi$i), rep.int(i-1L,length(QLi$i)), QLi$i - 1L, QLi$j - 1L, as.numeric(QLi$v))$ErrorCode
            CHECK_ERR(rEnv, nErr, STOP=TRUE)
        }
    }
        
    if ( is_mixed_intger(x) ) {
       nErr = rLSloadVarType(rModel, paste(types(x), collapse = ""))$ErrorCode
       CHECK_ERR(rEnv, nErr, STOP = TRUE) 
    }

    sol <- lindoapi_solve_model(rEnv, rModel, control)
    # str(sol)
    
    #rLSwriteMPSFile(rModel, "qp.mps", 0)

    rLSdeleteModel(rModel)
    #Delete the environment
    rLSdeleteEnv(rEnv)
    
    objval <- tryCatch(objective(x)(sol$x), error = function(e) as.numeric(NA))
    ROI_plugin_canonicalize_solution( solution = sol$x, optimum = objval,
        status = sol$status, solver = "lindoapi", message = sol )
}

solve_OP <- function(x, control = list()) {
    if ( any(OP_signature(x)[1:2] == "Q") ) {        
        solve_QP(x, control)
    } else {
        solve_LP(x, control)
    }
}

