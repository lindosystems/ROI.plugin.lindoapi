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

### Load LP
## @param rEnv LINDO enviroment object
## @param rModel LINDO model object
## @param x An object of class "OP" representing the optimization problem.
lindoapi_load_lp <- function(x, rEnv, rModel) {    
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
    return (nErr)
}

### Solve LP
## @param x An object of class "OP" representing the optimization problem.
## @param control A list of control parameters.
solve_LP <- function(x, control = list()) {
    #Create LINDO enviroment object
    rEnv <- rLScreateEnv()
    #Create LINDO model object
    rModel <- rLScreateModel(rEnv)

    nErr <- lindoapi_load_lp(x, rEnv, rModel)

    sol <- lindoapi_solve_model(rEnv, rModel, control)
    # str(sol)

    rLSdeleteModel(rModel)
    #Delete the environment
    rLSdeleteEnv(rEnv)
    
    objval <- tryCatch(objective(x)(sol$x), error = function(e) as.numeric(NA))
    ROI_plugin_canonicalize_solution( solution = sol$x, optimum = objval,
        status = sol$status, solver = "lindoapi", message = sol )
}

### Read the 'reorder_constraints' control.
## Defaults to TRUE: a model whose Q-constraints do not already occupy the tail
## of the constraint list is reordered internally rather than rejected.  Set it
## to FALSE to get the strict ordering check instead.
## @param control A list of control parameters.
use_reorder_constraints <- function(control) {
    value <- control$reorder_constraints
    if ( is.null(value) || length(value) != 1L || is.na(value) ) return(TRUE)
    isTRUE(as.logical(value))
}

### Map per-row solver output back to the ROI constraint order.
## The model handed to LINDO orders its rows [all L-constraints] followed by
## [all Q-constraints]; row_perm[r] is the ROI constraint that became LINDO row
## r.  Anything indexed by row -- duals and slacks -- has to be inverted before
## it is returned, or it is reported against the wrong constraints.  Primal
## values and reduced costs are indexed by variable and are left alone.
## @param sol A solution list as returned by lindoapi_solve_model.
## @param row_perm The row permutation, or NULL if the model has no Q-constraints.
unpermute_rows <- function(sol, row_perm) {
    if ( is.null(row_perm) ) return(sol)
    if ( identical(row_perm, seq_along(row_perm)) ) return(sol)
    for ( nm in c("pi", "slack") ) {
        v <- sol[[nm]]
        if ( is.null(v) || length(v) != length(row_perm) ) next
        v[row_perm] <- sol[[nm]]
        sol[[nm]] <- v
    }
    return(sol)
}

### Load QP
## @param rEnv LINDO enviroment object
## @param rModel LINDO model object
## @param x An object of class "OP" representing the optimization problem.
## @param control A list of control parameters.
## @return The last LINDO error code, carrying the LINDO-row-to-ROI-constraint
##         permutation in its "row_perm" attribute when the model has
##         Q-constraints.
lindoapi_load_qp <- function(x, rEnv, rModel, control = list()) {
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
    row_perm <- NULL
    if ( is_qcon ) { # q-constraints
        QL <- terms(constraints(x))$Q
        is_lconstr <- sapply(QL, is_zero_matrix)

        ## The model handed to LINDO always ends up ordered [all L-constraints]
        ## followed by [all Q-constraints]: rLSloadLPData() below takes the
        ## L-rows in their original relative order, then each Q-row is appended
        ## by rLSaddConstraints().  row_perm records which ROI constraint each
        ## LINDO row came from, and is the identity whenever the caller had
        ## already ordered the constraints that way.
        row_perm <- unname(c(which(is_lconstr), which(!is_lconstr)))
        reorder <- use_reorder_constraints(control)

        ## Say so when a reorder actually happens.  The model the solver sees is
        ## then not row-for-row the model that was supplied, and the per-row
        ## results are mapped back on the way out, so neither should be silent.
        ## A model already in the required order permutes to the identity and
        ## says nothing.  message() rather than warning(): reordering is correct,
        ## documented, default behaviour, and a solve loop would otherwise
        ## collapse into "There were 50 or more warnings".
        if ( reorder && !identical(row_perm, seq_along(row_perm)) ) {
            message("NOTE: L-constraints do not all precede the Q-constraints.")
            message(sprintf("NOTE: constraints reordered for LINDO as (%s).",
                            paste(row_perm, collapse = ", ")))
            message("NOTE: duals and slacks are mapped back to the ROI order on return.")
            message("NOTE: set control 'reorder_constraints' to FALSE to reject instead.")
        }

        ## With reordering disabled the caller is responsible for the ordering,
        ## so check it.  Anchor on the FIRST Q-constraint: any L-constraint at
        ## or after it breaks the requirement.  Anchoring on the LAST one is
        ## wrong twice over -- it misses an L-constraint sandwiched between two
        ## Q-constraints, and it indexes one past the end whenever the final
        ## constraint is quadratic, which is precisely the required layout.
        if ( !reorder && any(!is_lconstr) ) {
            first_qconstr_idx <- min(which(!is_lconstr))
            lconstr_idx <- which(is_lconstr)
            trailing_lconstr <- lconstr_idx[lconstr_idx > first_qconstr_idx]
            if (length(trailing_lconstr) > 0L) {
                message("CRITICAL: Linear constraints found after quadratic constraints.")
                message("CRITICAL: L-constraints must precede all Q-constraints.")
                message(sprintf("CRITICAL: first Q-constraint is at index %d.",
                                first_qconstr_idx))
                message(sprintf("CRITICAL: L-constraint(s) at index %s follow it.",
                                paste(trailing_lconstr, collapse = ", ")))
                message("Set control 'reorder_constraints' to TRUE to reorder them here.")
                message("Halting the optimization process.")
                stop("lindoapi: L-constraints must precede all Q-constraints.")
            }
        }

        mat <- make_csc_matrix(constraints(x)$L[is_lconstr,])
        as.matrix(constraints(x)$L[is_lconstr,])
        xsense <- map_sense(x)
        xrhs <- constraints(x)$rhs
        nLinRows <- sum(is_lconstr)
        nRows <- nLinRows
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
        qconstr_idx <- which(!is_lconstr)
        for ( k in seq_along(qconstr_idx) ) {
            i <- qconstr_idx[k]
            QLi <- QL[[i]]
            lc <- constraints(x)$L[i,]
            paiRows <- c(0L, length(lc$j))
            nErr = rLSaddConstraints(rModel, 1L, xsense[i], NULL, paiRows, lc$v, lc$j - 1L, xrhs[i])$ErrorCode
            CHECK_ERR(rEnv, nErr, STOP=TRUE)
            ## Target the row this constraint was actually appended to.  The
            ## model already holds nLinRows rows, so the k-th appended Q-row
            ## sits at 0-based index nLinRows + k - 1.  Addressing it by the ROI
            ## constraint index (i-1L) instead only agrees with that when the
            ## Q-constraints already occupy the tail of the constraint list.
            qrow <- nLinRows + k - 1L
            nErr = rLSaddQCterms(rModel, length(QLi$i), rep.int(qrow,length(QLi$i)), QLi$i - 1L, QLi$j - 1L, as.numeric(QLi$v))$ErrorCode
            CHECK_ERR(rEnv, nErr, STOP=TRUE)
        }
    }

    if ( is_mixed_intger(x) ) {
       nErr = rLSloadVarType(rModel, paste(types(x), collapse = ""))$ErrorCode
       CHECK_ERR(rEnv, nErr, STOP = TRUE)
    }
    if ( !is.null(row_perm) ) attr(nErr, "row_perm") <- row_perm
    return (nErr)
}

### Load LP or QP
## @param rEnv LINDO enviroment object
## @param rModel LINDO model object
## @param x An object of class "OP" representing the optimization problem.
lindoapi_load <- function(x, rEnv, rModel, control = list()) {
    if ( any(OP_signature(x)[1:2] == "Q") ) {
        lindoapi_load_qp(x, rEnv, rModel, control)
    } else {
        lindoapi_load_lp(x, rEnv, rModel)
    }
}

### Solve QP
## @param x An object of class "OP" representing the optimization problem.
## @param control A list of control parameters.
solve_QP <- function(x, control = list()) {
    #Create LINDO enviroment object
    rEnv <- rLScreateEnv()
    #Create LINDO model object
    rModel <- rLScreateModel(rEnv)
    
    nErr <- lindoapi_load_qp(x, rEnv, rModel, control)
    row_perm <- attr(nErr, "row_perm")

    sol <- lindoapi_solve_model(rEnv, rModel, control)
    ## Duals and slacks come back in LINDO row order; restore ROI's.
    sol <- unpermute_rows(sol, row_perm)
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

